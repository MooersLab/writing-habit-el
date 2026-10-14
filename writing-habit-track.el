;;; writing-habit-track.el --- Record actual writing sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit-el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is Phase 4 of the Emacs Lisp port of the Python writing-habit
;; package.  It ports the track module: `manual.add_session' and
;; `csv_actuals.import_csv'.  It adds one capability the Python version
;; cannot offer, an org-clock harvest, because Emacs already measures
;; writing time.
;;
;; Manual entry.  `writing-habit-track-add' inserts one session, for a
;; quick end-of-day entry.  Give minutes, or both a start and an end time.
;;
;; CSV import.  `writing-habit-track-import-csv' reads the same
;; tracking-template.csv columns the Python importer reads
;; (date, start, end, minutes, project_code, category, note), so a
;; spreadsheet exported from anywhere still loads.  The CSV parser is
;; written here and needs no third-party package, matching the Python
;; "standard library only" promise.
;;
;; Org-clock harvest.  `writing-habit-track-harvest-clock' reads completed
;; CLOCK lines from one or more org files.  A clock is attributed to a
;; project by a [CODE] bracket in its heading, the same bracketed-legend
;; convention the ICS importer uses, and to an activity by a generative,
;; editing, or support tag on the heading.  Harvested rows carry the source
;; 'manual' with a source_ref beginning "orgclock:", which keeps the shared
;; schema unchanged and makes the harvest idempotent: re-running it inserts
;; nothing new.  A first-class 'orgclock' source would be a one-line change
;; to schema.sql that both ports would pick up; the plan tracks that.
;;
;; Public functions:
;;   `writing-habit-track-add'            add one session by hand
;;   `writing-habit-track-import-csv'     import the tracking CSV
;;   `writing-habit-track-harvest-clock'  harvest completed org CLOCK lines
;; plus interactive wrappers over a database file.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'writing-habit-db)
(require 'writing-habit-name)
(require 'writing-habit-lists)

(declare-function org-mode "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-read-date "org" (&rest args))
(declare-function icalendar--read-element "icalendar" (invalue inparams))
(declare-function icalendar--all-events "icalendar" (icalendar))
(declare-function icalendar--get-event-property "icalendar" (event prop))
(declare-function icalendar--decode-isodatetime "icalendar"
                  (isodatetimestring &optional day-shift zone-in zone-out))

(defvar org-inhibit-startup)            ; declared special; set below when harvesting

(defconst writing-habit-track-categories '("generative" "editing" "support")
  "The activity names that a category tag or column may name.")


;;;; Small helpers

(defun writing-habit-track--nonempty (s)
  "Return S trimmed, or nil when it is blank."
  (let ((v (string-trim (or s ""))))
    (unless (string-empty-p v) v)))

(defun writing-habit-track--parse-csv (text)
  "Parse CSV TEXT into a list of rows, each a list of field strings.
Handles double-quoted fields with embedded commas, doubled quotes, and
newlines, which covers Excel and Google Sheets exports."
  (let ((rows '()) (row '()) (field '())
        (i 0) (n (length text)) (in-quote nil) (seen nil))
    (cl-flet ((push-field ()
                (push (apply #'string (nreverse field)) row)
                (setq field '()))
              (push-row ()
                (push (nreverse row) rows)
                (setq row '())))
      (while (< i n)
        (let ((ch (aref text i)))
          (setq seen t)
          (cond
           (in-quote
            (if (eq ch ?\")
                (if (and (< (1+ i) n) (eq (aref text (1+ i)) ?\"))
                    (progn (push ?\" field) (setq i (1+ i)))
                  (setq in-quote nil))
              (push ch field)))
           ((eq ch ?\") (setq in-quote t))
           ((eq ch ?,) (push-field))
           ((or (eq ch ?\n) (eq ch ?\r))
            (when (and (eq ch ?\r) (< (1+ i) n) (eq (aref text (1+ i)) ?\n))
              (setq i (1+ i)))
            (push-field) (push-row) (setq seen nil))
           (t (push ch field))))
        (setq i (1+ i)))
      (when (or field row seen)
        (push-field) (push-row)))
    (nreverse rows)))

(defun writing-habit-track--header-index (header)
  "Return an alist of trimmed column name to position for HEADER."
  (let ((idx '()) (i 0))
    (dolist (h header)
      (push (cons (string-trim h) i) idx)
      (setq i (1+ i)))
    (nreverse idx)))

(defun writing-habit-track--field (row idx key)
  "Return the value in ROW for the column named KEY, using IDX, or nil."
  (let ((pos (cdr (assoc key idx))))
    (and pos (< pos (length row)) (nth pos row))))

(defun writing-habit-track--blank-row-p (row)
  "Return non-nil when ROW is an empty CSV line, matching csv.DictReader.
Only a line with no fields, or one empty field, counts as blank."
  (or (null row)
      (and (= (length row) 1) (string= (car row) ""))))

(defun writing-habit-track--category-from-tags (tags)
  "Return the first activity category named in TAGS, downcased, or nil."
  (let ((found nil))
    (dolist (tg tags found)
      (when (and (not found) (member (downcase tg) writing-habit-track-categories))
        (setq found (downcase tg))))))


;;;; Manual entry (port of manual.add_session)

(cl-defun writing-habit-track-add (db &key day project minutes category start end note)
  "Insert one session into DB and return its new id.
DAY and PROJECT are required.  Give MINUTES, or both START and END, from
which the minutes are computed.  CATEGORY, when given, must be a known
activity name.  The session is stored with the source value manual."
  (unless day (error "A day is required"))
  (unless project (error "A project code is required"))
  (when (and category (string-empty-p (string-trim category)))
    (setq category nil))
  (let ((min minutes))
    (when (null min)
      (if (and start end)
          (setq min (writing-habit-db-minutes-between start end))
        (error "Give minutes, or both start and end")))
    (let ((cat-id (writing-habit-db-get-category-id db category)))
      (when (and category (null cat-id))
        (error "Unknown category %S" category))
      (let ((pid (writing-habit-db-get-or-create-project db project)))
        (sqlite-execute
         db (concat "INSERT INTO session(day, start_time, end_time, actual_min,"
                    " project_id, category_id, source, note)"
                    " VALUES (?, ?, ?, ?, ?, ?, 'manual', ?)")
         (list day start end min pid cat-id note))
        ;; Read the new session id before logging the import, because the
        ;; log write would otherwise become the last insert.
        (let ((sid (caar (sqlite-select db "SELECT last_insert_rowid()"))))
          (writing-habit-db-log-import db "sqlite" "manual add" 1 1 nil "track add")
          sid)))))


;;;; CSV import (port of csv_actuals.import_csv)

(defun writing-habit-track-import-csv (db path)
  "Insert one session per data row of the tracking CSV at PATH into DB.
Columns are date, start, end, minutes, project_code, category, note.
Give either a start and end time, or a minutes value.  Return the number
of sessions inserted."
  (let* ((text (with-temp-buffer (insert-file-contents path) (buffer-string)))
         (all (writing-habit-track--parse-csv text))
         (header (car all))
         (idx (writing-habit-track--header-index header))
         (rows-read 0) (inserted 0) (line-no 1))   ; header is line 1
    (dolist (r (cdr all))
      (unless (writing-habit-track--blank-row-p r)
        (setq line-no (1+ line-no))
        (let ((day (string-trim (or (writing-habit-track--field r idx "date") ""))))
          (unless (string-empty-p day)
            (setq rows-read (1+ rows-read))
            (let* ((start (writing-habit-track--nonempty
                           (writing-habit-track--field r idx "start")))
                   (end (writing-habit-track--nonempty
                         (writing-habit-track--field r idx "end")))
                   (minutes (string-trim
                             (or (writing-habit-track--field r idx "minutes") "")))
                   (code (string-trim
                          (or (writing-habit-track--field r idx "project_code") "")))
                   (category (writing-habit-track--nonempty
                              (writing-habit-track--field r idx "category")))
                   (note (writing-habit-track--nonempty
                          (writing-habit-track--field r idx "note")))
                   (actual nil))
              (when (string-empty-p code)
                (error "%s row %d: project_code is required" path line-no))
              (cond
               ((not (string-empty-p minutes)) (setq actual (string-to-number minutes)))
               ((and start end) (setq actual (writing-habit-db-minutes-between start end)))
               (t (error "%s row %d: give minutes, or both start and end" path line-no)))
              (let ((cat-id (writing-habit-db-get-category-id db category)))
                (when (and category (null cat-id))
                  (error "%s row %d: unknown category %S" path line-no category))
                (let ((pid (writing-habit-db-get-or-create-project db code)))
                  (sqlite-execute
                   db (concat "INSERT INTO session(day, start_time, end_time, actual_min,"
                              " project_id, category_id, source, source_ref, note)"
                              " VALUES (?, ?, ?, ?, ?, ?, 'csv', ?, ?)")
                   (list day start end actual pid cat-id
                         (format "%s:%d" path line-no) note))
                  (setq inserted (1+ inserted)))))))))
    (writing-habit-db-log-import db "csv" path rows-read inserted nil "track import")
    inserted))


;;;; Org-clock harvest (the Emacs-only capture path)

(defconst writing-habit-track--clock-re
  (concat "^[ \t]*CLOCK:[ \t]*"
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^]]* "
          "\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"
          "[ \t]*--[ \t]*"
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^]]* "
          "\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"
          "\\(?:[ \t]*=>[ \t]*\\([0-9]+\\):\\([0-9]\\{2\\}\\)\\)?")
  "Match a completed org CLOCK line, capturing dates, times, and duration.")

(defun writing-habit-track-harvest-clock (db files &optional default-category)
  "Insert one session per completed org CLOCK line found in FILES into DB.
FILES is a file path or a list of file paths.  A clock is attributed to a
project by a [CODE] bracket in its heading, and to an activity by a
generative, editing, or support tag on the heading, or by DEFAULT-CATEGORY.
A clock whose heading carries no project code is skipped.  Harvested rows
use the source value manual with a source_ref beginning \"orgclock:\", so a
second harvest of the same clocks inserts nothing new.  Return the number
inserted."
  (require 'org)
  (let ((files (if (listp files) files (list files)))
        (rows-read 0) (inserted 0))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((org-inhibit-startup t)) (org-mode))
        (goto-char (point-min))
        (while (re-search-forward writing-habit-track--clock-re nil t)
          (setq rows-read (1+ rows-read))
          (let* ((sdate (match-string 1)) (stime (match-string 2))
                 (edate (match-string 3)) (etime (match-string 4))
                 (dh (match-string 5)) (dm (match-string 6))
                 (title "") (tags nil) (code nil) (category nil))
            (save-excursion
              (when (ignore-errors (org-back-to-heading t) t)
                (setq title (or (org-get-heading t t t t) ""))
                (setq tags (org-get-tags))))
            (when (string-match "\\[\\([A-Z][A-Z0-9]\\{0,3\\}\\)\\]" title)
              (setq code (match-string 1 title)))
            (setq category (or (writing-habit-track--category-from-tags tags)
                               default-category))
            (when code
              (let* ((minutes (cond
                               ((and dh dm)
                                (+ (* 60 (string-to-number dh)) (string-to-number dm)))
                               (t (writing-habit-db-minutes-between stime etime))))
                     (ref (format "orgclock:%s:%s %s-%s"
                                  (file-name-nondirectory file) sdate stime etime)))
                (ignore edate)
                (unless (sqlite-select
                         db "SELECT 1 FROM session WHERE source_ref = ? LIMIT 1" (list ref))
                  (let ((cat-id (writing-habit-db-get-category-id db category))
                        (pid (writing-habit-db-get-or-create-project db code)))
                    (sqlite-execute
                     db (concat "INSERT INTO session(day, start_time, end_time, actual_min,"
                                " project_id, category_id, source, source_ref, note)"
                                " VALUES (?, ?, ?, ?, ?, ?, 'manual', ?, ?)")
                     (list sdate stime etime minutes pid cat-id ref title))
                    (setq inserted (1+ inserted))))))))))
    (writing-habit-db-log-import
     db "org"
     (if (= (length files) 1) (car files) (format "%d org files" (length files)))
     rows-read inserted nil "clock harvest")
    inserted))


;;;; ICS import (port of ics_actuals.import_ics)

;; The importer reuses the parsing primitives inside icalendar.el, which is
;; built into Emacs, the same primitives its own reader uses.  Because
;; icalendar.el is bundled, the Elisp ICS import needs no optional package,
;; unlike the Python version, which depends on the third-party icalendar.

(defconst writing-habit-track--code-in-summary
  "\\[\\([A-Z][A-Z0-9]\\{0,3\\}\\)\\]"
  "Match a bracketed legend code in an event summary, such as [A] or [EM].")

(defun writing-habit-track--ics-day (decoded)
  "Return the ISO date string from a DECODED time list."
  (format "%04d-%02d-%02d" (nth 5 decoded) (nth 4 decoded) (nth 3 decoded)))

(defun writing-habit-track--ics-hhmm (decoded)
  "Return the HH:MM string from a DECODED time list."
  (format "%02d:%02d" (nth 2 decoded) (nth 1 decoded)))

(defun writing-habit-track-import-ics (db path)
  "Import actual sessions from the ICS calendar at PATH into DB.
Keep actuals in their own calendar so plan and actual never mix.  Put the
legend code in the event summary in brackets, for example
\"[A] DNPH1 docking\", and put the activity in the CATEGORIES field, for
example \"generative\".  DTSTART and DTEND give the real minutes.  An event
with no bracketed code is skipped.  Return the number of sessions inserted.

Times are read as written in the calendar, so use local floating times, the
form a personal calendar exports for real work blocks.  This importer uses
the icalendar library bundled with Emacs, so it needs no extra package."
  (require 'icalendar)
  (let ((events (with-temp-buffer
                  (insert-file-contents path)
                  (goto-char (point-min))
                  (icalendar--all-events (icalendar--read-element nil nil))))
        (rows-read 0) (inserted 0))
    (dolist (e events)
      (setq rows-read (1+ rows-read))
      (let ((summary (or (icalendar--get-event-property e 'SUMMARY) "")))
        (when (string-match writing-habit-track--code-in-summary summary)
          (let* ((code (match-string 1 summary))
                 (cats (icalendar--get-event-property e 'CATEGORIES))
                 (first-cat (writing-habit-track--nonempty
                             (car (split-string (or cats "") ","))))
                 (category (and first-cat (downcase first-cat)))
                 (uid (or (icalendar--get-event-property e 'UID) ""))
                 (ds (icalendar--decode-isodatetime
                      (icalendar--get-event-property e 'DTSTART)))
                 (de (icalendar--decode-isodatetime
                      (icalendar--get-event-property e 'DTEND)))
                 (day (writing-habit-track--ics-day ds))
                 (start (writing-habit-track--ics-hhmm ds))
                 (end (writing-habit-track--ics-hhmm de))
                 ;; Both endpoints decode with the same zone, so their
                 ;; difference is correct whatever that zone is.
                 (minutes (floor (/ (float-time
                                     (time-subtract (encode-time de) (encode-time ds)))
                                    60)))
                 (cat-id (writing-habit-db-get-category-id db category))
                 (pid (writing-habit-db-get-or-create-project db code)))
            (sqlite-execute
             db (concat "INSERT INTO session(day, start_time, end_time, actual_min,"
                        " project_id, category_id, source, source_ref, note)"
                        " VALUES (?, ?, ?, ?, ?, ?, 'ics', ?, ?)")
             (list day start end minutes pid cat-id uid summary))
            (setq inserted (1+ inserted))))))
    (writing-habit-db-log-import db "ics" path rows-read inserted nil "track import")
    inserted))


;;;; mytime.db sync

;; `writing-habit-track-sync-mytime' ports `track/mytime_sync.py'.  It
;; copies timed sessions from the zTimeSpent table of a mytime.db
;; time-tracking database.  The tracker names a project by its four-digit
;; number, such as 1008 for 1008ToolsForQCrOfRNA, and the weekly table
;; names it by a legend code, such as Z.  The legend of the week a session
;; falls in joins the two.  Each copied row carries the source_ref
;; mytime.db:zTimeSpent:<id>, so a rerun updates rows instead of
;; duplicating them.

(defconst writing-habit-track-sync-source-table "zTimeSpent"
  "The table of a mytime.db database that holds timed sessions.")

(defconst writing-habit-track-sync-scopes '("semester" "legend" "all")
  "Which projects a sync keeps.
semester keeps the week's legend and the current and completed semester
lists, legend keeps only the week's legend, and all keeps every project.")

(defcustom writing-habit-track-mytime-db "~/6003TimeTracking/cb/mytime.db"
  "The mytime.db time-tracking database that `track sync' reads."
  :type 'file
  :group 'writing-habit)

(defconst writing-habit-track--activity-prefix
  '(("g" . "generative") ("e" . "editing") ("r" . "editing") ("s" . "support"))
  "First letter of the tracker's Activity tag mapped to an activity.")

(cl-defstruct (writing-habit-sync
               (:constructor writing-habit-sync-create ()))
  "What one sync read and did.
LEFT-OUT-MINUTES is an alist in first-seen order."
  (rows-read 0) (inserted 0) (updated 0) (unchanged 0)
  (skipped-no-time 0) (skipped-no-project 0) (skipped-not-in-legend 0)
  (skipped-no-legend 0) (semester-unplanned 0)
  (weeks-without-plan nil) (weeks-without-legend nil) (left-out-minutes nil))

(defun writing-habit-track-project-numbers (text)
  "Return the four-digit project numbers at the start of tokens in TEXT.
A number counts when no digit precedes it and a letter follows it, as in
0104BaGquad.  This matches PROJECT_NUMBER in the Python package."
  (let ((found '()) (start 0) (text (or text "")))
    (while (string-match "\\(?:\\`\\|[^0-9]\\)\\([0-9]\\{4\\}\\)[A-Za-z]" text start)
      (push (match-string 1 text) found)
      (setq start (match-end 1)))
    (nreverse found)))

(defun writing-habit-track-legend-numbers (table-path)
  "Return an alist of (PROJECT-NUMBER . CODE) from the legend of TABLE-PATH.
A code whose description names two projects claims both numbers.  When a
number appears under two codes the first code wins."
  (let ((numbers '()))
    (dolist (item (writing-habit-name-read-legend table-path))
      (dolist (n (writing-habit-track-project-numbers (cadr item)))
        (unless (assoc n numbers)
          (push (cons n (car item)) numbers))))
    (nreverse numbers)))

(defun writing-habit-track-semester-numbers (table-path)
  "Return the project numbers on the semester lists of TABLE-PATH.
A project counts while it is on the current list and after it moves to
the completed list, because late work on a finished project is still
writing."
  (let ((lines (with-temp-buffer
                 (insert-file-contents table-path)
                 (split-string (buffer-string) "\n")))
        (found '()))
    (dolist (name (list writing-habit-lists-current writing-habit-lists-completed))
      (dolist (entry (writing-habit-lists-entries lines name))
        (dolist (n (writing-habit-track-project-numbers
                    (writing-habit-lists-entry-description entry)))
          (cl-pushnew n found :test #'string=))))
    (nreverse found)))

(defun writing-habit-track--clock (value)
  "Return HH:MM for a tracker time such as 3:45 or 03:45, or nil."
  (when (and (stringp value)
             (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\(?::[0-9]\\{2\\}\\)?\\'"
                           (string-trim value)))
    (let ((h (string-to-number (match-string 1 (string-trim value))))
          (m (string-to-number (match-string 2 (string-trim value)))))
      (unless (or (> h 24) (> m 59))
        (format "%02d:%02d" h m)))))

(defun writing-habit-track--span (start end)
  "Minutes from START to END, wrapping past midnight."
  (cl-flet ((mins (s) (+ (* 60 (string-to-number (substring s 0 2)))
                         (string-to-number (substring s 3 5)))))
    (let ((span (- (mins end) (mins start))))
      (if (< span 0) (+ span 1440) span))))

(defun writing-habit-track--monday (day)
  "Return the ISO date of the Monday of the week of DAY."
  (let* ((time (date-to-time (concat day " 12:00:00")))
         (dow (string-to-number (format-time-string "%u" time))))
    (format-time-string "%Y-%m-%d" (time-subtract time (days-to-time (1- dow))))))

(defun writing-habit-track--project-number (project-id directory)
  "Return the four-digit project number from PROJECT-ID or DIRECTORY, or nil."
  (cond
   ((integerp project-id) (format "%04d" project-id))
   ((and (stringp project-id) (string-match-p "\\`[0-9]+\\'" (string-trim project-id)))
    (format "%04d" (string-to-number (string-trim project-id))))
   ((and (stringp directory) (string-match "\\`[ \t]*\\([0-9]\\{4\\}\\)" directory))
    (match-string 1 directory))))

(defun writing-habit-track--string (value)
  "Return VALUE trimmed when it is a string, else the empty string."
  (if (stringp value) (string-trim value) ""))

(defun writing-habit-track-default-since (db)
  "Return the first planned week in DB, or this week's Monday."
  (or (caar (sqlite-select db "SELECT MIN(week_start) FROM plan_week"))
      (writing-habit-track--monday (format-time-string "%Y-%m-%d"))))

(defun writing-habit-track--legends (db fallback)
  "Return a closure that finds the legend governing a week.
DB lists the planned weeks; FALLBACK is a table file or nil.  The closure
takes a Monday and the report and returns (NUMBERS . SEMESTER), where
NUMBERS is an alist of project numbers to codes, or nil when no legend
governs the week, and SEMESTER is the list of semester project numbers of
the table that supplied the legend."
  (let ((weeks (sqlite-select
                db (concat "SELECT week_start, table_path FROM plan_week"
                           " WHERE table_path IS NOT NULL ORDER BY week_start")))
        (cache (make-hash-table :test #'equal))
        (semester (make-hash-table :test #'equal)))
    (cl-labels
        ((read (path)
           (or (gethash path cache)
               (puthash path (and (file-regular-p path)
                                  (or (writing-habit-track-legend-numbers path) :none))
                        cache)))
         (numbers (path) (let ((v (read path))) (and (consp v) v)))
         (sem (path)
           (or (gethash path semester)
               (puthash path (if (file-regular-p path)
                                 (writing-habit-track-semester-numbers path)
                               nil)
                        semester))))
      (lambda (monday report)
        (let ((exact (cadr (assoc monday weeks))))
          (if (and exact (numbers exact))
              (cons (numbers exact) (sem exact))
            (let ((candidates
                   (delq nil (list fallback
                                   (cadr (car (last (seq-filter
                                                     (lambda (w) (string< (car w) monday))
                                                     weeks))))
                                   (cadr (car weeks)))))
                  (found nil))
              (while (and candidates (not found))
                (when (numbers (car candidates))
                  (setq found (car candidates)))
                (setq candidates (cdr candidates)))
              (if found
                  (progn
                    (cl-pushnew monday (writing-habit-sync-weeks-without-plan report)
                                :test #'string=)
                    (cons (numbers found) (sem found)))
                (cl-pushnew monday (writing-habit-sync-weeks-without-legend report)
                            :test #'string=)
                (cons nil nil)))))))))

(defun writing-habit-track--planned-category (db day pid start end)
  "Return the category of the planned block of PID on DAY overlapping START to END."
  (caar (sqlite-select
         db (concat "SELECT category_id FROM plan_block WHERE day = ? AND project_id = ?"
                    " AND start_time < ? AND end_time > ? ORDER BY start_time LIMIT 1")
         (list day pid end start))))

(defun writing-habit-track--left-out (report name minutes)
  "Add MINUTES to NAME among the projects REPORT left out."
  (let ((cell (assoc name (writing-habit-sync-left-out-minutes report))))
    (if cell
        (setcdr cell (+ (cdr cell) minutes))
      (setf (writing-habit-sync-left-out-minutes report)
            (append (writing-habit-sync-left-out-minutes report)
                    (list (cons name minutes)))))))

(cl-defun writing-habit-track-sync-mytime (db source &key since until (scope "semester")
                                              table dry-run)
  "Copy timed sessions from the mytime.db database SOURCE into DB.
SINCE and UNTIL bound the days copied; SINCE defaults to the first
planned week.  SCOPE is semester, legend, or all, as in
`writing-habit-track-sync-scopes'.  TABLE is a weekly org table whose
legend serves weeks with no imported plan.  With DRY-RUN, count what
would change and write nothing.  Return a
`writing-habit-sync'."
  (unless (member scope writing-habit-track-sync-scopes)
    (error "Scope must be one of %s" (string-join writing-habit-track-sync-scopes ", ")))
  (let ((src-path (expand-file-name source)))
    (unless (file-regular-p src-path)
      (error "No time-tracking database at %s" src-path))
    (let* ((since (or since (writing-habit-track-default-since db)))
           (rows (let ((src (sqlite-open src-path)))
                   (unwind-protect
                       (sqlite-select
                        src
                        (concat "SELECT id, DateDashed, Start, \"End\", ProjectID,"
                                " ProjectDirectory, Description, Activity FROM "
                                writing-habit-track-sync-source-table
                                " WHERE DateDashed >= ?"
                                (if until " AND DateDashed <= ?" "")
                                " ORDER BY DateDashed, Start")
                        (if until (list since until) (list since)))
                     (sqlite-close src))))
           (legends (writing-habit-track--legends db (and table (expand-file-name table))))
           (report (writing-habit-sync-create))
           (ref-prefix (format "%s:%s:" (file-name-nondirectory src-path)
                               writing-habit-track-sync-source-table)))
      (sqlite-transaction db)
      (condition-case err
          (dolist (r rows)
            (cl-incf (writing-habit-sync-rows-read report))
            (pcase-let* ((`(,id ,day0 ,start0 ,end0 ,project-id ,dir0 ,desc ,activity) r)
                         (day (writing-habit-track--string day0))
                         (start (writing-habit-track--clock start0))
                         (end (writing-habit-track--clock end0)))
              (catch 'next
                (when (or (string-empty-p day) (not start) (not end))
                  (cl-incf (writing-habit-sync-skipped-no-time report))
                  (throw 'next nil))
                (let ((minutes (writing-habit-track--span start end))
                      (number (writing-habit-track--project-number project-id dir0))
                      (directory (writing-habit-track--string dir0)))
                  (when (<= minutes 0)
                    (cl-incf (writing-habit-sync-skipped-no-time report))
                    (throw 'next nil))
                  (when (and (not number) (string-empty-p directory))
                    (cl-incf (writing-habit-sync-skipped-no-project report))
                    (throw 'next nil))
                  (pcase-let* ((`(,numbers . ,semester)
                                (funcall legends (writing-habit-track--monday day) report)))
                    (when (and (null numbers) (not (equal scope "all")))
                      (cl-incf (writing-habit-sync-skipped-no-legend report))
                      (throw 'next nil))
                    (let* ((letter (and number (cdr (assoc number numbers))))
                           (on-semester (and (null letter) (equal scope "semester") number
                                             (member number semester))))
                      (cond
                       (on-semester
                        (cl-incf (writing-habit-sync-semester-unplanned report)))
                       ((and (null letter) (not (equal scope "all")))
                        (cl-incf (writing-habit-sync-skipped-not-in-legend report))
                        (writing-habit-track--left-out
                         report (if (string-empty-p directory) number directory) minutes)
                        (throw 'next nil)))
                      (let* ((code (or letter (if (string-empty-p directory) number directory)))
                             (pid (writing-habit-db-get-or-create-project
                                   db code (unless (or letter (string-empty-p directory))
                                             directory)))
                             (tag (downcase (writing-habit-track--string activity)))
                             (cat-name (and (not (string-empty-p tag)) (not (equal tag "none"))
                                            (cdr (assoc (substring tag 0 1)
                                                        writing-habit-track--activity-prefix))))
                             (cat-id (if cat-name
                                         (writing-habit-db-get-category-id db cat-name)
                                       (writing-habit-track--planned-category
                                        db day pid start end)))
                             (note (writing-habit-track--nonempty
                                    (and (stringp desc) desc)))
                             (ref (format "%s%s" ref-prefix id))
                             (values (list day start end minutes pid cat-id note))
                             (old (car (sqlite-select
                                        db (concat "SELECT session_id, day, start_time, end_time,"
                                                   " actual_min, project_id, category_id, note"
                                                   " FROM session WHERE source = 'sqlite'"
                                                   " AND source_ref = ?")
                                        (list ref)))))
                        (cond
                         ((null old)
                          (sqlite-execute
                           db (concat "INSERT INTO session(day, start_time, end_time, actual_min,"
                                      " project_id, category_id, source, source_ref, note)"
                                      " VALUES (?, ?, ?, ?, ?, ?, 'sqlite', ?, ?)")
                           (append (seq-take values 6) (list ref note)))
                          (cl-incf (writing-habit-sync-inserted report)))
                         ((not (equal (cdr old) values))
                          (sqlite-execute
                           db (concat "UPDATE session SET day = ?, start_time = ?, end_time = ?,"
                                      " actual_min = ?, project_id = ?, category_id = ?, note = ?"
                                      " WHERE session_id = ?")
                           (append values (list (car old))))
                          (cl-incf (writing-habit-sync-updated report)))
                         (t (cl-incf (writing-habit-sync-unchanged report)))))))))))
        (error (sqlite-rollback db) (signal (car err) (cdr err))))
      (if dry-run
          (sqlite-rollback db)
        (sqlite-commit db)
        (writing-habit-db-log-import
         db "sqlite" src-path (writing-habit-sync-rows-read report)
         (+ (writing-habit-sync-inserted report)
            (writing-habit-sync-updated report))
         nil (concat "track sync since " since)))
      report)))

(defun writing-habit-track-sync-report-string (report source since until dry-run)
  "Return REPORT of a sync of SOURCE from SINCE to UNTIL as text.
DRY-RUN non-nil says that nothing was written.  The text matches the
Python package's report line for line."
  (let* ((r report)
         (span (if until (format "%s to %s" since until) (format "%s onward" since)))
         (lines
          (list
           (format "%sRead %d rows of %s in %s (%s)."
                   (if dry-run "Dry run, nothing written. " "")
                   (writing-habit-sync-rows-read r)
                   writing-habit-track-sync-source-table source span)
           (format "  inserted %d, updated %d, unchanged %d"
                   (writing-habit-sync-inserted r)
                   (writing-habit-sync-updated r)
                   (writing-habit-sync-unchanged r))
           (format "  skipped %d without start and end times"
                   (writing-habit-sync-skipped-no-time r))
           (format "  skipped %d without a project"
                   (writing-habit-sync-skipped-no-project r))
           (format "  left out %d whose project is not in the week's legend"
                   (writing-habit-sync-skipped-not-in-legend r)))))
    (when (> (writing-habit-sync-semester-unplanned r) 0)
      (setq lines (append lines (list (format (concat "  kept %d on the current semester list"
                                                      " but not in the week's legend,"
                                                      " as unplanned work")
                                              (writing-habit-sync-semester-unplanned r))))))
    (when (> (writing-habit-sync-skipped-no-legend r) 0)
      (setq lines
            (append lines
                    (list (format "  left out %d in weeks with no legend to read: %s"
                                  (writing-habit-sync-skipped-no-legend r)
                                  (string-join (sort (copy-sequence
                                                      (writing-habit-sync-weeks-without-legend r))
                                                     #'string<)
                                               ", "))
                          (concat "    Import the plan for these weeks (plan import), or name a table "
                                  "with --table; the Track tab uses the table open in the Schedule tab.")))))
    (when (writing-habit-sync-weeks-without-plan r)
      (setq lines
            (append lines
                    (list (format "  weeks with no imported plan, read with another table's legend: %s"
                                  (string-join (sort (copy-sequence
                                                      (writing-habit-sync-weeks-without-plan r))
                                                     #'string<)
                                               ", "))))))
    (when (writing-habit-sync-left-out-minutes r)
      (setq lines (append lines (list "  projects left out, with their minutes:")))
      (dolist (cell (seq-take (sort (copy-sequence (writing-habit-sync-left-out-minutes r))
                                    (lambda (a b) (> (cdr a) (cdr b))))
                              12))
        (setq lines (append lines (list (format "    %-36s %6d" (car cell) (cdr cell)))))))
    (string-join lines "\n")))

;;;; Interactive wrappers over a database file

;;;###autoload
(defun writing-habit-track-add-to-file (db-file day project minutes category note)
  "Add one session to the database at DB-FILE and report its id.
DAY is the ISO date, PROJECT the code, and NOTE an optional remark.
Interactively, prompt for each field.  MINUTES is the whole minutes
worked; CATEGORY is one of the three activities or empty."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (org-read-date nil nil nil "Day: ")
         (read-string "Project code: ")
         (read-number "Minutes: ")
         (completing-read "Category: " writing-habit-track-categories nil nil)
         (read-string "Note: ")))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (let ((id (writing-habit-track-add
                   db :day day :project project :minutes minutes
                   :category (writing-habit-track--nonempty category)
                   :note (writing-habit-track--nonempty note))))
          (message "Added session %s" id)
          id)
      (writing-habit-db-close db))))

;;;###autoload
(defun writing-habit-track-import-csv-file (db-file csv)
  "Import the tracking CSV at CSV into the database at DB-FILE and report."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (read-file-name "Actuals CSV: " nil nil t)))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (let ((n (writing-habit-track-import-csv db csv)))
          (message "Imported %d sessions from %s" n csv)
          n)
      (writing-habit-db-close db))))

;;;###autoload
(defun writing-habit-track-import-ics-file (db-file ics)
  "Import the actuals calendar at ICS into the database at DB-FILE and report."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (read-file-name "Actuals ICS: " nil nil t)))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (let ((n (writing-habit-track-import-ics db ics)))
          (message "Imported %d sessions from %s" n ics)
          n)
      (writing-habit-db-close db))))

;;;###autoload
(defun writing-habit-track-harvest-clock-file (db-file org-file)
  "Harvest completed org clocks from ORG-FILE into the database at DB-FILE."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (read-file-name "Org file with clocks: " nil nil t)))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (let ((n (writing-habit-track-harvest-clock db org-file)))
          (message "Harvested %d clocked sessions from %s" n org-file)
          n)
      (writing-habit-db-close db))))

;;;###autoload
(defun writing-habit-track-sync-mytime-file (db-file source &optional scope dry-run)
  "Copy sessions from the mytime.db SOURCE into the database at DB-FILE.
SCOPE is semester, legend, or all; interactively, a prefix argument asks
for it and makes a dry run, which writes nothing.  Show the report in a
buffer and return it as a string."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (read-file-name "mytime.db: " nil writing-habit-track-mytime-db t
                         writing-habit-track-mytime-db)
         (if current-prefix-arg
             (completing-read "Scope: " writing-habit-track-sync-scopes nil t nil nil "semester")
           "semester")
         (and current-prefix-arg t)))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (let* ((since (writing-habit-track-default-since db))
               (report (writing-habit-track-sync-mytime
                        db source :scope (or scope "semester") :dry-run dry-run))
               (text (writing-habit-track-sync-report-string report source since nil dry-run)))
          (when (called-interactively-p 'interactive)
            (with-help-window "*writing-habit sync*" (princ text)))
          text)
      (writing-habit-db-close db))))

(provide 'writing-habit-track)
;;; writing-habit-track.el ends here
