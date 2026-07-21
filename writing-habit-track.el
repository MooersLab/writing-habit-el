;;; writing-habit-track.el --- Record actual writing sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT license.

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


;;;; Interactive wrappers over a database file

;;;###autoload
(defun writing-habit-track-add-to-file (db-file day project minutes category note)
  "Add one session to the database at DB-FILE and report its id.
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

(provide 'writing-habit-track)
;;; writing-habit-track.el ends here
