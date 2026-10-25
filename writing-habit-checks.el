;;; writing-habit-checks.el --- Planning checks, rescue, and reviews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Keywords: tools, convenience

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

;; The twin of deadlines.py, energy.py, rescue.py, semester.py, and the
;; partner summary and block outcomes of coach.py and review.py.  The
;; software computes every fact and every proposal here.  A local model,
;; when `writing-habit-coach-use-model' allows it, only words the note,
;; and a note that holds a number the facts do not hold is replaced by
;; the plain note.  The plain notes match the Python port word for word.
;;
;; Commands, run from a buffer that shows a weekly table:
;;   `writing-habit-checks-coverage'    deadlines due soon with no block left
;;   `writing-habit-checks-energy'      blocks that do not suit their hours
;;   `writing-habit-checks-energy-show' the energy profile as a grid
;;   `writing-habit-checks-energy-from-reviews'  the profile from focus ratings
;;   `writing-habit-checks-rescue'      make up missed blocks or let them go
;;   `writing-habit-checks-partner'     the Friday summary for a partner
;;   `writing-habit-checks-semester'    a semester narrative for an annual report

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-coach)
(require 'writing-habit-db)
(require 'writing-habit-lists)
(require 'writing-habit-name)
(require 'writing-habit-table)

(declare-function writing-habit-table-current "writing-habit-table-mode")
(declare-function writing-habit-table--apply "writing-habit-table-mode" (model))
(defvar writing-habit-dispatch--last-db)
(defvar writing-habit-default-db)

(defgroup writing-habit-checks nil
  "Planning checks, missed-block rescue, and reviews."
  :group 'tools
  :prefix "writing-habit-checks-")

(defcustom writing-habit-checks-coverage-days 28
  "Days ahead of today in which a deadline counts as soon."
  :type 'integer)

(defcustom writing-habit-checks-energy-file
  (locate-user-emacs-file "writing-habit-energy.json")
  "File of the energy profile, in the JSON form the Python port saves."
  :type 'file)

(defcustom writing-habit-checks-semester nil
  "The semester to review, as (NAME START END) with ISO dates, or nil.
With nil, the last sixteen weeks up to today stand in for it."
  :type '(choice (const nil) (list string string string)))

(defconst writing-habit-checks-day-names
  ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"]
  "The names of the days, Monday first.")

(defconst writing-habit-checks-months
  ["January" "February" "March" "April" "May" "June" "July" "August"
   "September" "October" "November" "December"]
  "The names of the months.")

;;;; Small helpers

(defun writing-habit-checks--abs (iso)
  "Return the absolute day number of the ISO date string ISO."
  (calendar-absolute-from-gregorian
   (list (string-to-number (substring iso 5 7))
         (string-to-number (substring iso 8 10))
         (string-to-number (substring iso 0 4)))))

(defun writing-habit-checks--iso (day)
  "Return the absolute day number DAY as an ISO date string."
  (let ((date (calendar-gregorian-from-absolute day)))
    (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date))))

(defun writing-habit-checks--valid-iso (text)
  "Return TEXT when it is a real ISO date, else nil."
  (when (and text (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" text))
    (let ((month (string-to-number (substring text 5 7)))
          (day (string-to-number (substring text 8 10)))
          (year (string-to-number (substring text 0 4))))
      (when (and (<= 1 month 12) (<= 1 day (calendar-last-day-of-month month year)))
        text))))

(defun writing-habit-checks--today ()
  "Return today as an ISO date string."
  (format-time-string "%Y-%m-%d"))

(defun writing-habit-checks--minute (clock)
  "Return the minute of the day of the HH:MM string CLOCK, or nil."
  (when (and clock (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)" clock))
    (+ (* 60 (string-to-number (match-string 1 clock)))
       (string-to-number (match-string 2 clock)))))

(defun writing-habit-checks--clock (minute)
  "Return MINUTE of the day as HH:MM."
  (format "%02d:%02d" (/ minute 60) (% minute 60)))

(defun writing-habit-checks--hours (minutes)
  "Write MINUTES as hours and minutes, as the Python port does."
  (let ((hours (/ minutes 60)) (rest (% minutes 60)))
    (if (> hours 0) (format "%d h %02d min" hours rest) (format "%d min" rest))))

(defun writing-habit-checks--pct (actual planned)
  "Return ACTUAL as a rounded percent of PLANNED, or nil without a plan."
  (when (and planned (> planned 0))
    (round (* 100.0 (/ (float actual) planned)))))

(defun writing-habit-checks--series (items)
  "Join ITEMS as a writer would, with commas and a final and."
  (pcase (length items)
    (0 "")
    (1 (car items))
    (2 (concat (car items) " and " (cadr items)))
    (_ (concat (string-join (butlast items) ", ") ", and " (car (last items))))))

(defun writing-habit-checks--key< (a b)
  "Compare the key lists A and B element by element.
Numbers compare by value, strings by `string<', and booleans as nil
before t."
  (catch 'done
    (while (or a b)
      (let ((x (car a)) (y (car b)))
        (cond ((and (numberp x) (numberp y))
               (unless (= x y) (throw 'done (< x y))))
              ((and (stringp x) (stringp y))
               (unless (string= x y) (throw 'done (string< x y))))
              (t (let ((x (if x 1 0)) (y (if y 1 0)))
                   (unless (= x y) (throw 'done (< x y)))))))
      (setq a (cdr a) b (cdr b)))
    nil))

(defun writing-habit-checks--sort-by (key items)
  "Return ITEMS sorted stably by the key list KEY gives each one."
  (sort (copy-sequence items)
        (lambda (a b) (writing-habit-checks--key< (funcall key a) (funcall key b)))))

(defun writing-habit-checks--rstrip-dot (text)
  "Return TEXT without trailing full stops."
  (replace-regexp-in-string "\\.+\\'" "" text))

(defun writing-habit-checks--clean (text)
  "Collapse the white space of TEXT."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or text ""))))

(defun writing-habit-checks--tables (db)
  "Return the names of the tables and views of DB."
  (mapcar #'car (sqlite-select db "SELECT name FROM sqlite_master
 WHERE type IN ('table','view')")))

;;;; Slots: the blocks of the open week

(cl-defstruct (writing-habit-checks-slot
               (:constructor writing-habit-checks-slot-create)
               (:copier nil))
  "One planned block of the open week."
  offset start end code category minutes (row -1) (column -1))

(defun writing-habit-checks-slot-label (slot)
  "Return the day and times of SLOT, as Monday 05:00-06:30."
  (format "%s %s-%s" (aref writing-habit-checks-day-names
                           (writing-habit-checks-slot-offset slot))
          (writing-habit-checks-slot-start slot) (writing-habit-checks-slot-end slot)))

(defun writing-habit-checks-slots (table)
  "Return the blocks of TABLE as slots."
  (mapcar (lambda (b)
            (writing-habit-checks-slot-create
             :offset (writing-habit-table-block-offset b)
             :start (writing-habit-table-block-start b)
             :end (writing-habit-table-block-end b)
             :code (writing-habit-table-block-letter b)
             :category (writing-habit-table-block-category b)
             :minutes (writing-habit-table-block-minutes b)
             :row (writing-habit-table-block-row b)
             :column (writing-habit-table-block-column b)))
          (writing-habit-table-blocks table)))

(defun writing-habit-checks--legend (table)
  "Return the legend of TABLE as an alist of (CODE . DESCRIPTION)."
  (mapcar (lambda (entry) (cons (nth 0 entry) (nth 1 entry)))
          (writing-habit-table-legend table)))

;;;; Deadline coverage (Rule 3)

(cl-defstruct (writing-habit-checks-project
               (:constructor writing-habit-checks-project-create)
               (:copier nil))
  "A project with an ISO deadline, from the legend or from a list."
  name deadline code in-legend (source "legend") (description ""))

(cl-defstruct (writing-habit-checks-risk
               (:constructor writing-habit-checks-risk-create)
               (:copier nil))
  "A project due soon with no block left this week, and the block proposed."
  project days-left planned ahead donor (donor-name "") donor-deadline (donor-keeps 0))

(cl-defstruct (writing-habit-checks-coverage
               (:constructor writing-habit-checks-coverage-create)
               (:copier nil))
  "The result of a coverage check."
  today horizon risks covered)

(defun writing-habit-checks-projects-due (legend lists today &optional horizon)
  "Return the projects of LEGEND and LISTS due within HORIZON days of TODAY.
LEGEND is an alist of (CODE . DESCRIPTION) and LISTS a list of
\(LIST-NAME CODE DESCRIPTION).  A project in both comes from the legend.
The soonest comes first."
  (let* ((horizon (or horizon writing-habit-checks-coverage-days))
         (first (writing-habit-checks--abs today))
         (last (+ first horizon))
         (found '()))
    (cl-flet ((due-p (day) (and day (<= first (writing-habit-checks--abs day) last))))
      (dolist (pair legend)
        (let* ((split (writing-habit-name-split-deadline (cdr pair)))
               (day (writing-habit-checks--valid-iso (cdr split))))
          (when (and (due-p day) (not (assoc (car split) found)))
            (push (cons (car split)
                        (writing-habit-checks-project-create
                         :name (car split) :deadline day :code (car pair)
                         :in-legend t :description (cdr pair)))
                  found))))
      (dolist (item lists)
        (let* ((split (writing-habit-name-split-deadline (nth 2 item)))
               (day (writing-habit-checks--valid-iso (cdr split))))
          (when (and (due-p day) (not (assoc (car split) found)))
            (push (cons (car split)
                        (writing-habit-checks-project-create
                         :name (car split) :deadline day :code (nth 1 item)
                         :in-legend nil :source (nth 0 item)
                         :description (nth 2 item)))
                  found)))))
    (writing-habit-checks--sort-by
     (lambda (p) (list (writing-habit-checks-project-deadline p)
                       (writing-habit-checks-project-name p)))
     (mapcar #'cdr (nreverse found)))))

(defun writing-habit-checks-coverage-check (legend lists slots monday today &optional horizon)
  "Find the projects due soon with no block ahead, and propose a block for each.
LEGEND, LISTS, and HORIZON are as for `writing-habit-checks-projects-due',
SLOTS are the week's blocks, MONDAY the week's ISO Monday, and TODAY an
ISO date.  The rules follow deadlines.py."
  (let* ((horizon (or horizon writing-habit-checks-coverage-days))
         (due (writing-habit-checks-projects-due legend lists today horizon))
         (due-codes (delq nil (mapcar (lambda (p) (and (writing-habit-checks-project-in-legend p)
                                                       (writing-habit-checks-project-code p)))
                                      due)))
         (start (writing-habit-checks--abs monday))
         (now (writing-habit-checks--abs today))
         (ahead (seq-filter (lambda (s) (>= (+ start (writing-habit-checks-slot-offset s)) now))
                            slots))
         (planned (make-hash-table :test #'equal))
         (left (make-hash-table :test #'equal))
         (blocks-left (make-hash-table :test #'equal))
         (names (mapcar (lambda (p) (cons (car p) (car (writing-habit-name-split-deadline (cdr p)))))
                        legend))
         (deadlines (mapcar (lambda (p) (cons (car p) (writing-habit-checks--valid-iso
                                                       (cdr (writing-habit-name-split-deadline (cdr p))))))
                            legend))
         (used '()) (risks '()) (covered '()))
    (dolist (s slots)
      (cl-incf (gethash (writing-habit-checks-slot-code s) planned 0)
               (writing-habit-checks-slot-minutes s)))
    (dolist (s ahead)
      (cl-incf (gethash (writing-habit-checks-slot-code s) left 0)
               (writing-habit-checks-slot-minutes s))
      (cl-incf (gethash (writing-habit-checks-slot-code s) blocks-left 0)))
    (dolist (project due)
      (let* ((code (and (writing-habit-checks-project-in-legend project)
                        (writing-habit-checks-project-code project)))
             (spent (if code (gethash code planned 0) 0))
             (still (if code (gethash code left 0) 0)))
        (if (> still 0)
            (push (list project spent still) covered)
          (let ((risk (writing-habit-checks-risk-create
                       :project project
                       :days-left (- (writing-habit-checks--abs
                                      (writing-habit-checks-project-deadline project))
                                     now)
                       :planned spent :ahead still)))
            (catch 'found
              (dolist (s (writing-habit-checks--sort-by
                          (lambda (s)
                            (let ((c (writing-habit-checks-slot-code s)))
                              (list (equal (writing-habit-checks-slot-category s) "support")
                                    (and (cdr (assoc c deadlines)) t)
                                    (- (gethash c left 0))
                                    (writing-habit-checks-slot-offset s)
                                    (writing-habit-checks-slot-start s))))
                          ahead))
                (let ((c (writing-habit-checks-slot-code s))
                      (key (cons (writing-habit-checks-slot-row s)
                                 (writing-habit-checks-slot-column s))))
                  (unless (or (member key used) (member c due-codes)
                              (and (cdr (assoc c deadlines)) (< (gethash c blocks-left 0) 2)))
                    (push key used)
                    (cl-decf (gethash c blocks-left 0))
                    (cl-decf (gethash c left 0) (writing-habit-checks-slot-minutes s))
                    (setf (writing-habit-checks-risk-donor risk) s
                          (writing-habit-checks-risk-donor-name risk) (or (cdr (assoc c names)) c)
                          (writing-habit-checks-risk-donor-deadline risk) (cdr (assoc c deadlines))
                          (writing-habit-checks-risk-donor-keeps risk) (gethash c left 0))
                    (throw 'found nil)))))
            (push risk risks)))))
    (writing-habit-checks-coverage-create :today today :horizon horizon
                                          :risks (nreverse risks) :covered (nreverse covered))))

(defun writing-habit-checks--days (n)
  "Say how far ahead N days is."
  (pcase n (0 "today") (1 "tomorrow") (_ (format "in %d days" n))))

(defun writing-habit-checks--who (project)
  "Name PROJECT with its code when it has one."
  (if (writing-habit-checks-project-code project)
      (format "%s %s" (writing-habit-checks-project-code project)
              (writing-habit-checks-project-name project))
    (writing-habit-checks-project-name project)))

(defun writing-habit-checks-coverage-facts (result)
  "Write the coverage RESULT as the facts the model receives."
  (let ((lines (list (format "Today: %s" (writing-habit-checks-coverage-today result))
                     (format "Deadlines counted: the next %d days"
                             (writing-habit-checks-coverage-horizon result)))))
    (dolist (risk (writing-habit-checks-coverage-risks result))
      (let ((p (writing-habit-checks-risk-project risk))
            (d (writing-habit-checks-risk-donor risk)))
        (push (concat (format "Due soon with no block left this week: %s, due %s, %s"
                              (writing-habit-checks--who p)
                              (writing-habit-checks-project-deadline p)
                              (writing-habit-checks--days (writing-habit-checks-risk-days-left risk)))
                      (if (writing-habit-checks-project-in-legend p) ""
                        (format ", waiting on the %s list" (writing-habit-checks-project-source p))))
              lines)
        (push (if d
                  (format "Proposed block for it: %s, a %s block of %d minutes (%s) now planned for %s %s, which %s and keeps %d minutes (%s) ahead this week"
                          (writing-habit-checks-slot-label d) (writing-habit-checks-slot-category d)
                          (writing-habit-checks-slot-minutes d)
                          (writing-habit-checks--hours (writing-habit-checks-slot-minutes d))
                          (writing-habit-checks-slot-code d) (writing-habit-checks-risk-donor-name risk)
                          (if (writing-habit-checks-risk-donor-deadline risk)
                              (format "is due %s" (writing-habit-checks-risk-donor-deadline risk))
                            "has no deadline")
                          (writing-habit-checks-risk-donor-keeps risk)
                          (writing-habit-checks--hours (writing-habit-checks-risk-donor-keeps risk)))
                "Proposed block for it: none, because every block ahead belongs to a project that is due soon")
              lines)))
    (dolist (c (writing-habit-checks-coverage-covered result))
      (push (format "Covered: %s, due %s, %d minutes (%s) still ahead this week"
                    (writing-habit-checks--who (nth 0 c))
                    (writing-habit-checks-project-deadline (nth 0 c))
                    (nth 2 c) (writing-habit-checks--hours (nth 2 c)))
            lines))
    (string-join (nreverse lines) "\n")))

(defun writing-habit-checks-coverage-line (risk)
  "Return one proposal line for RISK."
  (let ((d (writing-habit-checks-risk-donor risk)))
    (format "Give %s (%s) to %s, taking it from %s %s"
            (writing-habit-checks-slot-label d) (writing-habit-checks-slot-category d)
            (writing-habit-checks--who (writing-habit-checks-risk-project risk))
            (writing-habit-checks-slot-code d) (writing-habit-checks-risk-donor-name risk))))

(defun writing-habit-checks-coverage-plain (result)
  "Describe the coverage RESULT without a model."
  (let ((risks (writing-habit-checks-coverage-risks result))
        (covered (writing-habit-checks-coverage-covered result))
        (horizon (writing-habit-checks-coverage-horizon result)))
    (if (null risks)
        (if (null covered)
            (format "No project in the legend or the lists is due in the next %d days." horizon)
          (format "Every project due in the next %d days has time left in this week's plan."
                  horizon))
      (let ((out '()))
        (dolist (risk risks)
          (let* ((p (writing-habit-checks-risk-project risk))
                 (d (writing-habit-checks-risk-donor risk))
                 (text (format "%s is due %s, %s, and has no block left this week.%s"
                               (writing-habit-checks--who p) (writing-habit-checks-project-deadline p)
                               (writing-habit-checks--days (writing-habit-checks-risk-days-left risk))
                               (if (writing-habit-checks-project-in-legend p) ""
                                 (format " It waits on the %s list."
                                         (writing-habit-checks-project-source p))))))
            (setq text
                  (concat text
                          (if d
                              (format " Give it %s, now planned for %s %s, which %s and keeps %s this week."
                                      (writing-habit-checks-slot-label d)
                                      (writing-habit-checks-slot-code d)
                                      (writing-habit-checks-risk-donor-name risk)
                                      (if (writing-habit-checks-risk-donor-deadline risk)
                                          (format "is due later, on %s"
                                                  (writing-habit-checks-risk-donor-deadline risk))
                                        "has no deadline")
                                      (writing-habit-checks--hours
                                       (writing-habit-checks-risk-donor-keeps risk)))
                            " Every block ahead belongs to a project that is due soon, so find time by adding a block or by moving a deadline.")))
            (push text out)))
        (when covered
          (push (format "%d other project%s due soon %s time left this week."
                        (length covered) (if (= (length covered) 1) "" "s")
                        (if (= (length covered) 1) "has" "have"))
                out))
        (string-join (nreverse out) "\n\n")))))

;;;; The energy profile (Rule 2)

(defconst writing-habit-checks-activities '("generative" "editing" "support")
  "The three kinds of writing an energy profile rates.")

(defconst writing-habit-checks-fits 1.0
  "A block fits its activity when the mean level of its hours reaches this.")

(defconst writing-habit-checks-peak 1.5
  "A support block in hours this high for generative work spends peak time.")

(defconst writing-habit-checks-min-reviews 3
  "Reviews needed in an hour before its mean focus can set a level.")

(defun writing-habit-checks--levels (&rest spans)
  "Return 24 levels, low except for the (FIRST LAST LEVEL) SPANS."
  (let ((out (make-vector 24 0)))
    (dolist (span spans)
      (cl-loop for hour from (nth 0 span) below (nth 1 span)
               do (aset out hour (nth 2 span))))
    out))

(defun writing-habit-checks-default-profile ()
  "Return the default energy profile as an alist of (ACTIVITY . LEVELS)."
  (list (cons "generative" (writing-habit-checks--levels '(5 10 2) '(10 12 1) '(19 21 1)))
        (cons "editing" (writing-habit-checks--levels '(5 9 1) '(9 12 2) '(13 17 1) '(17 21 1)))
        (cons "support" (writing-habit-checks--levels '(7 10 1) '(10 13 1) '(13 17 2) '(17 20 1)))))

(defun writing-habit-checks-profile-from-json (text)
  "Read a profile saved as JSON TEXT, falling back to the default for anything bad."
  (let ((profile (writing-habit-checks-default-profile))
        (data (condition-case nil
                  (let ((json-object-type 'alist) (json-array-type 'list)
                        (json-key-type 'string))
                    (json-read-from-string text))
                (error nil))))
    (when (consp data)
      (dolist (activity writing-habit-checks-activities)
        (let ((levels (cdr (assoc activity data))))
          (when (and (listp levels) (= (length levels) 24)
                     (seq-every-p (lambda (l) (memq l '(0 1 2))) levels))
            (setcdr (assoc activity profile) (vconcat levels))))))
    profile))

(defun writing-habit-checks-profile-json (profile)
  "Write PROFILE as compact JSON, as the Python port saves it."
  (concat "{"
          (mapconcat (lambda (activity)
                       (format "\"%s\":[%s]" activity
                               (mapconcat #'number-to-string
                                          (cdr (assoc activity profile)) ",")))
                     writing-habit-checks-activities ",")
          "}"))

(defun writing-habit-checks-profile ()
  "Return the saved energy profile, or the default."
  (writing-habit-checks-profile-from-json
   (if (file-readable-p writing-habit-checks-energy-file)
       (with-temp-buffer
         (insert-file-contents writing-habit-checks-energy-file)
         (buffer-string))
     "")))

(defun writing-habit-checks-fit (profile activity start end)
  "Return the mean level, 0 to 2, of ACTIVITY in PROFILE from START to END."
  (let* ((levels (or (cdr (assoc activity profile)) (cdr (assoc "generative" profile))))
         (first (writing-habit-checks--minute start))
         (last (writing-habit-checks--minute end))
         (last (if (<= last first) (+ last (* 24 60)) last))
         (quarters (cl-loop for m from first below last by 15
                            collect (aref levels (% (/ m 60) 24)))))
    (if quarters (/ (float (apply #'+ quarters)) (length quarters)) 1.0)))

(defun writing-habit-checks-word (value)
  "Name the mean level VALUE the way a writer would say it."
  (cond ((>= value 1.75) "high") ((>= value 1.25) "mostly high")
        ((>= value 0.75) "medium") ((>= value 0.25) "mostly low") (t "low")))

(defun writing-habit-checks-misfits (profile slots)
  "Return (SLOT KIND LEVEL) for the SLOTS that do not fit PROFILE, in time order."
  (let ((out '()))
    (dolist (s (writing-habit-checks--sort-by
                (lambda (s) (list (writing-habit-checks-slot-offset s) (writing-habit-checks-slot-start s)))
                slots))
      (let* ((category (writing-habit-checks-slot-category s))
             (own (writing-habit-checks-fit profile category (writing-habit-checks-slot-start s)
                                            (writing-habit-checks-slot-end s))))
        (cond ((and (member category '("generative" "editing")) (< own writing-habit-checks-fits))
               (push (list s "low" own) out))
              ((and (equal category "support")
                    (>= (writing-habit-checks-fit profile "generative" (writing-habit-checks-slot-start s)
                                                  (writing-habit-checks-slot-end s))
                        writing-habit-checks-peak))
               (push (list s "peak" own) out)))))
    (nreverse out)))

(defun writing-habit-checks--value (profile s start end)
  "Return how well slot S's work fits the hours from START to END in PROFILE."
  (if (equal (writing-habit-checks-slot-category s) "support")
      (- (/ (writing-habit-checks-fit profile "generative" start end) 2))
    (writing-habit-checks-fit profile (writing-habit-checks-slot-category s) start end)))

(defun writing-habit-checks-swaps (profile slots &optional movable)
  "Return swaps (FIRST SECOND BEFORE AFTER) that make the misfits of SLOTS fit.
Only slots for which MOVABLE returns non-nil take part.  Both blocks are
on one day and of one length, and each block is used once."
  (let* ((bad (mapcar (lambda (m) (cons (writing-habit-checks-slot-row (car m))
                                        (writing-habit-checks-slot-column (car m))))
                      (writing-habit-checks-misfits profile slots)))
         (pool (seq-filter (lambda (s) (or (null movable) (funcall movable s))) slots))
         (options '()))
    (cl-loop for (a . rest) on pool
             do (dolist (b rest)
                  (when (and (= (writing-habit-checks-slot-offset a) (writing-habit-checks-slot-offset b))
                             (= (writing-habit-checks-slot-minutes a) (writing-habit-checks-slot-minutes b))
                             (not (and (equal (writing-habit-checks-slot-category a)
                                              (writing-habit-checks-slot-category b))
                                       (equal (writing-habit-checks-slot-code a)
                                              (writing-habit-checks-slot-code b))))
                             (or (member (cons (writing-habit-checks-slot-row a) (writing-habit-checks-slot-column a)) bad)
                                 (member (cons (writing-habit-checks-slot-row b) (writing-habit-checks-slot-column b)) bad)))
                    (let* ((as (writing-habit-checks-slot-start a)) (ae (writing-habit-checks-slot-end a))
                           (bs (writing-habit-checks-slot-start b)) (be (writing-habit-checks-slot-end b))
                           (before (+ (writing-habit-checks--value profile a as ae)
                                      (writing-habit-checks--value profile b bs be)))
                           (after (+ (writing-habit-checks--value profile a bs be)
                                     (writing-habit-checks--value profile b as ae))))
                      (when (> (- after before) 0.25)
                        (push (list a b before after) options))))))
    (let ((used '()) (out '()))
      (dolist (w (writing-habit-checks--sort-by
                  (lambda (w) (list (- (- (nth 3 w) (nth 2 w)))
                                    (writing-habit-checks-slot-offset (nth 0 w))
                                    (writing-habit-checks-slot-start (nth 0 w))))
                  (nreverse options)))
        (let ((keys (list (cons (writing-habit-checks-slot-row (nth 0 w)) (writing-habit-checks-slot-column (nth 0 w)))
                          (cons (writing-habit-checks-slot-row (nth 1 w)) (writing-habit-checks-slot-column (nth 1 w))))))
          (unless (seq-intersection keys used)
            (setq used (append keys used))
            (push w out))))
      (nreverse out))))

(defun writing-habit-checks-swap-line (w)
  "Return one proposal line for the swap W."
  (let ((a (nth 0 w)) (b (nth 1 w)))
    (format "%s: swap %s %s at %s-%s with %s %s at %s-%s"
            (aref writing-habit-checks-day-names (writing-habit-checks-slot-offset a))
            (writing-habit-checks-slot-category a) (writing-habit-checks-slot-code a)
            (writing-habit-checks-slot-start a) (writing-habit-checks-slot-end a)
            (writing-habit-checks-slot-category b) (writing-habit-checks-slot-code b)
            (writing-habit-checks-slot-start b) (writing-habit-checks-slot-end b))))

(defun writing-habit-checks-misfit-line (profile m)
  "Describe the misfit M under PROFILE in one line."
  (let ((s (nth 0 m)))
    (if (equal (nth 1 m) "low")
        (format "%s  %s %s falls in %s hours for %s work"
                (writing-habit-checks-slot-label s) (writing-habit-checks-slot-category s)
                (writing-habit-checks-slot-code s) (writing-habit-checks-word (nth 2 m))
                (writing-habit-checks-slot-category s))
      (format "%s  support %s uses your %s hours for generative work"
              (writing-habit-checks-slot-label s) (writing-habit-checks-slot-code s)
              (writing-habit-checks-word
               (writing-habit-checks-fit profile "generative" (writing-habit-checks-slot-start s)
                                         (writing-habit-checks-slot-end s)))))))

(defun writing-habit-checks-energy-facts (profile found proposed)
  "Write the misfits FOUND and swaps PROPOSED under PROFILE as facts."
  (let ((lines (mapcar (lambda (m) (concat "Block that does not fit: "
                                           (writing-habit-checks-misfit-line profile m)))
                       found)))
    (dolist (w proposed)
      (let* ((a (nth 0 w)) (b (nth 1 w))
             (ca (writing-habit-checks-slot-category a))
             (cb (writing-habit-checks-slot-category b)))
        (setq lines
              (append lines
                      (list (format "Proposed swap: %s. Afterwards %s %s sits in %s hours for %s work and %s %s sits in %s hours for %s work"
                                    (writing-habit-checks-swap-line w)
                                    ca (writing-habit-checks-slot-code a)
                                    (writing-habit-checks-word
                                     (writing-habit-checks-fit profile ca (writing-habit-checks-slot-start b)
                                                               (writing-habit-checks-slot-end b)))
                                    ca cb (writing-habit-checks-slot-code b)
                                    (writing-habit-checks-word
                                     (writing-habit-checks-fit profile cb (writing-habit-checks-slot-start a)
                                                               (writing-habit-checks-slot-end a)))
                                    cb))))))
    (when (and found (null proposed))
      (setq lines (append lines (list "Proposed swap: none, because no block of the same length on the same day would fit better"))))
    (string-join lines "\n")))

(defun writing-habit-checks-energy-plain (profile found proposed)
  "Describe the misfits FOUND and swaps PROPOSED under PROFILE without a model."
  (if (null found)
      "Every block of the week fits your energy profile."
    (let ((n (length found)))
      (string-join
       (append (list (format "%d block%s %s not fit your energy profile." n
                             (if (= n 1) "" "s") (if (= n 1) "does" "do")))
               (mapcar (lambda (m) (concat "  " (writing-habit-checks-misfit-line profile m))) found)
               (list "" (if proposed
                            "Swapping the work of these blocks keeps every project's minutes and puts demanding work in better hours."
                          "No block of the same length on the same day would fit better, so move these by hand or adjust the profile.")))
       "\n"))))

(defun writing-habit-checks-focus-by-hour (db)
  "Return the focus ratings of reviewed blocks in DB by (ACTIVITY . HOUR)."
  (let ((out '()))
    (when (member "session_review" (writing-habit-checks--tables db))
      (dolist (row (sqlite-select db "SELECT r.focus, b.start_time, b.end_time, c.name
 FROM session_review r JOIN project p ON p.code = r.code
 JOIN plan_block b ON b.day = r.day AND b.start_time = r.start_time AND b.project_id = p.project_id
 JOIN category c USING(category_id) WHERE r.focus IS NOT NULL"))
        (let* ((first (writing-habit-checks--minute (nth 1 row)))
               (last (writing-habit-checks--minute (nth 2 row)))
               (last (if (<= last first) (+ last (* 24 60)) last))
               (hours (seq-uniq (cl-loop for m from first below last by 15
                                         collect (% (/ m 60) 24)))))
          (dolist (hour hours)
            (let* ((key (cons (nth 3 row) hour)) (cell (assoc key out)))
              (if cell (setcdr cell (append (cdr cell) (list (nth 0 row))))
                (push (list key (nth 0 row)) out)))))))
    (writing-habit-checks--sort-by (lambda (c) (list (car (car c)) (cdr (car c)))) out)))

(defun writing-habit-checks-level-for (mean)
  "Map a MEAN focus rating, 1 to 5, to an energy level."
  (cond ((>= mean 4.0) 2) ((>= mean 3.0) 1) (t 0)))

(defun writing-habit-checks-profile-from-reviews (db profile)
  "Return (NEW-PROFILE . CHANGES) from the focus ratings in DB.
Each change is (ACTIVITY HOUR OLD NEW MEAN REVIEWS).  Only an hour with
at least `writing-habit-checks-min-reviews' ratings changes."
  (let ((new (mapcar (lambda (p) (cons (car p) (copy-sequence (cdr p)))) profile))
        (changes '()))
    (dolist (cell (writing-habit-checks-focus-by-hour db))
      (let* ((activity (car (car cell))) (hour (cdr (car cell)))
             (ratings (cdr cell)) (levels (cdr (assoc activity new))))
        (when (and levels (>= (length ratings) writing-habit-checks-min-reviews))
          (let* ((mean (/ (float (apply #'+ ratings)) (length ratings)))
                 (level (writing-habit-checks-level-for mean)))
            (unless (= (aref levels hour) level)
              (push (list activity hour (aref levels hour) level
                          (/ (round (* mean 10)) 10.0) (length ratings))
                    changes)
              (aset levels hour level))))))
    (cons new (nreverse changes))))

;;;; Block outcomes

(defun writing-habit-checks-block-outcomes (db week-start &optional today)
  "Return plists of each planned block of WEEK-START in DB and its status.
The status is kept at half the planned minutes or more, partial below
that, missed at none, and ahead for a block after TODAY."
  (let* ((today (or today (writing-habit-checks--today)))
         (blocks (sqlite-select db "SELECT b.day, b.start_time, b.end_time, b.planned_min, p.code, c.name
 FROM plan_block b JOIN project p USING(project_id) JOIN category c USING(category_id)
 WHERE b.week_start = ? ORDER BY b.day, b.start_time, p.code" (list week-start)))
         (sessions (sqlite-select db "SELECT s.day, s.start_time, s.end_time, s.actual_min, p.code
 FROM session s JOIN project p USING(project_id) WHERE s.week_start = ?" (list week-start)))
         (used '()) (out '()))
    (dolist (b blocks)
      (let ((first (writing-habit-checks--minute (nth 1 b)))
            (last (writing-habit-checks--minute (nth 2 b)))
            (planned (nth 3 b)) (actual 0) (index -1))
        (dolist (s sessions)
          (setq index (1+ index))
          (when (and (equal (nth 0 s) (nth 0 b)) (equal (nth 4 s) (nth 4 b)))
            (let ((s-first (writing-habit-checks--minute (nth 1 s)))
                  (s-last (writing-habit-checks--minute (nth 2 s))))
              (if (or (null s-first) (null s-last))
                  (let ((key (list (nth 0 b) (nth 4 b) index)))
                    (unless (member key used)
                      (push key used)
                      (cl-incf actual (min (nth 3 s) planned))))
                (when (<= s-last s-first) (cl-incf s-last (* 24 60)))
                (cl-incf actual (max 0 (- (min last s-last) (max first s-first))))))))
        (setq actual (min actual planned))
        (push (list :day (nth 0 b) :start (nth 1 b) :end (nth 2 b) :planned planned
                    :code (nth 4 b) :category (nth 5 b) :actual actual
                    :status (cond ((string< today (nth 0 b)) "ahead")
                                  ((= actual 0) "missed")
                                  ((>= actual (* 0.5 planned)) "kept")
                                  (t "partial")))
              out)))
    (nreverse out)))

;;;; Missed-block rescue (Rule 9)

(defconst writing-habit-checks-max-rescues 2
  "At most this many missed blocks are rescued in one week.")

(cl-defstruct (writing-habit-checks-rescue
               (:constructor writing-habit-checks-rescue-create)
               (:copier nil))
  "A missed block and where to make it up, or why to let it go."
  missed offset start end (level 0.0) (reason ""))

(defun writing-habit-checks-rescue-label (r)
  "Return the make-up slot of R, as Wednesday 07:15-08:45."
  (format "%s %s-%s" (aref writing-habit-checks-day-names (writing-habit-checks-rescue-offset r))
          (writing-habit-checks-rescue-start r) (writing-habit-checks-rescue-end r)))

(defun writing-habit-checks-rescue-line (r)
  "Return one proposal line for the rescue R."
  (let ((m (writing-habit-checks-rescue-missed r)))
    (format "Make up %s %s from %s on %s" (writing-habit-checks-slot-category m)
            (writing-habit-checks-slot-code m) (writing-habit-checks-slot-label m)
            (writing-habit-checks-rescue-label r))))

(defun writing-habit-checks-missed-blocks (slots outcomes monday now)
  "Return the SLOTS that have passed with nothing tracked.
OUTCOMES come from `writing-habit-checks-block-outcomes', MONDAY is an
ISO date, and NOW is (ISO-DATE . MINUTE-OF-DAY)."
  (let ((now-at (+ (* 1440 (writing-habit-checks--abs (car now))) (cdr now)))
        (start (writing-habit-checks--abs monday)))
    (seq-filter
     (lambda (s)
       (let* ((day (+ start (writing-habit-checks-slot-offset s)))
              (first (writing-habit-checks--minute (writing-habit-checks-slot-start s)))
              (last (writing-habit-checks--minute (writing-habit-checks-slot-end s)))
              (ended (+ (* 1440 day) last (if (<= last first) 1440 0))))
         (and (<= ended now-at)
              (seq-find (lambda (o) (and (equal (plist-get o :day) (writing-habit-checks--iso day))
                                         (equal (plist-get o :start) (writing-habit-checks-slot-start s))
                                         (equal (plist-get o :code) (writing-habit-checks-slot-code s))
                                         (equal (plist-get o :status) "missed")))
                        outcomes))))
     slots)))

(defun writing-habit-checks--fits-runs (intervals)
  "Return non-nil when every run of INTERVALS stays within two hours."
  (let ((run-start nil) (run-end nil) (ok t))
    (dolist (iv (sort (copy-sequence intervals)
                      (lambda (a b) (or (< (car a) (car b))
                                        (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))
      (if (and run-end (< (- (car iv) run-end) 15))
          (setq run-end (max run-end (cdr iv)))
        (setq run-start (car iv) run-end (cdr iv)))
      (when (> (- run-end run-start) 120) (setq ok nil)))
    ok))

(defun writing-habit-checks-find-slot (missed slots profile days monday now &optional taken)
  "Return (OFFSET START END LEVEL) of the best make-up slot for MISSED, or nil.
DAYS are the day offsets of the table, NOW is (ISO-DATE . MINUTE), and
TAKEN holds (OFFSET FIRST LAST) minutes promised to earlier rescues."
  (let* ((length (writing-habit-checks-slot-minutes missed))
         (today (writing-habit-checks--abs (car now)))
         (start (writing-habit-checks--abs monday))
         best)
    (dolist (offset days)
      (let ((day (+ start offset)))
        (unless (or (< day today) (< offset (writing-habit-checks-slot-offset missed)))
          (let ((busy (append
                       (mapcar (lambda (s) (cons (writing-habit-checks--minute (writing-habit-checks-slot-start s))
                                                 (writing-habit-checks--minute (writing-habit-checks-slot-end s))))
                               (seq-filter (lambda (s) (= (writing-habit-checks-slot-offset s) offset)) slots))
                       (mapcar (lambda (tk) (cons (nth 1 tk) (nth 2 tk)))
                               (seq-filter (lambda (tk) (= (car tk) offset)) taken))))
                (earliest (* 5 60)))
            (when (= day today)
              (setq earliest (max earliest (* 15 (ceiling (cdr now) 15)))))
            (cl-loop for first from earliest to (- (* 22 60) length) by 15
                     do (let ((last (+ first length)))
                          (unless (seq-some (lambda (iv) (and (< first (cdr iv)) (< (car iv) last))) busy)
                            (let ((level (writing-habit-checks-fit
                                          profile (writing-habit-checks-slot-category missed)
                                          (writing-habit-checks--clock first)
                                          (writing-habit-checks--clock last))))
                              (when (and (>= level writing-habit-checks-fits)
                                         (writing-habit-checks--fits-runs
                                          (cons (cons first last) busy)))
                                (let ((key (list (- level) (and (seq-find (lambda (tk) (= (car tk) offset)) taken) t)
                                                 offset first)))
                                  (when (or (null best) (writing-habit-checks--key< key (car best)))
                                    (setq best (cons key (list offset (writing-habit-checks--clock first)
                                                               (writing-habit-checks--clock last) level))))))))))))))
    (cdr best)))

(defun writing-habit-checks-rescue-plan (slots missed profile days monday now legend)
  "Decide for each MISSED slot whether to make it up, and where.
LEGEND is an alist of (CODE . DESCRIPTION) that gives the deadlines."
  (let ((order (writing-habit-checks--sort-by
                (lambda (s) (list (equal (writing-habit-checks-slot-category s) "support")
                                  (or (cdr (writing-habit-name-split-deadline
                                            (or (cdr (assoc (writing-habit-checks-slot-code s) legend)) "")))
                                      "9999-12-31")
                                  (- (writing-habit-checks-slot-offset s))
                                  (writing-habit-checks-slot-start s)))
                missed))
        (out '()) (taken '()) (rescued 0))
    (dolist (m order)
      (cond
       ((equal (writing-habit-checks-slot-category m) "support")
        (push (writing-habit-checks-rescue-create
               :missed m :reason "support work fills whatever time it gets, so let it go")
              out))
       ((>= rescued writing-habit-checks-max-rescues)
        (push (writing-habit-checks-rescue-create
               :missed m :reason (format "%d blocks are already being made up this week, and more would overload it"
                                         writing-habit-checks-max-rescues))
              out))
       (t
        (let ((found (writing-habit-checks-find-slot m slots profile days monday now taken)))
          (if (null found)
              (push (writing-habit-checks-rescue-create
                     :missed m :reason "no free time of that length is left this week in hours that suit the work")
                    out)
            (push (list (nth 0 found) (writing-habit-checks--minute (nth 1 found))
                        (writing-habit-checks--minute (nth 2 found)))
                  taken)
            (push (writing-habit-checks-rescue-create
                   :missed m :offset (nth 0 found) :start (nth 1 found) :end (nth 2 found)
                   :level (nth 3 found))
                  out)
            (cl-incf rescued))))))
    (writing-habit-checks--sort-by
     (lambda (r) (list (writing-habit-checks-slot-offset (writing-habit-checks-rescue-missed r))
                       (writing-habit-checks-slot-start (writing-habit-checks-rescue-missed r))))
     (nreverse out))))

(defun writing-habit-checks-rescue-facts (found)
  "Write the rescue decisions FOUND as the facts the model receives."
  (string-join
   (mapcar (lambda (r)
             (let ((m (writing-habit-checks-rescue-missed r)))
               (if (writing-habit-checks-rescue-offset r)
                   (format "Missed block to make up: %s %s from %s, %d minutes, now proposed for %s, in %s hours for %s work"
                           (writing-habit-checks-slot-category m) (writing-habit-checks-slot-code m)
                           (writing-habit-checks-slot-label m) (writing-habit-checks-slot-minutes m)
                           (writing-habit-checks-rescue-label r)
                           (writing-habit-checks-word (writing-habit-checks-rescue-level r))
                           (writing-habit-checks-slot-category m))
                 (format "Missed block to let go: %s %s from %s, because %s"
                         (writing-habit-checks-slot-category m) (writing-habit-checks-slot-code m)
                         (writing-habit-checks-slot-label m) (writing-habit-checks-rescue-reason r)))))
           found)
   "\n"))

(defun writing-habit-checks-rescue-plain (found)
  "Describe the rescue decisions FOUND without a model."
  (if (null found)
      "No block of this week has been missed so far. Nothing needs making up."
    (string-join
     (cons (format "%d block%s passed with nothing tracked. A missed block is information, not a failure."
                   (length found) (if (= (length found) 1) "" "s"))
           (mapcar (lambda (r)
                     (let ((m (writing-habit-checks-rescue-missed r)))
                       (if (writing-habit-checks-rescue-offset r)
                           (format "  %s %s from %s can be made up on %s."
                                   (capitalize (writing-habit-checks-slot-category m))
                                   (writing-habit-checks-slot-code m) (writing-habit-checks-slot-label m)
                                   (writing-habit-checks-rescue-label r))
                         (format "  Let %s %s from %s go, because %s."
                                 (writing-habit-checks-slot-category m) (writing-habit-checks-slot-code m)
                                 (writing-habit-checks-slot-label m) (writing-habit-checks-rescue-reason r)))))
                   found))
     "\n")))

;;;; The partner summary (Rule 8)

(defun writing-habit-checks--week-overall (db week)
  "Return (PLANNED ACTUAL) of WEEK in DB, or nil."
  (car (sqlite-select db "SELECT planned_min, actual_min FROM v_week_overall WHERE week_start = ?"
                      (list week))))

(defun writing-habit-checks--first-max (key items)
  "Return the first of ITEMS with the largest KEY, as Python's max does."
  (let ((best nil) (best-key nil))
    (dolist (item items)
      (let ((k (funcall key item)))
        (when (or (null best-key) (writing-habit-checks--key< best-key k))
          (setq best item best-key k))))
    best))

(defun writing-habit-checks-personal-bests (db week-start)
  "Return (LABEL VALUE WEEK THIS-WEEK) for each personal best up to WEEK-START."
  (let* ((weeks (sqlite-select db "SELECT week_start, planned_min, actual_min, adherence
 FROM v_week_overall WHERE week_start <= ? ORDER BY week_start" (list week-start)))
         (planned (seq-filter (lambda (w) (and (nth 1 w) (> (nth 1 w) 0) (nth 3 w))) weeks))
         (out '()))
    (when planned
      (let ((best (writing-habit-checks--first-max
                   (lambda (w) (list (min (nth 3 w) 1.0) (nth 2 w))) planned)))
        (push (list "Most of the plan kept" (format "%d%%" (round (* (min (nth 3 best) 1.0) 100)))
                    (nth 0 best) (equal (nth 0 best) week-start))
              out)))
    (when weeks
      (let ((best (writing-habit-checks--first-max (lambda (w) (list (nth 2 w))) weeks)))
        (when (> (nth 2 best) 0)
          (push (list "Most minutes written" (format "%d h %02d min" (/ (nth 2 best) 60) (% (nth 2 best) 60))
                      (nth 0 best) (equal (nth 0 best) week-start))
                out))))
    (let ((run 0) (longest 0) (longest-end nil) (previous nil))
      (dolist (row (sqlite-select db "SELECT day FROM v_day_actual WHERE worked = 1 AND week_start <= ?
 ORDER BY day" (list week-start)))
        (let ((day (writing-habit-checks--abs (car row))))
          (setq run (if (and previous (= (- day previous) 1)) (1+ run) 1))
          (when (> run longest) (setq longest run longest-end day))
          (setq previous day)))
      (when (> longest 0)
        (let* ((weekday (mod (1- (calendar-day-of-week
                                  (calendar-gregorian-from-absolute longest-end)))
                             7))
               (end-week (writing-habit-checks--iso (- longest-end weekday))))
          (push (list "Longest run of writing days"
                      (format "%d day%s" longest (if (= longest 1) "" "s"))
                      end-week (equal end-week week-start))
                out))))
    (when (member "v_week_review" (writing-habit-checks--tables db))
      (let* ((rows (sqlite-select db "SELECT week_start, met, mean_focus, reviews FROM v_week_review
 WHERE week_start <= ?" (list week-start)))
             (met (seq-filter (lambda (r) (and (nth 1 r) (> (nth 1 r) 0))) rows))
             (focused (seq-filter (lambda (r) (and (nth 2 r) (>= (nth 3 r) 3))) rows)))
        (when met
          (let ((best (writing-habit-checks--first-max (lambda (r) (list (nth 1 r))) met)))
            (push (list "Most objectives met" (number-to-string (nth 1 best))
                        (nth 0 best) (equal (nth 0 best) week-start))
                  out)))
        (when focused
          (let ((best (writing-habit-checks--first-max (lambda (r) (list (nth 2 r))) focused)))
            (push (list "Most focused week" (format "%.1f of 5" (nth 2 best))
                        (nth 0 best) (equal (nth 0 best) week-start))
                  out)))))
    (nreverse out)))

(defun writing-habit-checks-partner-week (db week-start &optional today)
  "Collect the numbers of the week starting WEEK-START in DB for a partner.
Return a plist, or nil when the week has no plan."
  (let ((row (writing-habit-checks--week-overall db week-start)))
    (when (and row (nth 0 row) (> (nth 0 row) 0))
      (let* ((planned (nth 0 row)) (actual (or (nth 1 row) 0))
             (counts (list (cons "kept" 0) (cons "partial" 0) (cons "missed" 0) (cons "ahead" 0)))
             (objectives (car (sqlite-select db "SELECT COUNT(*), COALESCE(SUM(done), 0)
 FROM block_objective WHERE week_start = ?" (list week-start))))
             (done (mapcar (lambda (r) (writing-habit-checks--clean (car r)))
                           (sqlite-select db "SELECT text FROM block_objective WHERE week_start = ? AND done = 1
 ORDER BY day, start_time, position, objective_id" (list week-start))))
             (weeks (caar (sqlite-select db "SELECT COUNT(*) FROM v_week_overall
 WHERE planned_min > 0 AND week_start <= ?" (list week-start))))
             (bests (when (>= weeks 2)
                      (seq-take (mapcar (lambda (b) (cons (nth 0 b) (nth 1 b)))
                                        (seq-filter (lambda (b) (nth 3 b))
                                                    (writing-habit-checks-personal-bests db week-start)))
                                2)))
             (monday (writing-habit-checks--abs week-start))
             (before (writing-habit-checks--week-overall db (writing-habit-checks--iso (- monday 7))))
             (after (writing-habit-checks--week-overall db (writing-habit-checks--iso (+ monday 7)))))
        (dolist (block (writing-habit-checks-block-outcomes db week-start today))
          (cl-incf (cdr (assoc (plist-get block :status) counts))))
        (list :week-start week-start :planned planned :actual actual
              :kept-pct (writing-habit-checks--pct actual planned) :blocks counts
              :committed (nth 0 objectives) :met (nth 1 objectives)
              :done (seq-take done 3) :done-total (length done) :bests bests
              :restart (and before (nth 0 before) (> (nth 0 before) 0)
                            (equal (nth 1 before) 0) (> actual 0))
              :next-planned (and after (nth 0 after) (> (nth 0 after) 0) (nth 0 after)))))))

(defun writing-habit-checks-partner-facts (week)
  "Write WEEK, from `writing-habit-checks-partner-week', as facts."
  (let* ((b (plist-get week :blocks))
         (lines (list (format "Week starting: %s" (plist-get week :week-start))
                      (format "Minutes planned: %d (%s)" (plist-get week :planned)
                              (writing-habit-checks--hours (plist-get week :planned)))
                      (format "Minutes written: %d (%s)" (plist-get week :actual)
                              (writing-habit-checks--hours (plist-get week :actual)))
                      (format "Share of the plan kept: %d%%" (plist-get week :kept-pct))
                      (format "Blocks kept: %d" (cdr (assoc "kept" b)))
                      (format "Blocks kept in part: %d" (cdr (assoc "partial" b)))
                      (format "Blocks missed: %d" (cdr (assoc "missed" b)))
                      (format "Blocks still ahead this week: %d" (cdr (assoc "ahead" b))))))
    (when (> (plist-get week :committed) 0)
      (setq lines (append lines (list (format "Objectives committed: %d" (plist-get week :committed))
                                      (format "Objectives met: %d" (plist-get week :met))))))
    (setq lines (append lines
                        (mapcar (lambda (text) (concat "Objective met: " text)) (plist-get week :done))
                        (mapcar (lambda (b) (format "Personal best set this week: %s, %s" (car b) (cdr b)))
                                (plist-get week :bests))))
    (when (plist-get week :restart)
      (setq lines (append lines (list "Back on the plan after a planned week with no writing: yes"))))
    (when (plist-get week :next-planned)
      (setq lines (append lines (list (format "Minutes planned for next week: %d (%s)"
                                              (plist-get week :next-planned)
                                              (writing-habit-checks--hours (plist-get week :next-planned)))))))
    (string-join lines "\n")))

(defun writing-habit-checks--date-words (iso)
  "Write the ISO date as October 12."
  (format "%s %d" (aref writing-habit-checks-months (1- (string-to-number (substring iso 5 7))))
          (string-to-number (substring iso 8 10))))

(defun writing-habit-checks-partner-plain (week)
  "Write the partner summary of WEEK without a model, in the writer's voice."
  (let* ((b (plist-get week :blocks))
         (kept (cdr (assoc "kept" b))) (partial (cdr (assoc "partial" b)))
         (missed (cdr (assoc "missed" b))) (ahead (cdr (assoc "ahead" b)))
         (passed (+ kept partial missed))
         (time (list (format "Here is my writing week of %s." (writing-habit-checks--date-words
                                                               (plist-get week :week-start)))
                     (format "I planned %s of writing and wrote %s, which is %d%% of the plan."
                             (writing-habit-checks--hours (plist-get week :planned))
                             (writing-habit-checks--hours (plist-get week :actual))
                             (plist-get week :kept-pct))))
         (work '()) (more '())
         (met (plist-get week :met)) (committed (plist-get week :committed)))
    (when (> passed 0)
      (setq time (append time (list (concat (format "I kept %d of the %d planned blocks that have passed" kept passed)
                                            (if (> partial 0) (format " and kept %d more in part" partial) "")
                                            ".")))))
    (when (> ahead 0)
      (setq time (append time (list (format "%d more block%s %s still ahead this week."
                                            ahead (if (= ahead 1) "" "s") (if (= ahead 1) "is" "are"))))))
    (cond ((= committed 1)
           (push (if (> met 0) "I met the one objective I committed to."
                   "I did not meet the one objective I committed to.")
                 work))
          ((and (> committed 0) (= met committed))
           (push (format "I met all %d objectives I committed to." committed) work))
          ((> committed 0)
           (push (format "I met %d of the %d objectives I committed to." met committed) work)))
    (when (plist-get week :done)
      (let ((quoted (mapcar (lambda (text) (concat "\"" (writing-habit-checks--rstrip-dot text) "\""))
                            (plist-get week :done))))
        (setq work (append work (list (format "%s %s."
                                              (if (= (length quoted) 1) "The objective I met was"
                                                "Objectives I met include")
                                              (writing-habit-checks--series quoted)))))))
    (when (plist-get week :restart)
      (push "This week put me back on the plan after a week without writing." more))
    (when (plist-get week :bests)
      (let ((marks (mapcar (lambda (b) (format "%s at %s" (downcase (car b)) (cdr b)))
                           (plist-get week :bests))))
        (setq more (append more (list (format "This week set %s for %s."
                                              (if (= (length marks) 1) "a personal best" "personal bests")
                                              (writing-habit-checks--series marks)))))))
    (when (plist-get week :next-planned)
      (setq more (append more (list (format "Next week I have planned %s."
                                            (writing-habit-checks--hours (plist-get week :next-planned)))))))
    (string-join (mapcar (lambda (p) (string-join p " ")) (seq-remove #'null (list time work more)))
                 "\n\n")))

;;;; Semester review

(defun writing-habit-checks--key (name)
  "Name a project by its four-digit number when it has one."
  (if (string-match "\\`[ \t]*\\([0-9]\\{4\\}\\)" (or name ""))
      (match-string 1 name)
    (string-trim (or name ""))))

(defun writing-habit-checks-semester-gather (db name start end &optional completed)
  "Collect the semester NAME from START to END, ISO dates, in DB.
COMPLETED are the descriptions on the completed list.  Return a plist,
or nil when no week of the semester has a plan."
  (let ((planned 0) (actual 0) (weeks 0) (written 0) (months '()))
    (dolist (row (sqlite-select db "SELECT week_start, planned_min, actual_min FROM v_week_overall
 WHERE week_start BETWEEN ? AND ? ORDER BY week_start" (list start end)))
      (let ((p (or (nth 1 row) 0)) (a (or (nth 2 row) 0)))
        (when (> p 0)
          (cl-incf weeks) (when (> a 0) (cl-incf written))
          (cl-incf planned p) (cl-incf actual a)
          (let ((cell (assoc (substring (nth 0 row) 0 7) months)))
            (if cell (setcdr cell (list (+ (nth 1 cell) p) (+ (nth 2 cell) a)))
              (push (list (substring (nth 0 row) 0 7) p a) months))))))
    (when (> weeks 0)
      (let ((totals '()) (committed 0) (met 0) (done '()))
        (dolist (row (sqlite-select db "SELECT p.code, p.description, SUM(s.actual_min) FROM session s
 JOIN project p ON p.project_id = s.project_id WHERE s.day BETWEEN ? AND ? GROUP BY p.project_id"
                                    (list start end)))
          (let* ((project (let ((n (car (writing-habit-name-split-deadline (or (nth 1 row) "")))))
                            (if (string-empty-p n) (nth 0 row) n)))
                 (cell (assoc (writing-habit-checks--key project) totals)))
            (if cell (setcdr cell (list (nth 1 cell) (+ (nth 2 cell) (or (nth 2 row) 0))))
              (push (list (writing-habit-checks--key project) project (or (nth 2 row) 0)) totals))))
        (when (member "block_objective" (writing-habit-checks--tables db))
          (let ((row (car (sqlite-select db "SELECT COUNT(*), COALESCE(SUM(done), 0) FROM block_objective
 WHERE day BETWEEN ? AND ?" (list start end)))))
            (setq committed (nth 0 row) met (nth 1 row)))
          (setq done (mapcar (lambda (r) (writing-habit-checks--clean (car r)))
                             (sqlite-select db "SELECT text FROM block_objective WHERE done = 1 AND day BETWEEN ? AND ?
 ORDER BY day, start_time, position, objective_id" (list start end)))))
        (list :name name :start start :end end :planned planned :actual actual
              :weeks weeks :weeks-written written
              :months (writing-habit-checks--sort-by (lambda (m) (list (car m))) months)
              :projects (writing-habit-checks--sort-by
                         (lambda (p) (list (- (cdr p)) (car p)))
                         (mapcar (lambda (c) (cons (nth 1 c) (nth 2 c)))
                                 (seq-filter (lambda (c) (> (nth 2 c) 0)) (nreverse totals))))
              :completed (mapcar (lambda (c) (car (writing-habit-name-split-deadline c))) completed)
              :committed committed :met met :done done)))))

(defun writing-habit-checks--month (key)
  "Name the month of the YYYY-MM KEY."
  (aref writing-habit-checks-months (1- (string-to-number (substring key 5 7)))))

(defun writing-habit-checks-semester-pct (s)
  "Return the share of the plan kept in semester S."
  (writing-habit-checks--pct (plist-get s :actual) (plist-get s :planned)))

(defun writing-habit-checks-semester-facts (s)
  "Write the semester S as the facts the model receives."
  (let ((lines (list (format "Semester: %s, %s to %s" (plist-get s :name) (plist-get s :start) (plist-get s :end))
                     (format "Weeks with a plan: %d" (plist-get s :weeks))
                     (format "Planned weeks with some writing: %d" (plist-get s :weeks-written))
                     (format "Minutes planned: %d (%s)" (plist-get s :planned)
                             (writing-habit-checks--hours (plist-get s :planned)))
                     (format "Minutes written: %d (%s)" (plist-get s :actual)
                             (writing-habit-checks--hours (plist-get s :actual)))
                     (format "Share of the plan kept: %d%%" (writing-habit-checks-semester-pct s)))))
    (dolist (m (plist-get s :months))
      (setq lines (append lines (list (format "Month %s: %s written of %s planned, %d%% kept"
                                              (writing-habit-checks--month (nth 0 m))
                                              (writing-habit-checks--hours (nth 2 m))
                                              (writing-habit-checks--hours (nth 1 m))
                                              (writing-habit-checks--pct (nth 2 m) (nth 1 m)))))))
    (dolist (p (seq-take (plist-get s :projects) 3))
      (setq lines (append lines (list (format "Project with the most time: %s, %s"
                                              (car p) (writing-habit-checks--hours (cdr p)))))))
    (setq lines (append lines (mapcar (lambda (p) (concat "Project completed: " p))
                                      (plist-get s :completed))))
    (when (> (plist-get s :committed) 0)
      (setq lines (append lines (list (format "Objectives committed: %d" (plist-get s :committed))
                                      (format "Objectives met: %d" (plist-get s :met))))))
    (setq lines (append lines (mapcar (lambda (d) (concat "Objective met: " d))
                                      (seq-take (plist-get s :done) 3))))
    (string-join lines "\n")))

(defun writing-habit-checks-semester-plain (s)
  "Write the narrative of semester S without a model, in the writer's voice."
  (let* ((weeks (plist-get s :weeks))
         (time (list (format "During %s, I planned %s of writing over %d week%s and wrote %s, which is %d%% of the plan."
                             (plist-get s :name) (writing-habit-checks--hours (plist-get s :planned))
                             weeks (if (= weeks 1) "" "s")
                             (writing-habit-checks--hours (plist-get s :actual))
                             (writing-habit-checks-semester-pct s))))
         (rated (mapcar (lambda (m) (cons (nth 0 m) (writing-habit-checks--pct (nth 2 m) (nth 1 m))))
                        (seq-filter (lambda (m) (> (nth 1 m) 0)) (plist-get s :months))))
         (work '()))
    (when (< (plist-get s :weeks-written) weeks)
      (setq time (append time (list (format "I wrote in %d of those %d weeks."
                                            (plist-get s :weeks-written) weeks)))))
    (when (>= (length rated) 2)
      (let ((best (writing-habit-checks--first-max (lambda (r) (list (cdr r) (car r))) rated))
            (worst (writing-habit-checks--first-max (lambda (r) (list (- (cdr r)) (car r))) (reverse rated))))
        (unless (= (cdr best) (cdr worst))
          (setq time (append time (list (format "The share kept was highest in %s, at %d%%, and lowest in %s, at %d%%."
                                                (writing-habit-checks--month (car best)) (cdr best)
                                                (writing-habit-checks--month (car worst)) (cdr worst))))))))
    (when (plist-get s :projects)
      (push (format "Most of the time went to %s."
                    (writing-habit-checks--series
                     (mapcar (lambda (p) (format "%s (%s)" (car p) (writing-habit-checks--hours (cdr p))))
                             (seq-take (plist-get s :projects) 3))))
            work))
    (setq work (append work (list (if (plist-get s :completed)
                                      (format "I completed %s." (writing-habit-checks--series (plist-get s :completed)))
                                    "No project was marked completed this semester."))))
    (when (> (plist-get s :committed) 0)
      (setq work (append work (list (format "I met %d of the %d block objectives I committed to."
                                            (plist-get s :met) (plist-get s :committed))))))
    (when (plist-get s :done)
      (let ((quoted (mapcar (lambda (d) (concat "\"" (writing-habit-checks--rstrip-dot d) "\""))
                            (seq-take (plist-get s :done) 3))))
        (setq work (append work (list (format "%s %s."
                                              (if (= (length quoted) 1) "The objective I met was"
                                                "Objectives I met include")
                                              (writing-habit-checks--series quoted)))))))
    (string-join (list (string-join time " ") (string-join work " ")) "\n\n")))

;;;; Commands

(defun writing-habit-checks--db-path ()
  "Return the habit database to read, asking when none is known."
  (or (and (boundp 'writing-habit-dispatch--last-db) writing-habit-dispatch--last-db)
      (and (boundp 'writing-habit-default-db) writing-habit-default-db)
      (read-file-name "Habit database: " nil nil t)))

(defmacro writing-habit-checks--with-db (var &rest body)
  "Open the habit database as VAR around BODY."
  (declare (indent 1))
  `(let ((,var (writing-habit-db-connect (writing-habit-checks--db-path))))
     (unwind-protect (progn ,@body) (writing-habit-db-close ,var))))

(defun writing-habit-checks-week-monday (table)
  "Return the ISO Monday of TABLE's week from its file name, or ask."
  (let* ((stem (and (writing-habit-table-path table)
                    (file-name-base (writing-habit-table-path table))))
         (months '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))
         (today (writing-habit-checks--abs (writing-habit-checks--today))))
    (or (when (and stem (string-match "\\`\\([A-Za-z]+\\)[-_ ]?\\([0-9]\\{1,2\\}\\)\\'" stem))
          (let ((month (cl-position (downcase (substring (match-string 1 stem) 0 (min 3 (length (match-string 1 stem)))))
                                    months :test #'equal))
                (day (string-to-number (match-string 2 stem)))
                (year (string-to-number (format-time-string "%Y")))
                best)
            (when month
              (dolist (y (list (1- year) year (1+ year)))
                (when (<= 1 day (calendar-last-day-of-month (1+ month) y))
                  (let ((abs (calendar-absolute-from-gregorian (list (1+ month) day y))))
                    (when (or (null best) (< (abs (- abs today)) (abs (- best today))))
                      (setq best abs)))))
              (and best (writing-habit-checks--iso best)))))
        (read-string "Monday of this week (YYYY-MM-DD): "))))

(defun writing-habit-checks--lists (table names)
  "Return (LIST-NAME CODE DESCRIPTION) for every entry of the NAMES lists of TABLE."
  (cl-loop for name in names
           append (mapcar (lambda (e) (list name (writing-habit-lists-entry-code e)
                                            (writing-habit-lists-entry-description e)))
                          (writing-habit-table-list-entries table name))))

(defun writing-habit-checks--cell-value (category code)
  "Return the cell text for CODE with the letter of CATEGORY."
  (concat (or (cdr (assoc category writing-habit-table-category-letter)) "") code))

(defun writing-habit-checks--note (task facts plain week)
  "Word FACTS of TASK with the model when allowed, logging under WEEK.
Return the note plist of `writing-habit-coach-note'."
  (if (writing-habit-coach-allowed-p)
      (writing-habit-checks--with-db db
        (writing-habit-coach-note task facts plain db week))
    (writing-habit-coach-note task facts plain)))

;;;###autoload
(defun writing-habit-checks-coverage ()
  "Find projects due soon with no block left this week, and offer a block for each."
  (interactive)
  (let* ((table (writing-habit-table-current))
         (monday (writing-habit-checks-week-monday table))
         (legend (writing-habit-checks--legend table))
         (lists (writing-habit-checks--lists
                 table (seq-remove (lambda (n) (equal n writing-habit-lists-completed))
                                   (mapcar #'car writing-habit-lists))))
         (result (writing-habit-checks-coverage-check
                  legend lists (writing-habit-checks-slots table) monday (writing-habit-checks--today)))
         (plain (writing-habit-checks-coverage-plain result)))
    (if (null (writing-habit-checks-coverage-risks result))
        (message "%s" plain)
      (let* ((note (writing-habit-checks--note "coverage" (writing-habit-checks-coverage-facts result)
                                               plain monday))
             (risks (seq-filter #'writing-habit-checks-risk-donor (writing-habit-checks-coverage-risks result)))
             (chosen (funcall writing-habit-coach-chooser "Deadline coverage" (plist-get note :text)
                              (mapcar #'writing-habit-checks-coverage-line risks))))
        (writing-habit-checks-apply-coverage table (mapcar (lambda (i) (nth i risks)) chosen))))))

(defun writing-habit-checks-apply-coverage (table risks)
  "Give each of RISKS its proposed block in TABLE and show the result."
  (let ((done 0))
    (dolist (risk risks)
      (let* ((p (writing-habit-checks-risk-project risk))
             (d (writing-habit-checks-risk-donor risk))
             (code (and (writing-habit-checks-project-in-legend p) (writing-habit-checks-project-code p))))
        (unless code
          (let ((entry (seq-find (lambda (e) (equal (writing-habit-lists-entry-project e)
                                                    (writing-habit-checks-project-name p)))
                                 (writing-habit-table-list-entries table (writing-habit-checks-project-source p)))))
            (when entry
              (let ((at (writing-habit-table-copy-entry-to-legend table entry)))
                (setq code (car (writing-habit-table-row-parsed (writing-habit-table-row-at table at))))
                ;; A legend row inserted above the grid would shift the rows.
                (setq d (seq-find (lambda (s) (and (= (writing-habit-checks-slot-offset s) (writing-habit-checks-slot-offset d))
                                                   (equal (writing-habit-checks-slot-start s) (writing-habit-checks-slot-start d))
                                                   (equal (writing-habit-checks-slot-code s) (writing-habit-checks-slot-code d))))
                                  (writing-habit-checks-slots table)))))))
        (when (and code d)
          (writing-habit-table-set-cell table (writing-habit-checks-slot-row d) (writing-habit-checks-slot-column d)
                                        (writing-habit-checks--cell-value (writing-habit-checks-slot-category d) code))
          (cl-incf done))))
    (when (> done 0) (writing-habit-table--apply table))
    done))

;;;###autoload
(defun writing-habit-checks-energy ()
  "Find the blocks whose hours do not suit their kind of writing, and offer swaps."
  (interactive)
  (let* ((table (writing-habit-table-current))
         (monday (writing-habit-checks-week-monday table))
         (start (writing-habit-checks--abs monday))
         (today (writing-habit-checks--abs (writing-habit-checks--today)))
         (profile (writing-habit-checks-profile))
         (slots (writing-habit-checks-slots table))
         (found (writing-habit-checks-misfits profile slots))
         (proposed (writing-habit-checks-swaps
                    profile slots (lambda (s) (>= (+ start (writing-habit-checks-slot-offset s)) today))))
         (plain (writing-habit-checks-energy-plain profile found proposed)))
    (if (null found)
        (message "%s" plain)
      (let* ((note (writing-habit-checks--note "energy" (writing-habit-checks-energy-facts profile found proposed)
                                               plain monday))
             (chosen (funcall writing-habit-coach-chooser "Energy" (plist-get note :text)
                              (mapcar #'writing-habit-checks-swap-line proposed))))
        (writing-habit-checks-apply-swaps table (mapcar (lambda (i) (nth i proposed)) chosen))))))

(defun writing-habit-checks-apply-swaps (table swaps)
  "Exchange the work of each pair of blocks in SWAPS in TABLE."
  (dolist (w swaps)
    (let* ((a (nth 0 w)) (b (nth 1 w))
           (one (writing-habit-table-cell table (writing-habit-checks-slot-row a) (writing-habit-checks-slot-column a)))
           (two (writing-habit-table-cell table (writing-habit-checks-slot-row b) (writing-habit-checks-slot-column b))))
      (writing-habit-table-set-cell table (writing-habit-checks-slot-row a) (writing-habit-checks-slot-column a)
                                    (writing-habit-checks--cell-value (writing-habit-checks-slot-category b)
                                                                      (writing-habit-checks-slot-code b)))
      (writing-habit-table-set-cell table (writing-habit-checks-slot-row b) (writing-habit-checks-slot-column b)
                                    (writing-habit-checks--cell-value (writing-habit-checks-slot-category a)
                                                                      (writing-habit-checks-slot-code a)))
      (ignore one two)))
  (when swaps (writing-habit-table--apply table))
  (length swaps))

;;;###autoload
(defun writing-habit-checks-energy-show ()
  "Show the energy profile as a grid of hours, H high, M medium, and L low."
  (interactive)
  (let ((profile (writing-habit-checks-profile)))
    (with-current-buffer (get-buffer-create "*writing-habit energy*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Energy profile from " (abbreviate-file-name writing-habit-checks-energy-file) "\n\n")
        (insert (format "%-11s" "Hour"))
        (dotimes (h 24) (insert (format "%3d" h)))
        (insert "\n")
        (dolist (activity writing-habit-checks-activities)
          (insert (format "%-11s" activity))
          (seq-doseq (level (cdr (assoc activity profile)))
            (insert (format "%3s" (aref ["L" "M" "H"] level))))
          (insert "\n")))
      (special-mode)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun writing-habit-checks-energy-from-reviews ()
  "Set the energy profile from the focus ratings of reviewed blocks."
  (interactive)
  (let* ((result (writing-habit-checks--with-db db
                   (writing-habit-checks-profile-from-reviews db (writing-habit-checks-profile))))
         (changes (cdr result)))
    (if (null changes)
        (message "No hour has %d focus ratings that differ from the profile."
                 writing-habit-checks-min-reviews)
      (when (y-or-n-p (format "Change %d hour%s of the profile? " (length changes)
                              (if (= (length changes) 1) "" "s")))
        (with-temp-file writing-habit-checks-energy-file
          (insert (writing-habit-checks-profile-json (car result))))
        (message "Saved the energy profile to %s" writing-habit-checks-energy-file)))))

;;;###autoload
(defun writing-habit-checks-rescue ()
  "Offer a slot to make up each missed block worth rescuing, and let the rest go."
  (interactive)
  (let* ((table (writing-habit-table-current))
         (monday (writing-habit-checks-week-monday table))
         (today (writing-habit-checks--today))
         (now (cons today (writing-habit-checks--minute (format-time-string "%H:%M"))))
         (slots (writing-habit-checks-slots table))
         (outcomes (writing-habit-checks--with-db db
                     (writing-habit-checks-block-outcomes db monday today)))
         (profile (writing-habit-checks-profile))
         (missed (writing-habit-checks-missed-blocks slots outcomes monday now))
         (days (sort (seq-uniq (mapcar #'cadr (writing-habit-table-columns table))) #'<))
         (found (writing-habit-checks-rescue-plan slots missed profile days monday now
                                                  (writing-habit-checks--legend table)))
         (plain (writing-habit-checks-rescue-plain found)))
    (if (null found)
        (message "%s" plain)
      (let* ((note (writing-habit-checks--note "rescue" (writing-habit-checks-rescue-facts found) plain monday))
             (rescues (seq-filter #'writing-habit-checks-rescue-offset found))
             (chosen (funcall writing-habit-coach-chooser "Missed blocks" (plist-get note :text)
                              (mapcar #'writing-habit-checks-rescue-line rescues))))
        (writing-habit-checks-apply-rescues table (mapcar (lambda (i) (nth i rescues)) chosen))))))

(defun writing-habit-checks-place-block (table offset start end value)
  "Write VALUE for day OFFSET at START to END in TABLE, adding a row when needed.
Return the row index."
  (let* ((column (car (seq-find (lambda (c) (= (nth 1 c) offset)) (writing-habit-table-columns table))))
         (rows (writing-habit-table-block-rows table))
         (row (seq-find (lambda (i) (equal (writing-habit-table-row-parsed (writing-habit-table-row-at table i))
                                           (cons start end)))
                        rows)))
    (unless row
      (let* ((first (writing-habit-checks--minute start))
             (before (seq-filter (lambda (i) (<= (writing-habit-checks--minute
                                                  (car (writing-habit-table-row-parsed (writing-habit-table-row-at table i))))
                                                 first))
                                 rows)))
        (setq row (if before
                      (writing-habit-table-insert-block table (car (last before)) nil start end)
                    (writing-habit-table-insert-block table (car rows) t start end)))))
    (writing-habit-table-set-cell table row column value)
    row))

(defun writing-habit-checks-apply-rescues (table rescues)
  "Place a make-up block in TABLE for each of RESCUES."
  (dolist (r rescues)
    (let ((m (writing-habit-checks-rescue-missed r)))
      (writing-habit-checks-place-block
       table (writing-habit-checks-rescue-offset r) (writing-habit-checks-rescue-start r)
       (writing-habit-checks-rescue-end r)
       (writing-habit-checks--cell-value (writing-habit-checks-slot-category m) (writing-habit-checks-slot-code m)))))
  (when rescues (writing-habit-table--apply table))
  (length rescues))

;;;###autoload
(defun writing-habit-checks-partner (&optional week-start)
  "Draft the summary of WEEK-START for an accountability partner (Rule 8).
Nothing is sent.  Edit the draft and copy it with \\`C-c C-c'."
  (interactive)
  (let* ((week-start (or week-start
                         (writing-habit-checks-week-monday (writing-habit-table-current))))
         (allowed (writing-habit-coach-allowed-p)))
    (writing-habit-checks--with-db db
      (let ((week (writing-habit-checks-partner-week db week-start)))
        (if (null week)
            (message "The week has no plan in the habit database yet, so there is nothing to summarize.")
          (let* ((pct (format "%d%%" (plist-get week :kept-pct)))
                 (note (writing-habit-coach-note
                        "partner" (writing-habit-checks-partner-facts week)
                        (writing-habit-checks-partner-plain week) (and allowed db) week-start
                        (lambda (text) (unless (string-search pct text)
                                         "the draft left out the share of the plan kept")))))
            (writing-habit-coach-show-draft
             "Summary for your writing partner"
             "Edit it, then copy it into a message." (plist-get note :text))
            note))))))

;;;###autoload
(defun writing-habit-checks-semester (&optional name start end)
  "Draft a narrative of the semester NAME from START to END for an annual report.
Without arguments, `writing-habit-checks-semester' names the semester,
or the last sixteen weeks stand in for it.  Run it before rolling the
lists forward, while the completed list still holds the semester's work."
  (interactive)
  (let* ((today (writing-habit-checks--today))
         (span (or (and name (list name start end)) writing-habit-checks-semester
                   (list "this semester" (writing-habit-checks--iso (- (writing-habit-checks--abs today) 112))
                         today)))
         (completed (condition-case nil
                        (mapcar #'writing-habit-lists-entry-description
                                (writing-habit-table-list-entries (writing-habit-table-current)
                                                                  writing-habit-lists-completed))
                      (error nil)))
         (allowed (writing-habit-coach-allowed-p)))
    (writing-habit-checks--with-db db
      (let ((s (writing-habit-checks-semester-gather db (nth 0 span) (nth 1 span)
                                                     (if (string< today (nth 2 span)) today (nth 2 span))
                                                     completed)))
        (if (null s)
            (message "No week of %s has a plan in the habit database, so there is no semester to review."
                     (nth 0 span))
          (let* ((pct (format "%d%%" (writing-habit-checks-semester-pct s)))
                 (note (writing-habit-coach-note
                        "semester" (writing-habit-checks-semester-facts s)
                        (writing-habit-checks-semester-plain s) (and allowed db) nil
                        (lambda (text) (unless (string-search pct text)
                                         "the draft left out the share of the plan kept")))))
            (writing-habit-coach-show-draft
             (format "Semester review of %s" (nth 0 span))
             "Edit it, then copy it into your annual report." (plist-get note :text))
            note))))))

(provide 'writing-habit-checks)
;;; writing-habit-checks.el ends here
