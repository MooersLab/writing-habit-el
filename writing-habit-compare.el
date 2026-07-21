;;; writing-habit-compare.el --- Read the comparison views -*- lexical-binding: t; -*-

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

;; This is part of Phase 5 of the Emacs Lisp port of the Python
;; writing-habit package.  It ports compare/queries.py: thin wrappers over
;; the four comparison views defined in schema.sql, plus the streak count.
;; The module never writes its own aggregation SQL, so the schema stays the
;; single source of truth.
;;
;; Each wrapper returns a list of rows, and each row is an alist of column
;; name to value, so the report module can read fields by name.
;;
;; Public functions:
;;   `writing-habit-compare-week-project'   planned versus actual per project
;;   `writing-habit-compare-week-category'  planned versus actual per activity
;;   `writing-habit-compare-week-barbell'   planned versus actual per risk class
;;   `writing-habit-compare-day-actual'     actual minutes per day
;;   `writing-habit-compare-current-streak' run of consecutive worked days

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'writing-habit-db)

(defconst writing-habit-compare--project-columns
  '("week_start" "code" "description" "risk_class"
    "planned_min" "actual_min" "diff_min" "adherence")
  "Column order of v_week_project.")

(defconst writing-habit-compare--category-columns
  '("week_start" "category" "planned_min" "actual_min")
  "Column order of v_week_category.")

(defconst writing-habit-compare--barbell-columns
  '("week_start" "risk_class" "planned_min" "actual_min")
  "Column order of v_week_barbell.")

(defconst writing-habit-compare--day-columns
  '("day" "week_start" "actual_min" "worked")
  "Column order of v_day_actual.")


;;;; Date helpers (calendar math, so no timezone surprises)

(defun writing-habit-compare--iso-abs (iso)
  "Return the absolute day number for the ISO date string ISO."
  (let ((p (mapcar #'string-to-number (split-string iso "-"))))
    (calendar-absolute-from-gregorian (list (nth 1 p) (nth 2 p) (nth 0 p)))))

(defun writing-habit-compare--monday (week)
  "Return the ISO Monday of the week containing the ISO date WEEK.
This matches the week_start generated column in the schema."
  (let* ((p (mapcar #'string-to-number (split-string week "-")))
         (greg (list (nth 1 p) (nth 2 p) (nth 0 p)))     ; (month day year)
         (dow (calendar-day-of-week greg))               ; 0 Sunday .. 6 Saturday
         (monday-abs (- (calendar-absolute-from-gregorian greg)
                        (if (= dow 0) 6 (1- dow))))
         (mg (calendar-gregorian-from-absolute monday-abs)))
    (format "%04d-%02d-%02d" (nth 2 mg) (nth 0 mg) (nth 1 mg))))


;;;; View wrappers

(defun writing-habit-compare--rows (db sql params columns)
  "Run SQL with PARAMS on DB and zip each result row with COLUMNS into an alist."
  (mapcar (lambda (row) (cl-mapcar #'cons columns row))
          (sqlite-select db sql params)))

(defun writing-habit-compare-week-project (db week)
  "Planned versus actual minutes and adherence per project for WEEK."
  (writing-habit-compare--rows
   db "SELECT * FROM v_week_project WHERE week_start = ? ORDER BY code"
   (list (writing-habit-compare--monday week))
   writing-habit-compare--project-columns))

(defun writing-habit-compare-week-category (db week)
  "Planned versus actual minutes per activity for WEEK."
  (writing-habit-compare--rows
   db "SELECT * FROM v_week_category WHERE week_start = ?"
   (list (writing-habit-compare--monday week))
   writing-habit-compare--category-columns))

(defun writing-habit-compare-week-barbell (db week)
  "Planned versus actual minutes per risk class for WEEK."
  (writing-habit-compare--rows
   db "SELECT * FROM v_week_barbell WHERE week_start = ? ORDER BY risk_class"
   (list (writing-habit-compare--monday week))
   writing-habit-compare--barbell-columns))

(defun writing-habit-compare-day-actual (db week)
  "Actual minutes and worked flag per day for WEEK."
  (writing-habit-compare--rows
   db "SELECT * FROM v_day_actual WHERE week_start = ? ORDER BY day"
   (list (writing-habit-compare--monday week))
   writing-habit-compare--day-columns))

(defun writing-habit-compare-current-streak (db)
  "Return the length of the run of consecutive worked days ending at the latest.
Zero when no day has any recorded minutes."
  (let* ((rows (sqlite-select
                db "SELECT day FROM v_day_actual WHERE worked = 1 ORDER BY day"))
         (days (mapcar #'car rows)))
    (if (null days)
        0
      (let ((rev (reverse days))                 ; latest first
            (streak 1)
            (stop nil))
        (while (and (cdr rev) (not stop))
          (let ((later (writing-habit-compare--iso-abs (car rev)))
                (earlier (writing-habit-compare--iso-abs (cadr rev))))
            (if (= 1 (- later earlier))
                (setq streak (1+ streak))
              (setq stop t)))
          (setq rev (cdr rev)))
        streak))))

(provide 'writing-habit-compare)
;;; writing-habit-compare.el ends here
