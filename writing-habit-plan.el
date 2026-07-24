;;; writing-habit-plan.el --- Load a weekly plan through the writing-schedule parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit

;; writing-schedule.el is required at runtime for plan import only, and is
;; loaded lazily inside the entry points, so it is not a hard dependency of
;; the package as a whole.  The other stages run without it.

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT license.

;;; Commentary:

;; This is Phase 3 of the Emacs Lisp port of the Python writing-habit
;; package.  It ports `plan_import.py'.
;;
;; The whole point of this stage is that it does not re-parse the org
;; table.  It calls the parser already inside writing-schedule.el, the same
;; parser the schedule module uses, so the plan and the schedule can never
;; drift into two dialects.  writing-schedule.el is required lazily inside
;; the entry points, so the rest of the toolkit (db, track, compare, name)
;; runs without it loaded.
;;
;; The functions reused from writing-schedule.el are currently internal
;; (double-dash) names: `writing-schedule--parse',
;; `writing-schedule--week-monday', and `writing-schedule--iso-date'.
;; Reusing them keeps a single source of truth rather than copying the
;; parser.  When writing-schedule.el promotes a public parse entry point,
;; this file should switch to it; the plan document tracks that as an
;; upstream follow-up.
;;
;; Risk tags.  The writing-schedule legend maps a code to a description,
;; for example "A: DNPH1 docking".  To give the barbell view a class to
;; group on, add a risk tag at the end of the description in either the
;; org-tag form :safe: or the parenthesis form (safe).  The two risk classes
;; are safe and speculative.  Support is an activity category, not a risk
;; class, so a legacy support tag is stripped but records no risk class.  The
;; tag is stripped before the description is stored, so it never pollutes the
;; project name.
;;
;; Public functions:
;;   `writing-habit-plan-import'        parse a table and load a week's blocks
;;   `writing-habit-plan-parse-file'    return the writing-schedule parse plist
;;   `writing-habit-plan-import-file'   interactive wrapper over a database file

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'writing-habit-db)

(declare-function writing-schedule--parse "writing-schedule" (table))
(declare-function writing-schedule--week-monday "writing-schedule" (time))
(declare-function writing-schedule--iso-date "writing-schedule" (abs))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function org-read-date "org" (&rest args))

;; writing-schedule section header -> our activity category.
(defconst writing-habit-plan--section-to-category
  '(("generative" . "generative")
    ("generating" . "generative")
    ("writing" . "generative")          ; the writing-schedule default section name
    ("rewriting" . "editing")
    ("editing" . "editing")
    ("revising" . "editing")
    ("revision" . "editing")
    ("supporting" . "support")
    ("support" . "support"))
  "Map a lower-case writing-schedule section header to an activity category.")

(defconst writing-habit-plan--risk-tag-re
  "\\(?:(\\(safe\\|speculative\\|support\\))\\|:\\(safe\\|speculative\\|support\\):\\)[ \t]*\\'"
  "Match a trailing risk tag in either the (safe) or :safe: form.")

(defun writing-habit-plan--split-risk (description)
  "Return a cons (CLEAN-DESC . RISK) from a legend DESCRIPTION.
CLEAN-DESC is nil when the description is empty after the tag is removed.
RISK is a lowercase string, one of \"safe\" or \"speculative\", or nil.
Support is an activity category, not a risk class."
  (if (or (null description) (string-empty-p (string-trim description)))
      (cons nil nil)
    (let ((case-fold-search t))
      (if (string-match writing-habit-plan--risk-tag-re description)
          (let* ((tag (downcase (or (match-string 1 description)
                                    (match-string 2 description))))
                 ;; Only safe and speculative are risk classes; a legacy support
                 ;; tag is stripped but records no risk class.
                 (risk (and (member tag '("safe" "speculative")) tag))
                 (clean (string-trim
                         (replace-regexp-in-string
                          writing-habit-plan--risk-tag-re "" description))))
            (cons (if (string-empty-p clean) nil clean) risk))
        (let ((clean (string-trim description)))
          (cons (if (string-empty-p clean) nil clean) nil))))))

(defun writing-habit-plan--category-for (section)
  "Return a cons (CATEGORY . KNOWN) for a writing-schedule SECTION header.
KNOWN is nil when SECTION was not recognized, in which case CATEGORY
falls back to \"generative\"."
  (let ((name (cdr (assoc (downcase (string-trim (or section "")))
                          writing-habit-plan--section-to-category))))
    (if name (cons name t) (cons "generative" nil))))

(defun writing-habit-plan--schedule-code (path)
  "Derive the schedule file-name code from a table PATH.
Strips the directory and the .org extension, and an optional leading ISO date
prefix, so 2026-01-19_4gAAeAsA-gWW.org yields 4gAAeAsA-gWW and my-week.org yields
my-week."
  (let ((stem (file-name-base path)))
    (if (string-match "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[_-]\\(.+\\)\\'" stem)
        (match-string 1 stem)
      stem)))

(defun writing-habit-plan-parse-file (path)
  "Return the writing-schedule parse plist for the org table in PATH.
The plist keys are :events, :legend, :letters, and :columns.  Signal an
error when writing-schedule.el is not available or PATH has no table."
  (unless (require 'writing-schedule nil t)
    (error "Plan import needs writing-schedule.el on the load-path"))
  (with-temp-buffer
    (insert-file-contents path)
    (org-mode)
    (goto-char (point-min))
    (unless (re-search-forward "^[ \t]*|" nil t)
      (error "No org table found in %s" path))
    (writing-schedule--parse (org-table-to-lisp))))

(defun writing-habit-plan-import (db path week)
  "Parse the weekly table at PATH for the week containing WEEK, load it into DB.
WEEK is any date inside the target week, as an ISO string such as
\"2026-01-19\"; it snaps to the Monday.  Return the number of plan_block
rows for that week.  Rows are inserted with INSERT OR IGNORE, so a second
import of the same week is a no-op."
  (unless (require 'writing-schedule nil t)
    (error "Plan import needs writing-schedule.el on the load-path"))
  (let* ((parsed (writing-habit-plan-parse-file path))
         (events (plist-get parsed :events))
         (legend (plist-get parsed :legend))
         (monday-abs (writing-schedule--week-monday (org-read-date nil t week)))
         (monday-iso (writing-schedule--iso-date monday-abs))
         (unknown '()))
    (dolist (ev events)
      (let* ((letter (plist-get ev :letter))
             (split (writing-habit-plan--split-risk (cdr (assoc letter legend))))
             (desc (car split))
             (risk (cdr split))
             (pid (writing-habit-db-get-or-create-project db letter desc risk))
             (cat-known (writing-habit-plan--category-for (plist-get ev :section)))
             (cat-id (writing-habit-db-get-category-id db (car cat-known)))
             (day (writing-schedule--iso-date (+ monday-abs (plist-get ev :offset)))))
        (unless (cdr cat-known)
          (cl-pushnew (plist-get ev :section) unknown :test #'equal))
        (sqlite-execute
         db (concat "INSERT OR IGNORE INTO plan_block"
                    "(day, start_time, end_time, project_id, category_id)"
                    " VALUES (?, ?, ?, ?, ?)")
         (list day (plist-get ev :start) (plist-get ev :end) pid cat-id))))
    ;; Record which schedule file produced this week, for the grouping dashboard.
    (sqlite-execute
     db (concat "INSERT OR REPLACE INTO plan_week(week_start, schedule_code, table_path)"
                " VALUES (?, ?, ?)")
     (list monday-iso (writing-habit-plan--schedule-code path) path))
    (let ((count (caar (sqlite-select
                        db "SELECT COUNT(*) FROM plan_block WHERE week_start = ?"
                        (list monday-iso)))))
      (writing-habit-db-log-import db "org" path (length events) count nil "plan import")
      (when unknown
        (message "warning: sections mapped to generative by default: %s"
                 (mapconcat #'identity (sort unknown #'string<) ", ")))
      count)))

;;;###autoload
(defun writing-habit-plan-import-file (db-file table week)
  "Import the weekly TABLE for WEEK into the database at DB-FILE.
DB-FILE must already hold the schema; run `writing-habit-db-init' first.
Interactively, prompt for each argument.  Report and return the count of
planned blocks for that week."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (read-file-name "Weekly table (org): " nil nil t)
         (org-read-date nil nil nil "Any date in the target week: ")))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (let ((n (writing-habit-plan-import db table week)))
          (message "Imported %d planned blocks from %s" n table)
          n)
      (writing-habit-db-close db))))

(provide 'writing-habit-plan)
;;; writing-habit-plan.el ends here
