;;; writing-habit-context.el --- Event-context tags on a week -*- lexical-binding: t; -*-

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

;; The twin of the Python context.py.  A context tag marks a week with the kind
;; of period it was, for example a national meeting, a teaching block, or a
;; data-collection push.  The second dashboard groups adherence by tag.  A week
;; may carry several tags.
;;
;; Public functions:
;;   `writing-habit-context-set'    attach a tag to a week
;;   `writing-habit-context-clear'  remove a tag, or all tags, from a week
;;   `writing-habit-context-list'   list context tags

;;; Code:

(require 'writing-habit-db)
(require 'writing-habit-compare)

(declare-function org-read-date "org" (&rest args))

(defun writing-habit-context-set (db week tag &optional note)
  "Attach TAG, with an optional NOTE, to the week containing WEEK in DB."
  (sqlite-execute
   db "INSERT OR REPLACE INTO week_context(week_start, tag, note) VALUES (?, ?, ?)"
   (list (writing-habit-compare--monday week) tag note)))

(defun writing-habit-context-clear (db week &optional tag)
  "Remove TAG from the week containing WEEK, or every tag when TAG is nil.
Return the number of rows removed."
  (let ((monday (writing-habit-compare--monday week)))
    (if tag
        (sqlite-execute db "DELETE FROM week_context WHERE week_start = ? AND tag = ?"
                        (list monday tag))
      (sqlite-execute db "DELETE FROM week_context WHERE week_start = ?" (list monday)))))

(defun writing-habit-context-list (db &optional week)
  "Return (WEEK-START TAG NOTE) rows from DB, optionally limited to one WEEK."
  (if week
      (sqlite-select
       db "SELECT week_start, tag, note FROM week_context WHERE week_start = ? ORDER BY tag"
       (list (writing-habit-compare--monday week)))
    (sqlite-select
     db "SELECT week_start, tag, note FROM week_context ORDER BY week_start, tag")))

;;;###autoload
(defun writing-habit-context-set-interactive (db-file week tag note)
  "Tag the week of WEEK with TAG, and an optional NOTE, in the database DB-FILE."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (org-read-date nil nil nil "Any date in the target week: ")
         (read-string "Tag: ")
         (read-string "Note (optional): ")))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (progn
          (writing-habit-context-set db week tag
                                     (unless (string-empty-p note) note))
          (message "Tagged the week of %s with %s" week tag))
      (writing-habit-db-close db))))

(provide 'writing-habit-context)
;;; writing-habit-context.el ends here
