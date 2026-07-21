;;; writing-habit-db.el --- SQLite data layer for writing-habit -*- lexical-binding: t; -*-

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

;; This is Phase 2 of the Emacs Lisp port of the Python writing-habit
;; package.  It ports `db.py', the single contract between the three
;; modules, onto the SQLite interface built into Emacs 29.1 and later.
;;
;; The schema itself is not reimplemented.  `writing-habit-db-init' reads
;; the same schema.sql that the Python package uses, so a database created
;; here is byte-for-byte the same as one created by Python, and either port
;; can read what the other wrote.  This is the interoperability contract.
;;
;; Emacs runs only the first statement of a multi-statement string through
;; `sqlite-execute', so `writing-habit-db-init' splits the schema into
;; statements first.  The schema's string literals contain no semicolons,
;; so a simple split after removing line comments is safe.
;;
;; Public functions, matching their Python counterparts:
;;   `writing-habit-db-connect'             open a connection, foreign keys on
;;   `writing-habit-db-init'                load the schema, seed activities
;;   `writing-habit-db-get-category-id'     name -> category id or nil
;;   `writing-habit-db-get-or-create-project'  legend code -> project id
;;   `writing-habit-db-log-import'          record an import's provenance
;;   `writing-habit-db-minutes-between'     minutes between two HH:MM times
;;   `writing-habit-db-close'               close a connection

;;; Code:

(require 'subr-x)

(defgroup writing-habit nil
  "Track and compare planned versus actual academic writing effort."
  :group 'org
  :prefix "writing-habit-")

(defconst writing-habit-db--load-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory this file was loaded from, used to locate the shared schema.")

(defcustom writing-habit-db-schema-file
  (let ((root (locate-dominating-file writing-habit-db--load-dir "schema.sql")))
    (expand-file-name "schema.sql" (or root writing-habit-db--load-dir)))
  "Path to the shared schema.sql, the single contract between the modules.
Located by walking up from this file's directory, so it works whether
schema.sql sits beside the modules, as in the installed package, or one
level up, as in the development layout."
  :type 'file
  :group 'writing-habit)


;;;; Connections

(defun writing-habit-db-connect (&optional db-path)
  "Open a SQLite connection with foreign keys enforced and return it.
With DB-PATH nil or the string \":memory:\", open an in-memory database.
Signal an error when this Emacs was built without SQLite support."
  (unless (sqlite-available-p)
    (error "This Emacs was built without SQLite support; cannot use writing-habit-db"))
  (let ((db (if (or (null db-path) (string= db-path ":memory:"))
                (sqlite-open)
              (sqlite-open (expand-file-name db-path)))))
    (sqlite-execute db "PRAGMA foreign_keys = ON")
    db))

(defun writing-habit-db-close (db)
  "Close the SQLite connection DB."
  (sqlite-close db))


;;;; Schema loading

(defun writing-habit-db--statements (sql)
  "Split the SQL script text SQL into a list of individual statements.
Line comments, from -- to end of line, are removed.  This handles the
shared schema.sql, whose string literals contain no semicolons."
  (let* ((no-comments
          (mapconcat
           (lambda (line)
             (if (string-match "--" line)
                 (substring line 0 (match-beginning 0))
               line))
           (split-string sql "\n")
           "\n"))
         (parts (split-string no-comments ";")))
    (delq nil
          (mapcar (lambda (s)
                    (let ((trimmed (string-trim s)))
                      (unless (string-empty-p trimmed) trimmed)))
                  parts))))

(defun writing-habit-db-init (db &optional schema-file)
  "Create the schema in DB and seed the three activities.  Safe to run twice.
SCHEMA-FILE defaults to `writing-habit-db-schema-file'."
  (let ((sql (with-temp-buffer
               (insert-file-contents (or schema-file writing-habit-db-schema-file))
               (buffer-string))))
    (dolist (stmt (writing-habit-db--statements sql))
      (sqlite-execute db stmt)))
  ;; `sqlite-execute' commits each statement, so restore the pragma, the
  ;; same reason db.py re-runs it after executescript.
  (sqlite-execute db "PRAGMA foreign_keys = ON")
  db)


;;;; Reference-table helpers

(defun writing-habit-db--blank-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (and (stringp value) (string-empty-p value))))

(defun writing-habit-db-get-category-id (db name)
  "Return the category id for NAME in DB, or nil when NAME is blank or unknown."
  (unless (writing-habit-db--blank-p name)
    (caar (sqlite-select db
                         "SELECT category_id FROM category WHERE name = ?"
                         (list name)))))

(defun writing-habit-db-get-or-create-project (db code &optional description risk-class)
  "Return the project id for legend CODE in DB, creating the row when absent.
A later call that supplies a DESCRIPTION or a RISK-CLASS backfills those
fields when they were previously empty, so the schedule legend can enrich
projects that a tracker created first.  An existing value is never
overwritten."
  (let ((row (car (sqlite-select
                   db
                   "SELECT project_id, description, risk_class FROM project WHERE code = ?"
                   (list code)))))
    (if row
        (let ((pid (nth 0 row))
              (existing-desc (nth 1 row))
              (existing-risk (nth 2 row)))
          (when (and (not (writing-habit-db--blank-p description))
                     (writing-habit-db--blank-p existing-desc))
            (sqlite-execute db "UPDATE project SET description = ? WHERE project_id = ?"
                            (list description pid)))
          (when (and (not (writing-habit-db--blank-p risk-class))
                     (writing-habit-db--blank-p existing-risk))
            (sqlite-execute db "UPDATE project SET risk_class = ? WHERE project_id = ?"
                            (list risk-class pid)))
          pid)
      (sqlite-execute
       db "INSERT INTO project(code, description, risk_class) VALUES (?, ?, ?)"
       (list code description risk-class))
      (caar (sqlite-select db "SELECT last_insert_rowid()")))))


;;;; Provenance and time helpers

(defun writing-habit-db-log-import (db source-type source-name rows-read rows-inserted
                                       &optional tool-version note)
  "Record the provenance of one import into DB.
SOURCE-TYPE and SOURCE-NAME name the import.  ROWS-READ and ROWS-INSERTED
are counts.  TOOL-VERSION and NOTE are optional."
  (sqlite-execute
   db
   (concat "INSERT INTO import_log"
           "(source_type, source_name, rows_read, rows_inserted, tool_version, note)"
           " VALUES (?, ?, ?, ?, ?, ?)")
   (list source-type source-name rows-read rows-inserted tool-version note))
  db)

(defun writing-habit-db-minutes-between (start end)
  "Return the whole minutes between the HH:MM times START and END on one day."
  (let* ((s (split-string start ":"))
         (e (split-string end ":"))
         (sh (string-to-number (nth 0 s)))
         (sm (string-to-number (nth 1 s)))
         (eh (string-to-number (nth 0 e)))
         (em (string-to-number (nth 1 e))))
    (- (+ (* eh 60) em) (+ (* sh 60) sm))))

(provide 'writing-habit-db)
;;; writing-habit-db.el ends here
