;;; writing-habit.el --- Track and compare planned versus actual writing effort -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1") (transient "0.4"))
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT license.

;;; Commentary:

;; writing-habit is the Emacs Lisp companion to the Python writing-habit
;; package and to writing-schedule.el.  It records the effort you actually
;; spent on academic writing and compares it against a weekly plan, sharing
;; one SQLite database (schema.sql) with the Python port, so either can read
;; what the other wrote.
;;
;; This file is the aggregator.  It pulls in the five module files, adds an
;; initdb command, gathers every command under a transient menu, and offers
;; a batch entry point so the toolkit runs from a shell or a Makefile the
;; way the Python argparse CLI does.
;;
;;   Modules: writing-habit-db, writing-habit-name, writing-habit-plan,
;;            writing-habit-track, writing-habit-compare, writing-habit-report.
;;
;; Interactive use: M-x writing-habit opens the menu.
;;
;; Batch use:
;;   emacs --batch -l writing-habit -f writing-habit-batch initdb --db habit.db
;;   emacs --batch -l writing-habit -f writing-habit-batch \
;;         plan import my-week.org --week 2026-01-19 --db habit.db
;;   emacs --batch -l writing-habit -f writing-habit-batch \
;;         track import actuals.csv --format csv --db habit.db
;;   emacs --batch -l writing-habit -f writing-habit-batch compare --week 2026-01-19 --db habit.db
;;   emacs --batch -l writing-habit -f writing-habit-batch \
;;         dashboard --week 2026-01-19 --out week.html --db habit.db
;;   emacs --batch -l writing-habit -f writing-habit-batch name 4gAAeAsA-gWW --table my-week.org
;;
;; Plan import additionally needs writing-schedule.el on the load-path; the
;; other commands do not.

;;; Code:

(require 'transient)
(require 'writing-habit-db)
(require 'writing-habit-name)
(require 'writing-habit-plan)
(require 'writing-habit-track)
(require 'writing-habit-compare)
(require 'writing-habit-report)
(require 'writing-habit-dashboard)

(defconst writing-habit-version "0.0.0"
  "Version of the writing-habit Emacs Lisp package.")


;;;; Database creation

;;;###autoload
(defun writing-habit-initdb (db-file)
  "Create the schema and seed the activities in the database at DB-FILE.
Safe to run on an existing database."
  (interactive (list (read-file-name "Database file: ")))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (progn (writing-habit-db-init db)
               (message "Initialized %s" db-file)
               db-file)
      (writing-habit-db-close db))))


;;;; Transient menu

;;;###autoload (autoload 'writing-habit "writing-habit" nil t)
(transient-define-prefix writing-habit ()
  "Menu for the writing-habit toolkit."
  ["writing-habit"
   ["Set up"
    ("d" "Create a database"      writing-habit-initdb)]
   ["Plan and track"
    ("p" "Import a weekly plan"   writing-habit-plan-import-file)
    ("c" "Import actuals CSV"     writing-habit-track-import-csv-file)
    ("i" "Import actuals ICS"     writing-habit-track-import-ics-file)
    ("k" "Harvest org clocks"     writing-habit-track-harvest-clock-file)
    ("a" "Add a session by hand"  writing-habit-track-add-to-file)]
   ["Review"
    ("r" "Weekly report"          writing-habit-report-week)
    ("D" "HTML dashboard"         writing-habit-dashboard)
    ("n" "Decode a schedule code" writing-habit-name)]])


;;;; Command-line dispatch

(defun writing-habit--parse-args (args)
  "Split ARGS into a cons (POSITIONALS . OPTIONS).
POSITIONALS is a list of bare tokens.  OPTIONS is an alist mapping the name
of each --flag (without the dashes) to the token that follows it."
  (let ((pos '()) (opts '()))
    (while args
      (let ((tok (car args)))
        (if (string-prefix-p "--" tok)
            (let ((key (substring tok 2))
                  (val (cadr args)))
              (when (or (null val) (string-prefix-p "--" val))
                (error "Option --%s needs a value" key))
              (push (cons key val) opts)
              (setq args (cddr args)))
          (push tok pos)
          (setq args (cdr args)))))
    (cons (nreverse pos) (nreverse opts))))

(defun writing-habit--opt (opts name)
  "Return the value of option NAME in OPTS, or nil."
  (cdr (assoc name opts)))

(defun writing-habit--req (opts name)
  "Return the value of option NAME in OPTS, or signal a usage error."
  (or (writing-habit--opt opts name)
      (error "Missing required option --%s" name)))

(defun writing-habit--dispatch (args)
  "Run the command described by ARGS and return its output string.
Signal an error on a usage problem.  This is the shared core of the batch
entry point, factored out so it can be tested directly."
  (let* ((parsed (writing-habit--parse-args args))
         (pos (car parsed))
         (opts (cdr parsed))
         (cmd (car pos)))
    (pcase cmd
      ("name"
       (let ((code (nth 1 pos)))
         (unless code (error "Usage: name CODE [--table FILE]"))
         (writing-habit-name-report-string code (writing-habit--opt opts "table"))))
      ("initdb"
       (let* ((dbf (writing-habit--req opts "db"))
              (db (writing-habit-db-connect dbf)))
         (unwind-protect (progn (writing-habit-db-init db) (format "Initialized %s" dbf))
           (writing-habit-db-close db))))
      ("plan"
       (unless (equal (nth 1 pos) "import")
         (error "Usage: plan import PATH --week WEEK --db DB"))
       (let ((path (nth 2 pos))
             (week (writing-habit--req opts "week"))
             (db (writing-habit-db-connect (writing-habit--req opts "db"))))
         (unless path (error "Usage: plan import PATH --week WEEK --db DB"))
         (unwind-protect
             (format "Imported %d planned blocks from %s"
                     (writing-habit-plan-import db path week) path)
           (writing-habit-db-close db))))
      ("track"
       (pcase (nth 1 pos)
         ("import"
          (let ((path (nth 2 pos))
                (fmt (or (writing-habit--opt opts "format") "csv"))
                (db (writing-habit-db-connect (writing-habit--req opts "db"))))
            (unless path (error "Usage: track import PATH [--format csv|ics] --db DB"))
            (unwind-protect
                (format "Imported %d sessions from %s"
                        (pcase fmt
                          ("csv" (writing-habit-track-import-csv db path))
                          ("ics" (writing-habit-track-import-ics db path))
                          (_ (error "Unknown format %s, use csv or ics" fmt)))
                        path)
              (writing-habit-db-close db))))
         ("add"
          (let ((db (writing-habit-db-connect (writing-habit--req opts "db")))
                (min (writing-habit--opt opts "minutes")))
            (unwind-protect
                (format "Added session %s"
                        (writing-habit-track-add
                         db
                         :day (writing-habit--req opts "day")
                         :project (writing-habit--req opts "project")
                         :minutes (and min (string-to-number min))
                         :category (writing-habit--opt opts "category")
                         :start (writing-habit--opt opts "start")
                         :end (writing-habit--opt opts "end")
                         :note (writing-habit--opt opts "note")))
              (writing-habit-db-close db))))
         (_ (error "Usage: track import|add ..."))))
      ("compare"
       (let* ((week (writing-habit--req opts "week"))
              (plot (writing-habit--opt opts "plot"))
              (db (writing-habit-db-connect (writing-habit--req opts "db"))))
         (unwind-protect
             (let ((out (writing-habit-report-week-string db week plot)))
               (if plot
                   (progn (writing-habit-report-write-plot db week plot)
                          (concat out (format "\n# Wrote plot to %s\n" plot)))
                 out))
           (writing-habit-db-close db))))
      ("dashboard"
       (let ((week (writing-habit--req opts "week"))
             (out (writing-habit--req opts "out"))
             (db (writing-habit-db-connect (writing-habit--req opts "db"))))
         (unwind-protect
             (progn (writing-habit-dashboard-write db week out)
                    (format "Wrote dashboard to %s" out))
           (writing-habit-db-close db))))
      (_ (error "Unknown command %S; use initdb, plan, track, compare, dashboard, or name"
                (or cmd ""))))))

;;;###autoload
(defun writing-habit-batch ()
  "Entry point for command-line use through \"emacs --batch\".
Read the remaining command-line arguments, run the command, print the
result to standard output, and exit with a status code."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (condition-case err
        (progn
          (princ (writing-habit--dispatch args))
          (princ "\n")
          (kill-emacs 0))
      (error
       (message "error: %s" (error-message-string err))
       (kill-emacs 1)))))

(provide 'writing-habit)
;;; writing-habit.el ends here
