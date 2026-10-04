;;; writing-habit-dispatch.el --- A command center for the tracker and the scheduler -*- lexical-binding: t; -*-

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

;; This is the Emacs counterpart of the tabs and forms of the Python
;; package's graphical interface.  `writing-habit-dispatch' opens a menu
;; with one group per stage of the weekly loop, the same groups as the
;; Python tabs:
;;
;;   Schedule, Generate, Sheets   writing-schedule.el commands
;;   Plan, Track, Compare         writing-habit commands
;;   History, Context, Seasons    writing-habit commands
;;
;; Each group opens a form whose options mirror the command line.  The
;; database option is seeded from `writing-habit-default-db' and then from
;; the database used last, and the week option from today's date, so a
;; weekly loop of plan import, compare, and dashboard needs no retyping.
;;
;; The interface teaches the command line rather than hiding it.  Every
;; run is written to the *writing-habit log* buffer as the equivalent shell
;; line, the writing-habit line that the Python package and the Emacs batch
;; entry point both accept, or the writing-schedule.sh line, followed by
;; what the command printed.  A file that a command writes gets a button in
;; the log, and the main one is shown at once: an HTML dashboard in eww, a
;; plot in image mode, a PDF in doc-view, and an org, ICS, or TeX file as
;; text.
;;
;; The scheduler groups appear only when writing-schedule.el is loadable,
;; as the Python interface hides its scheduler tabs when writing_schedule
;; is missing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'transient)
(require 'writing-habit-db)

(declare-function writing-habit--dispatch "writing-habit" (args))
(declare-function writing-habit-track-harvest-clock-file "writing-habit-track" (db-file org-file))
(declare-function writing-schedule-batch-insert-template "writing-schedule" (n &optional file))
(declare-function writing-schedule-batch-check "writing-schedule" (table))
(declare-function writing-schedule-batch-generate "writing-schedule" (table week &optional out-dir))
(declare-function writing-schedule-batch-generate-day "writing-schedule" (table day &optional out-dir))
(declare-function writing-schedule-export-ics "writing-schedule" (&optional file))
(declare-function writing-schedule-batch-list-weeks "writing-schedule" (&optional directory))
(declare-function writing-schedule-batch-timeblock-sheets "writing-schedule"
                  (table week &optional per-day out-dir format))
(declare-function writing-schedule-batch-timeblock-sheet-day "writing-schedule"
                  (table day &optional out-dir format))
(declare-function writing-schedule-new-week-from-template "writing-schedule" ())
(declare-function writing-schedule-open-recent "writing-schedule" ())
(declare-function org-read-date "org" (&optional with-time to-time from-string prompt
                                                  default-time default-input inactive))
(declare-function org-mode "org" ())
(declare-function eww-open-file "eww" (file))
(defvar writing-schedule-overlap-action)
(defvar writing-schedule-use-todo)
(defvar writing-schedule-directory)

(defgroup writing-habit-dispatch nil
  "The writing-habit command center."
  :group 'writing-habit
  :prefix "writing-habit-dispatch-")

(defcustom writing-habit-default-db nil
  "Database file that seeds the --db option of every form, or nil."
  :type '(choice (const :tag "None" nil) file)
  :group 'writing-habit-dispatch)

(defcustom writing-habit-dispatch-auto-preview t
  "When non-nil, show the main file a command writes as soon as it is written."
  :type 'boolean
  :group 'writing-habit-dispatch)

(defvar writing-habit-dispatch--last-db nil
  "The database file used by the last run.")

(defvar writing-habit-dispatch--last-table nil
  "The weekly table used by the last scheduler run.")

;;;; The log

(defvar writing-habit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map)
  "Keymap of `writing-habit-log-mode'.")

(define-derived-mode writing-habit-log-mode special-mode "WH-Log"
  "Mode of the *writing-habit log* buffer.
Each run shows the equivalent shell line, so a line can be copied into a
terminal, then the output of the command and a button for each file it
wrote.")

(defun writing-habit-dispatch--log-buffer ()
  "Return the log buffer, creating it when needed."
  (let ((buf (get-buffer-create "*writing-habit log*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'writing-habit-log-mode) (writing-habit-log-mode)))
    buf))

(defun writing-habit-dispatch--shell-line (words)
  "Return WORDS as one shell command line with each word quoted."
  (mapconcat #'shell-quote-argument words " "))

(defun writing-habit-dispatch--log (shell-lines output files &optional failed)
  "Append a run to the log: SHELL-LINES, then OUTPUT, then FILES as buttons.
FAILED non-nil marks the run as an error.  Return the log buffer."
  (let ((buf (writing-habit-dispatch--log-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp) (insert "\n"))
        (insert (propertize (format-time-string "%Y-%m-%d %H:%M:%S") 'face 'shadow) "\n")
        (dolist (line shell-lines)
          (insert (if (string-prefix-p "(" line)
                      (propertize (concat "  " line) 'face 'shadow)
                    (propertize (concat "$ " line) 'face 'font-lock-keyword-face))
                  "\n"))
        (when (and output (not (string-empty-p (string-trim output))))
          (insert (if failed (propertize output 'face 'error) output))
          (unless (bolp) (insert "\n")))
        (dolist (f files)
          (insert "Wrote ")
          (insert-text-button f 'action (lambda (_) (writing-habit-dispatch-preview f))
                              'follow-link t 'help-echo "Show this file")
          (insert "\n"))))
    (display-buffer buf '(display-buffer-reuse-window display-buffer-at-bottom
                                                      (window-height . 0.3)))
    buf))

;;;; Previews

(defun writing-habit-dispatch-preview (file)
  "Show FILE in the way its kind deserves.
HTML opens in eww, an image in image mode, a PDF in doc-view, and any
other file read-only as text."
  (interactive "fFile: ")
  (unless (file-exists-p file) (user-error "No such file: %s" file))
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "html" "htm")
     (if (fboundp 'eww-open-file)
         (save-selected-window
           (let ((display-buffer-overriding-action '(display-buffer-pop-up-window)))
             (eww-open-file file)))
       (browse-url-of-file file)))
    (_ (save-selected-window (view-file-other-window file)))))

(defun writing-habit-dispatch--written-files (output)
  "Return the files that OUTPUT reports writing, in order."
  (let ((files '()) (start 0))
    (while (string-match "^Wrote [^\n]*?\\(?:to\\|:\\) *\\(/[^\n]+?\\) *$" output start)
      (push (match-string 1 output) files)
      (setq start (match-end 0)))
    (nreverse files)))

;;;; Running

(defun writing-habit-dispatch--parse (args)
  "Split transient ARGS into (POSITIONALS . OPTIONS).
A --path= or --code= option becomes a positional word.  Every other
--name=value becomes the two words --name and value, and a bare --flag
stays one word."
  (let ((pos '()) (opts '()))
    (dolist (a args)
      (cond
       ((string-match "\\`--\\(path\\|code\\)=\\(.*\\)\\'" a)
        (push (match-string 2 a) pos))
       ((string-match "\\`--\\([^=]+\\)=\\(.*\\)\\'" a)
        (push (concat "--" (match-string 1 a)) opts)
        (push (match-string 2 a) opts))
       (t (push a opts))))
    (cons (nreverse pos) (nreverse opts))))

(defun writing-habit-dispatch--arg (args name)
  "Return the value of the option NAME, given as --NAME=, in transient ARGS."
  (transient-arg-value (concat "--" name "=") args))

(defun writing-habit-dispatch-run-habit (command args &optional outputs)
  "Run the writing-habit COMMAND, a list of words, with transient ARGS.
OUTPUTS names the options, such as \"out\" or \"plot\", that hold files
the command writes.  Log the run and return its output."
  (require 'writing-habit)
  (let* ((parsed (writing-habit-dispatch--parse args))
         (argv (append command (car parsed) (cdr parsed)))
         (db (writing-habit-dispatch--arg args "db"))
         (files (delq nil (mapcar (lambda (o)
                                    (let ((f (writing-habit-dispatch--arg args o)))
                                      (and f (expand-file-name f))))
                                  outputs)))
         (shell (list (writing-habit-dispatch--shell-line (cons "writing-habit" argv))
                      (writing-habit-dispatch--shell-line
                       (append '("emacs" "--batch" "-l" "writing-habit"
                                 "-f" "writing-habit-batch")
                               argv))))
         output failed)
    (when db (setq writing-habit-dispatch--last-db db))
    (condition-case err
        (setq output (writing-habit--dispatch argv))
      (error (setq failed t output (concat "error: " (error-message-string err)))))
    (setq files (seq-filter #'file-exists-p files))
    (writing-habit-dispatch--log shell (or output "") (and (not failed) files) failed)
    (when (and (not failed) files writing-habit-dispatch-auto-preview)
      (writing-habit-dispatch-preview (car files)))
    output))

(defun writing-habit-dispatch-run-schedule (words thunk &optional notes)
  "Run THUNK, a scheduler call, and log it as writing-schedule.sh WORDS.
NOTES are extra lines that name settings the shell line cannot show.
Return the printed output."
  (unless (require 'writing-schedule nil t)
    (user-error "This command needs writing-schedule.el on the load-path"))
  (let* ((env (seq-take-while (lambda (w) (string-match-p "\\`[A-Z_]+=" w)) words))
         (rest (nthcdr (length env) words))
         (shell (cons (concat (mapconcat (lambda (e)
                                           (let ((i (string-search "=" e)))
                                             (concat (substring e 0 (1+ i))
                                                     (shell-quote-argument (substring e (1+ i))))))
                                         env " ")
                              (if env " " "")
                              (writing-habit-dispatch--shell-line
                               (cons "writing-schedule.sh" rest)))
                      notes))
         output failed)
    (condition-case err
        (setq output (with-output-to-string (funcall thunk)))
      (error (setq failed t output (concat "error: " (error-message-string err)))))
    (let ((files (and (not failed)
                      (seq-filter #'file-exists-p (writing-habit-dispatch--written-files output)))))
      (writing-habit-dispatch--log shell output files failed)
      (when (and files writing-habit-dispatch-auto-preview)
        (writing-habit-dispatch-preview (car files))))
    output))

;;;; Readers and seeds

(defun writing-habit-dispatch--read-date (prompt _initial-input _history)
  "Read a date with the org calendar after PROMPT, returning an ISO string."
  (require 'org)
  (format-time-string "%F" (org-read-date nil t nil prompt)))

(defun writing-habit-dispatch--read-db (prompt initial-input _history)
  "Read a database file after PROMPT, starting from INITIAL-INPUT."
  (expand-file-name
   (read-file-name prompt nil nil nil
                   (or initial-input writing-habit-dispatch--last-db writing-habit-default-db))))

(defun writing-habit-dispatch--read-table (prompt initial-input _history)
  "Read a weekly table file after PROMPT, starting from INITIAL-INPUT."
  (let ((f (expand-file-name
            (read-file-name prompt nil nil t
                            (or initial-input writing-habit-dispatch--last-table)))))
    (setq writing-habit-dispatch--last-table f)
    f))

(defun writing-habit-dispatch--seed (&rest more)
  "Return the seed values of a form: the database, today's week, and MORE."
  (let ((db (or writing-habit-dispatch--last-db writing-habit-default-db)))
    (append (and db (list (concat "--db=" (expand-file-name db))))
            more)))

(defun writing-habit-dispatch--today ()
  "Return today's date as an ISO string."
  (format-time-string "%F"))

(defun writing-habit-dispatch--read-tag (prompt initial-input history)
  "Read a context tag after PROMPT, offering the common ones.
INITIAL-INPUT and HISTORY are passed to `completing-read'."
  (completing-read prompt '("teaching" "meeting" "travel" "data-collection"
                            "grant-deadline" "holiday")
                   nil nil initial-input history))

;;;; Shared infixes

(transient-define-argument writing-habit-dispatch--db ()
  :description "Database" :class 'transient-option :key "-d" :argument "--db="
  :reader #'writing-habit-dispatch--read-db :always-read t)

(transient-define-argument writing-habit-dispatch--week ()
  :description "Week (any date in it)" :class 'transient-option :key "-w"
  :argument "--week=" :reader #'writing-habit-dispatch--read-date :always-read t)

(transient-define-argument writing-habit-dispatch--table ()
  :description "Weekly table" :class 'transient-option :key "-t"
  :argument "--path=" :reader #'writing-habit-dispatch--read-table :always-read t)

;;;; Plan

(defun writing-habit-dispatch--require (args &rest names)
  "Signal a user error naming the first of NAMES missing from ARGS."
  (dolist (n names)
    (unless (writing-habit-dispatch--arg args n)
      (user-error "Set %s first" (pcase n ("path" "the file") ("code" "the code")
                                   (_ (concat "--" n)))))))

(transient-define-suffix writing-habit-dispatch--initdb (args)
  "Create the schema and seed the activities."
  (interactive (list (transient-args 'writing-habit-dispatch-plan)))
  (writing-habit-dispatch--require args "db")
  (writing-habit-dispatch-run-habit '("initdb") (seq-filter (lambda (a) (string-prefix-p "--db=" a)) args)))

(transient-define-suffix writing-habit-dispatch--plan-import (args)
  "Load the weekly table into the database for the week."
  (interactive (list (transient-args 'writing-habit-dispatch-plan)))
  (writing-habit-dispatch--require args "db" "path" "week")
  (writing-habit-dispatch-run-habit '("plan" "import") args))

;;;###autoload (autoload 'writing-habit-dispatch-plan "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-plan ()
  "Create a database and import a weekly plan."
  :init-value (lambda (obj) (oset obj value (writing-habit-dispatch--seed
                     (concat "--week=" (writing-habit-dispatch--today)))))
  ["Options"
   (writing-habit-dispatch--db)
   (writing-habit-dispatch--table)
   (writing-habit-dispatch--week)]
  ["Run"
   ("i" "initdb" writing-habit-dispatch--initdb)
   ("p" "plan import" writing-habit-dispatch--plan-import)])

;;;; Track

(transient-define-suffix writing-habit-dispatch--track-import (args)
  "Load actual sessions from a CSV or ICS file."
  (interactive (list (transient-args 'writing-habit-dispatch-track)))
  (writing-habit-dispatch--require args "db" "path")
  (writing-habit-dispatch-run-habit
   '("track" "import")
   (seq-filter (lambda (a) (string-match-p "\\`--\\(db\\|path\\|format\\)=" a)) args)))

(transient-define-suffix writing-habit-dispatch--track-add (args)
  "Add one session by hand."
  (interactive (list (transient-args 'writing-habit-dispatch-track)))
  (writing-habit-dispatch--require args "db" "day" "project")
  (unless (or (writing-habit-dispatch--arg args "minutes")
              (and (writing-habit-dispatch--arg args "start") (writing-habit-dispatch--arg args "end")))
    (user-error "Set --minutes, or both --start and --end"))
  (writing-habit-dispatch-run-habit
   '("track" "add")
   (seq-remove (lambda (a) (string-match-p "\\`--\\(path\\|format\\)=" a)) args)))

(transient-define-suffix writing-habit-dispatch--harvest (args)
  "Harvest org-clock intervals from the org file into sessions."
  (interactive (list (transient-args 'writing-habit-dispatch-track)))
  (writing-habit-dispatch--require args "db" "path")
  (require 'writing-habit-track)
  (let ((db (writing-habit-dispatch--arg args "db"))
        (org (writing-habit-dispatch--arg args "path")))
    (setq writing-habit-dispatch--last-db db)
    (writing-habit-dispatch--log
     (list (format "(writing-habit-track-harvest-clock-file %S %S)" db org))
     (with-output-to-string
       (princ (format "%s" (writing-habit-track-harvest-clock-file db org))))
     nil)))

;;;###autoload (autoload 'writing-habit-dispatch-track "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-track ()
  "Record the effort you spent."
  :init-value (lambda (obj) (oset obj value (writing-habit-dispatch--seed
                     (concat "--day=" (writing-habit-dispatch--today)) "--format=csv")))
  ["Options"
   (writing-habit-dispatch--db)
   ("-f" "Actuals or org file" "--path=" :reader transient-read-existing-file)
   ("-F" "Format" "--format=" :choices ("csv" "ics"))]
  ["Session"
   ("-D" "Day" "--day=" :reader writing-habit-dispatch--read-date)
   ("-p" "Project code" "--project=")
   ("-m" "Minutes" "--minutes=" :reader transient-read-number-N+)
   ("-c" "Category" "--category=" :choices ("generative" "editing" "support"))
   ("-s" "Start (HH:MM)" "--start=")
   ("-e" "End (HH:MM)" "--end=")
   ("-n" "Note" "--note=")]
  ["Run"
   ("i" "track import" writing-habit-dispatch--track-import)
   ("a" "track add" writing-habit-dispatch--track-add)
   ("k" "harvest org clocks" writing-habit-dispatch--harvest)])

;;;; Compare

(transient-define-suffix writing-habit-dispatch--compare (args)
  "Print the planned versus actual report for the week."
  (interactive (list (transient-args 'writing-habit-dispatch-compare)))
  (writing-habit-dispatch--require args "db" "week")
  (let ((out (writing-habit-dispatch-run-habit
              '("compare")
              (seq-filter (lambda (a) (string-match-p "\\`--\\(db\\|week\\|plot\\)=" a)) args)
              '("plot"))))
    (when out
      (with-current-buffer (get-buffer-create "*writing-habit compare*")
        (let ((inhibit-read-only t))
          (erase-buffer) (insert out) (org-mode) (goto-char (point-min)))
        (display-buffer (current-buffer))))))

(transient-define-suffix writing-habit-dispatch--dashboard (args)
  "Write the single-week HTML dashboard."
  (interactive (list (transient-args 'writing-habit-dispatch-compare)))
  (writing-habit-dispatch--require args "db" "week" "out")
  (writing-habit-dispatch-run-habit
   '("dashboard")
   (seq-filter (lambda (a) (string-match-p "\\`--\\(db\\|week\\|out\\)=" a)) args)
   '("out")))

;;;###autoload (autoload 'writing-habit-dispatch-compare "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-compare ()
  "Compare the week you planned with the week you had."
  :init-value (lambda (obj) (oset obj value (writing-habit-dispatch--seed
                     (concat "--week=" (writing-habit-dispatch--today)))))
  ["Options"
   (writing-habit-dispatch--db)
   (writing-habit-dispatch--week)
   ("-p" "Bar chart (PNG)" "--plot=" :reader transient-read-file)
   ("-o" "Dashboard (HTML)" "--out=" :reader transient-read-file)]
  ["Run"
   ("c" "compare" writing-habit-dispatch--compare)
   ("D" "dashboard" writing-habit-dispatch--dashboard)])

;;;; History

(transient-define-suffix writing-habit-dispatch--history (args)
  "Print the weekly adherence series, and plot them when asked."
  (interactive (list (transient-args 'writing-habit-dispatch-history)))
  (writing-habit-dispatch--require args "db")
  (writing-habit-dispatch-run-habit '("history") args '("plot")))

;;;###autoload (autoload 'writing-habit-dispatch-history "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-history ()
  "Follow adherence across weeks."
  :init-value (lambda (obj) (oset obj value (writing-habit-dispatch--seed)))
  ["Options"
   (writing-habit-dispatch--db)
   ("-f" "From week" "--from=" :reader writing-habit-dispatch--read-date)
   ("-t" "To week" "--to=" :reader writing-habit-dispatch--read-date)
   ("-p" "Plots (PNG)" "--plot=" :reader transient-read-file)]
  ["Run"
   ("h" "history" writing-habit-dispatch--history)])

;;;; Context

(defun writing-habit-dispatch--context (sub args keep)
  "Run context SUB with the options of ARGS named in KEEP."
  (writing-habit-dispatch-run-habit
   (list "context" sub)
   (seq-filter (lambda (a) (seq-some (lambda (k) (string-prefix-p (concat "--" k "=") a)) keep))
               args)))

(transient-define-suffix writing-habit-dispatch--context-set (args)
  "Attach a tag to the week."
  (interactive (list (transient-args 'writing-habit-dispatch-context)))
  (writing-habit-dispatch--require args "db" "week" "tag")
  (writing-habit-dispatch--context "set" args '("db" "week" "tag" "note")))

(transient-define-suffix writing-habit-dispatch--context-clear (args)
  "Remove the tag, or every tag, from the week."
  (interactive (list (transient-args 'writing-habit-dispatch-context)))
  (writing-habit-dispatch--require args "db" "week")
  (writing-habit-dispatch--context "clear" args '("db" "week" "tag")))

(transient-define-suffix writing-habit-dispatch--context-list (args)
  "List the context tags of the week, or of every week."
  (interactive (list (transient-args 'writing-habit-dispatch-context)))
  (writing-habit-dispatch--require args "db")
  (writing-habit-dispatch--context "list" args '("db" "week")))

;;;###autoload (autoload 'writing-habit-dispatch-context "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-context ()
  "Tag weeks with the events that shaped them."
  :init-value (lambda (obj) (oset obj value (writing-habit-dispatch--seed
                     (concat "--week=" (writing-habit-dispatch--today)))))
  ["Options"
   (writing-habit-dispatch--db)
   ("-w" "Week (empty lists every week)" "--week=" :reader writing-habit-dispatch--read-date)
   ("-t" "Tag" "--tag=" :reader writing-habit-dispatch--read-tag)
   ("-n" "Note" "--note=")]
  ["Run"
   ("s" "context set" writing-habit-dispatch--context-set)
   ("c" "context clear" writing-habit-dispatch--context-clear)
   ("l" "context list" writing-habit-dispatch--context-list)])

;;;; Seasons and names

(transient-define-suffix writing-habit-dispatch--seasons (args)
  "Write the seasons dashboard grouped by month, context, and plan shape."
  (interactive (list (transient-args 'writing-habit-dispatch-seasons)))
  (writing-habit-dispatch--require args "db" "out")
  (writing-habit-dispatch-run-habit
   '("seasons") (seq-filter (lambda (a) (string-match-p "\\`--\\(db\\|out\\)=" a)) args) '("out")))

(transient-define-suffix writing-habit-dispatch--name (args)
  "Decode a schedule code, and check it against a table's legend."
  (interactive (list (transient-args 'writing-habit-dispatch-seasons)))
  (writing-habit-dispatch--require args "code")
  (writing-habit-dispatch-run-habit
   '("name") (seq-filter (lambda (a) (string-match-p "\\`--\\(code\\|table\\)=" a)) args)))

;;;###autoload (autoload 'writing-habit-dispatch-seasons "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-seasons ()
  "See the seasons of your writing, and decode schedule codes."
  :init-value (lambda (obj) (oset obj value (writing-habit-dispatch--seed)))
  ["Seasons"
   (writing-habit-dispatch--db)
   ("-o" "Seasons page (HTML)" "--out=" :reader transient-read-file)
   ("s" "seasons" writing-habit-dispatch--seasons)]
  ["Name"
   ("-c" "Schedule code" "--code=")
   ("-t" "Check against table" "--table=" :reader transient-read-existing-file)
   ("n" "name" writing-habit-dispatch--name)])

;;;; Scheduler groups

(defun writing-habit-dispatch--schedule-available-p ()
  "Return non-nil when writing-schedule.el can be loaded."
  (and (locate-library "writing-schedule") t))

(defun writing-habit-dispatch--schedule-notes (args)
  "Return lines naming the scheduler settings that ARGS bind for one run."
  (delq nil (list (and (member "--strict" args)
                       "(with writing-schedule-overlap-action bound to error)")
                  (and (member "--no-todo" args)
                       "(with writing-schedule-use-todo bound to nil)"))))

(defmacro writing-habit-dispatch--with-schedule-options (args &rest body)
  "Run BODY with the scheduler settings that ARGS ask for."
  (declare (indent 1))
  `(let ((writing-schedule-overlap-action
          (if (member "--strict" ,args) 'error writing-schedule-overlap-action))
         (writing-schedule-use-todo
          (if (member "--no-todo" ,args) nil writing-schedule-use-todo)))
     ,@body))

(defun writing-habit-dispatch--out-words (args words)
  "Return WORDS prefixed by the WS_OUT_DIR assignment that ARGS ask for."
  (let ((dir (writing-habit-dispatch--arg args "dir")))
    (if dir (cons (concat "WS_OUT_DIR=" (expand-file-name dir)) words) words)))

(transient-define-suffix writing-habit-dispatch--template (args)
  "Write a blank weekly table for the number of projects."
  (interactive (list (transient-args 'writing-habit-dispatch-schedule)))
  (let ((n (or (writing-habit-dispatch--arg args "projects") "3"))
        (file (writing-habit-dispatch--arg args "file")))
    (writing-habit-dispatch-run-schedule
     (delq nil (list "template" n file))
     (lambda () (writing-schedule-batch-insert-template (string-to-number n) (or file ""))))
    (when (and file (file-exists-p file)) (find-file file))))

(transient-define-suffix writing-habit-dispatch--check (args)
  "Report the overlapping blocks of the table."
  (interactive (list (transient-args 'writing-habit-dispatch-schedule)))
  (writing-habit-dispatch--require args "path")
  (let ((table (writing-habit-dispatch--arg args "path")))
    (writing-habit-dispatch-run-schedule
     (list "check" table) (lambda () (writing-schedule-batch-check table)))))

(transient-define-suffix writing-habit-dispatch--edit-table (args)
  "Open the table in its editing mode."
  (interactive (list (transient-args 'writing-habit-dispatch-schedule)))
  (writing-habit-dispatch--require args "path")
  (find-file (writing-habit-dispatch--arg args "path")))

;;;###autoload (autoload 'writing-habit-dispatch-schedule "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-schedule ()
  "Make and check the weekly block table."
  :init-value (lambda (obj) (oset obj value (and writing-habit-dispatch--last-table
                         (list (concat "--path=" writing-habit-dispatch--last-table)))))
  ["Options"
   (writing-habit-dispatch--table)
   ("-n" "Projects (1-26)" "--projects=" :reader transient-read-number-N+)
   ("-f" "New table file" "--file=" :reader transient-read-file)]
  ["Run"
   ("e" "edit table" writing-habit-dispatch--edit-table)
   ("k" "check" writing-habit-dispatch--check)
   ("T" "template" writing-habit-dispatch--template)
   ("N" "new week from template" writing-schedule-new-week-from-template)
   ("r" "open recent table" writing-schedule-open-recent)])

(transient-define-suffix writing-habit-dispatch--generate (args)
  "Write the dated schedule and its calendar file for the week."
  (interactive (list (transient-args 'writing-habit-dispatch-generate)))
  (writing-habit-dispatch--require args "path" "week")
  (let ((table (writing-habit-dispatch--arg args "path"))
        (week (writing-habit-dispatch--arg args "week"))
        (dir (writing-habit-dispatch--arg args "dir")))
    (writing-habit-dispatch-run-schedule
     (writing-habit-dispatch--out-words args (list "generate" table week))
     (lambda () (writing-habit-dispatch--with-schedule-options args
                  (writing-schedule-batch-generate table week dir)))
     (writing-habit-dispatch--schedule-notes args))))

(transient-define-suffix writing-habit-dispatch--generate-day (args)
  "Write the schedule and calendar file for one day."
  (interactive (list (transient-args 'writing-habit-dispatch-generate)))
  (writing-habit-dispatch--require args "path" "week")
  (let ((table (writing-habit-dispatch--arg args "path"))
        (day (writing-habit-dispatch--arg args "week"))
        (dir (writing-habit-dispatch--arg args "dir")))
    (writing-habit-dispatch-run-schedule
     (writing-habit-dispatch--out-words args (list "generate-day" table day))
     (lambda () (writing-habit-dispatch--with-schedule-options args
                  (writing-schedule-batch-generate-day table day dir)))
     (writing-habit-dispatch--schedule-notes args))))

(transient-define-suffix writing-habit-dispatch--export (args)
  "Write the calendar file for a schedule file that was already generated."
  (interactive (list (transient-args 'writing-habit-dispatch-generate)))
  (writing-habit-dispatch--require args "schedule")
  (let ((sched (expand-file-name (writing-habit-dispatch--arg args "schedule"))))
    (writing-habit-dispatch-run-schedule
     (list "export" sched)
     (lambda () (writing-habit-dispatch--with-schedule-options args
                  (princ (format "Wrote iCalendar: %s\n" (writing-schedule-export-ics sched)))))
     (writing-habit-dispatch--schedule-notes args))))

(transient-define-suffix writing-habit-dispatch--weeks (args)
  "List the archived weekly schedule files."
  (interactive (list (transient-args 'writing-habit-dispatch-generate)))
  (let ((dir (writing-habit-dispatch--arg args "dir")))
    (writing-habit-dispatch-run-schedule
     (writing-habit-dispatch--out-words args (list "weeks"))
     (lambda () (writing-schedule-batch-list-weeks dir)))))

;;;###autoload (autoload 'writing-habit-dispatch-generate "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-generate ()
  "Turn the weekly table into a dated schedule and a calendar file."
  :init-value (lambda (obj) (oset obj value (append (and writing-habit-dispatch--last-table
                                 (list (concat "--path=" writing-habit-dispatch--last-table)))
                            (list (concat "--week=" (writing-habit-dispatch--today))))))
  ["Options"
   (writing-habit-dispatch--table)
   ("-w" "Week or day (any date)" "--week=" :reader writing-habit-dispatch--read-date)
   ("-o" "Output directory" "--dir=" :reader transient-read-directory)
   ("-s" "Schedule file to export" "--schedule=" :reader transient-read-existing-file)
   ("-S" "Refuse to write when blocks overlap" "--strict")
   ("-T" "Omit the TODO keyword" "--no-todo")]
  ["Run"
   ("g" "generate (week)" writing-habit-dispatch--generate)
   ("d" "generate (one day)" writing-habit-dispatch--generate-day)
   ("x" "export .ics" writing-habit-dispatch--export)
   ("l" "list archived weeks" writing-habit-dispatch--weeks)])

(transient-define-suffix writing-habit-dispatch--sheets (args)
  "Draw printable time-block sheets for the week."
  (interactive (list (transient-args 'writing-habit-dispatch-sheets)))
  (writing-habit-dispatch--require args "path" "week")
  (let ((table (writing-habit-dispatch--arg args "path"))
        (week (writing-habit-dispatch--arg args "week"))
        (dir (writing-habit-dispatch--arg args "dir"))
        (fmt (or (writing-habit-dispatch--arg args "format") "both"))
        (per-day (member "--per-day" args)))
    (writing-habit-dispatch-run-schedule
     (writing-habit-dispatch--out-words
      args (append (list "sheets" table week) (and per-day '("--per-day"))))
     (lambda () (writing-schedule-batch-timeblock-sheets table week (and per-day t) dir fmt)))))

(transient-define-suffix writing-habit-dispatch--sheet-day (args)
  "Draw the printable time-block sheet for one day."
  (interactive (list (transient-args 'writing-habit-dispatch-sheets)))
  (writing-habit-dispatch--require args "path" "week")
  (let ((table (writing-habit-dispatch--arg args "path"))
        (day (writing-habit-dispatch--arg args "week"))
        (dir (writing-habit-dispatch--arg args "dir"))
        (fmt (or (writing-habit-dispatch--arg args "format") "both")))
    (writing-habit-dispatch-run-schedule
     (writing-habit-dispatch--out-words args (list "sheet" table day fmt))
     (lambda () (writing-schedule-batch-timeblock-sheet-day table day dir fmt)))))

;;;###autoload (autoload 'writing-habit-dispatch-sheets "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch-sheets ()
  "Print time-block sheets from the weekly table."
  :init-value (lambda (obj) (oset obj value (append (and writing-habit-dispatch--last-table
                                 (list (concat "--path=" writing-habit-dispatch--last-table)))
                            (list (concat "--week=" (writing-habit-dispatch--today))
                                  "--format=both"))))
  ["Options"
   (writing-habit-dispatch--table)
   ("-w" "Week or day (any date)" "--week=" :reader writing-habit-dispatch--read-date)
   ("-o" "Output directory" "--dir=" :reader transient-read-directory)
   ("-F" "Format" "--format=" :choices ("pdf" "org" "both"))
   ("-p" "One PDF per day" "--per-day")]
  ["Run"
   ("s" "sheets (week)" writing-habit-dispatch--sheets)
   ("d" "sheet (one day)" writing-habit-dispatch--sheet-day)])

;;;; The top menu

(defun writing-habit-dispatch-show-log ()
  "Show the log of every command run from the menu."
  (interactive)
  (pop-to-buffer (writing-habit-dispatch--log-buffer)))

;;;###autoload (autoload 'writing-habit-dispatch "writing-habit-dispatch" nil t)
(transient-define-prefix writing-habit-dispatch ()
  "The weekly loop, from the table to the comparison of plan and practice."
  [:description
   (lambda ()
     (concat "writing-habit"
             (if writing-habit-dispatch--last-db
                 (format "   database: %s" (abbreviate-file-name writing-habit-dispatch--last-db))
               "")))
   ["Scheduler"
    :if writing-habit-dispatch--schedule-available-p
    ("s" "Schedule" writing-habit-dispatch-schedule)
    ("g" "Generate" writing-habit-dispatch-generate)
    ("S" "Sheets" writing-habit-dispatch-sheets)]
   ["Tracker"
    ("p" "Plan" writing-habit-dispatch-plan)
    ("t" "Track" writing-habit-dispatch-track)
    ("c" "Compare" writing-habit-dispatch-compare)]
   ["Across weeks"
    ("h" "History" writing-habit-dispatch-history)
    ("x" "Context" writing-habit-dispatch-context)
    ("n" "Seasons and names" writing-habit-dispatch-seasons)]
   ["Session"
    ("l" "Show the log" writing-habit-dispatch-show-log)]])

(provide 'writing-habit-dispatch)
;;; writing-habit-dispatch.el ends here
