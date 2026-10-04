;;; writing-habit-dispatch-tests.el --- ERT tests for the command center -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests the argument translation, the runners, the log, and the suffixes
;; of writing-habit-dispatch.el.  The suffixes are called as functions with
;; an argument list, which is what the menu passes them.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit)
(require 'writing-habit-dispatch)

(defconst writing-habit-dispatch-tests--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this file.")

(defconst writing-habit-dispatch-tests--schedule
  (and (require 'writing-schedule nil t) (fboundp 'writing-schedule-split-row))
  "Non-nil when writing-schedule.el 0.3.1 is available.")

(defmacro writing-habit-dispatch-tests--with-dir (var &rest body)
  "Bind VAR to a fresh temporary directory around BODY, with a clean log."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "wh-dispatch" t)))
         (writing-habit-dispatch-auto-preview nil)
         (writing-habit-dispatch--last-db nil))
     (when (get-buffer "*writing-habit log*") (kill-buffer "*writing-habit log*"))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun writing-habit-dispatch-tests--log ()
  "Return the text of the log buffer."
  (with-current-buffer (writing-habit-dispatch--log-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest writing-habit-dispatch/parse ()
  "Transient arguments become the words of the command line."
  (should (equal (writing-habit-dispatch--parse
                  '("--db=/tmp/h.db" "--path=week.org" "--week=2026-01-19" "--strict"))
                 '(("week.org") "--db" "/tmp/h.db" "--week" "2026-01-19" "--strict")))
  (should (equal (writing-habit-dispatch--parse '("--code=4gA" "--table=t.org"))
                 '(("4gA") "--table" "t.org"))))

(ert-deftest writing-habit-dispatch/shell-line-quotes ()
  "A word with a space is quoted in the shell line."
  (should (equal (writing-habit-dispatch--shell-line '("writing-habit" "--db" "my file.db"))
                 "writing-habit --db my\\ file.db")))

(ert-deftest writing-habit-dispatch/written-files ()
  "Both scheduler and tracker wordings of a written file are found."
  (should (equal (writing-habit-dispatch--written-files
                  (concat "Wrote schedule:  /tmp/a.org\nWrote iCalendar: /tmp/a.ics\n"
                          "Wrote dashboard to /tmp/w.html\nother\n"))
                 '("/tmp/a.org" "/tmp/a.ics" "/tmp/w.html"))))

(ert-deftest writing-habit-dispatch/seed-uses-last-db ()
  "The forms are seeded with the database used last, then the default."
  (let ((writing-habit-dispatch--last-db nil)
        (writing-habit-default-db "/tmp/default.db"))
    (should (equal (writing-habit-dispatch--seed "--x=1") '("--db=/tmp/default.db" "--x=1")))
    (setq writing-habit-dispatch--last-db "/tmp/last.db")
    (should (equal (writing-habit-dispatch--seed) '("--db=/tmp/last.db")))))

(ert-deftest writing-habit-dispatch/tracker-loop ()
  "initdb, track add, compare, dashboard, history, and context run and log."
  (writing-habit-dispatch-tests--with-dir dir
    (let ((db (concat dir "h.db"))
          (html (concat dir "week.html")))
      (writing-habit-dispatch--initdb (list (concat "--db=" db) "--week=2026-01-19"))
      (should (file-exists-p db))
      (should (equal writing-habit-dispatch--last-db db))
      (writing-habit-dispatch--track-add
       (list (concat "--db=" db) "--day=2026-01-19" "--project=A" "--minutes=60"
             "--category=generative" "--format=csv"))
      (writing-habit-dispatch--dashboard
       (list (concat "--db=" db) "--week=2026-01-19" (concat "--out=" html)))
      (should (file-exists-p html))
      (writing-habit-dispatch--history (list (concat "--db=" db)))
      (writing-habit-dispatch--context-set
       (list (concat "--db=" db) "--week=2026-01-19" "--tag=teaching"))
      (let ((log (writing-habit-dispatch-tests--log)))
        (should (string-match-p (regexp-quote (concat "$ writing-habit initdb --db " db)) log))
        (should (string-match-p "\\$ emacs --batch -l writing-habit -f writing-habit-batch track add" log))
        (should (string-match-p (regexp-quote (concat "Wrote " html)) log))
        (should (string-match-p "Weekly adherence history" log))
        (should (string-match-p "Tagged the week of 2026-01-19 with teaching" log))))))

(ert-deftest writing-habit-dispatch/errors-are-logged ()
  "A failing command is logged as an error rather than signalled."
  (writing-habit-dispatch-tests--with-dir dir
    (writing-habit-dispatch-run-habit '("compare") (list (concat "--db=" dir "x.db")))
    (should (string-match-p "error: Missing required option --week"
                            (writing-habit-dispatch-tests--log)))))

(ert-deftest writing-habit-dispatch/missing-option-asks ()
  "A suffix names the option it still needs."
  (should-error (writing-habit-dispatch--plan-import '("--db=/tmp/x.db")) :type 'user-error)
  (should-error (writing-habit-dispatch--track-add '("--db=/tmp/x.db" "--day=2026-01-19"
                                                     "--project=A"))
                :type 'user-error))

(ert-deftest writing-habit-dispatch/plan-import ()
  "plan import reads a table through the scheduler parser."
  (skip-unless writing-habit-dispatch-tests--schedule)
  (writing-habit-dispatch-tests--with-dir dir
    (let ((db (concat dir "h.db"))
          (table (expand-file-name "fixtures/my-week-named.org" writing-habit-dispatch-tests--dir)))
      (writing-habit-dispatch--initdb (list (concat "--db=" db)))
      (writing-habit-dispatch--plan-import
       (list (concat "--db=" db) (concat "--path=" table) "--week=2026-01-19"))
      (should (string-match-p "Imported [0-9]+ planned blocks"
                              (writing-habit-dispatch-tests--log))))))

(ert-deftest writing-habit-dispatch/scheduler-check-and-template ()
  "The scheduler suffixes log the writing-schedule.sh line and the output."
  (skip-unless writing-habit-dispatch-tests--schedule)
  (writing-habit-dispatch-tests--with-dir dir
    (let ((table (concat dir "t.org")))
      (cl-letf (((symbol-function 'find-file) #'ignore))
        (writing-habit-dispatch--template (list "--projects=2" (concat "--file=" table))))
      (should (file-exists-p table))
      (writing-habit-dispatch--check (list (concat "--path=" table)))
      (let ((log (writing-habit-dispatch-tests--log)))
        (should (string-match-p (regexp-quote (concat "$ writing-schedule.sh template 2 " table)) log))
        (should (string-match-p (regexp-quote (concat "$ writing-schedule.sh check " table)) log))
        (should (string-match-p "No overlapping time blocks" log))))))

(ert-deftest writing-habit-dispatch/generate-notes-settings ()
  "Generate binds the strict and no-todo settings and says so in the log."
  (skip-unless writing-habit-dispatch-tests--schedule)
  (writing-habit-dispatch-tests--with-dir dir
    (let ((table (expand-file-name "fixtures/my-week-named.org" writing-habit-dispatch-tests--dir))
          (seen nil))
      (cl-letf (((symbol-function 'writing-schedule-batch-generate)
                 (lambda (&rest _) (setq seen (list writing-schedule-overlap-action
                                                    writing-schedule-use-todo)))))
        (writing-habit-dispatch--generate
         (list (concat "--path=" table) "--week=2026-01-19" (concat "--dir=" dir)
               "--strict" "--no-todo")))
      (should (equal seen '(error nil)))
      (let ((log (writing-habit-dispatch-tests--log)))
        (should (string-match-p "\\$ WS_OUT_DIR=.* writing-schedule.sh generate" log))
        (should (string-match-p "overlap-action bound to error" log))))))

(ert-deftest writing-habit-dispatch/menus-are-commands ()
  "Every menu is an interactive command, and writing-habit opens the top one."
  (dolist (cmd '(writing-habit-dispatch writing-habit-dispatch-plan writing-habit-dispatch-track
                 writing-habit-dispatch-compare writing-habit-dispatch-history
                 writing-habit-dispatch-context writing-habit-dispatch-seasons
                 writing-habit-dispatch-schedule writing-habit-dispatch-generate
                 writing-habit-dispatch-sheets))
    (should (commandp cmd)))
  (should (eq (indirect-function 'writing-habit) (indirect-function 'writing-habit-dispatch))))

(ert-deftest writing-habit-dispatch/forms-seed-their-values ()
  "Opening a form fills in the database and today's week."
  (let ((writing-habit-dispatch--last-db "/tmp/last.db")
        (obj (get 'writing-habit-dispatch-compare 'transient--prefix)))
    (should obj)
    (let ((copy (clone obj)))
      (funcall (oref copy init-value) copy)
      (should (member "--db=/tmp/last.db" (oref copy value)))
      (should (member (concat "--week=" (format-time-string "%F")) (oref copy value))))))

(provide 'writing-habit-dispatch-tests)
;;; writing-habit-dispatch-tests.el ends here
