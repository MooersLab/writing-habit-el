;;; writing-habit-main-tests.el --- ERT tests for the aggregator and CLI -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the command-line argument parser, the batch dispatch, and the
;; cross-port interoperability guard: a database built by the Python port,
;; committed as a fixture, read back through the Elisp layer.  Run from the
;; repository root with:
;;
;;   emacs -batch -L elisp -l ert \
;;     -l elisp/test/writing-habit-main-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit)

;; Best effort to make writing-schedule.el available for the plan dispatch test.
(unless (require 'writing-schedule nil t)
  (let ((dir (getenv "WRITING_SCHEDULE_DIR")))
    (when (and dir (file-directory-p dir))
      (add-to-list 'load-path dir)
      (require 'writing-schedule nil t))))

(defconst writing-habit-main-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst writing-habit-main-tests--root
  (locate-dominating-file writing-habit-main-tests--dir "schema.sql")
  "Repository root, found by walking up to schema.sql, so tests work in any layout.")

(defconst writing-habit-main-tests--schema
  (expand-file-name "schema.sql" writing-habit-main-tests--root))

(defconst writing-habit-main-tests--table
  (expand-file-name "examples/my-week.org" writing-habit-main-tests--root))

(defconst writing-habit-main-tests--csv
  (expand-file-name "examples/actuals.csv" writing-habit-main-tests--root))

(defconst writing-habit-main-tests--xport-db
  (expand-file-name "fixtures/cross-port.db" writing-habit-main-tests--dir)
  "A database built by the Python port, committed as a fixture.")


;;;; Argument parser

(ert-deftest writing-habit-parse-args ()
  "Bare tokens become positionals; --flag value pairs become options."
  (should (equal (writing-habit--parse-args '("initdb" "--db" "x.db"))
                 '(("initdb") . (("db" . "x.db")))))
  (should (equal (writing-habit--parse-args
                  '("plan" "import" "my-week.org" "--week" "2026-01-19" "--db" "x.db"))
                 '(("plan" "import" "my-week.org")
                   . (("week" . "2026-01-19") ("db" . "x.db")))))
  (should-error (writing-habit--parse-args '("initdb" "--db"))))       ; missing value


;;;; Dispatch

(ert-deftest writing-habit-dispatch-name ()
  "The name command needs no database."
  (let ((out (writing-habit--dispatch '("name" "4gAAeAsA-gWW"))))
    (should (string-match-p "Schedule 4gAAeAsA-gWW" out))
    (should (string-match-p "18 blocks over 5 days" out))))

(ert-deftest writing-habit-dispatch-errors ()
  "Unknown commands and missing required options are errors."
  (should-error (writing-habit--dispatch '("frobnicate")))
  (should-error (writing-habit--dispatch '("initdb")))               ; no --db
  (should-error (writing-habit--dispatch '("compare" "--week" "2026-01-19"))))  ; no --db

(ert-deftest writing-habit-dispatch-initdb-track-compare ()
  "A full initdb, manual track add, and compare cycle over a temp database."
  (skip-unless (sqlite-available-p))
  (let ((dbf (make-temp-file "wh-cli" nil ".db")))
    (unwind-protect
        (progn
          (should (string-prefix-p "Initialized"
                                   (writing-habit--dispatch (list "initdb" "--db" dbf))))
          (should (string-match-p
                   "Imported 7 sessions"
                   (writing-habit--dispatch
                    (list "track" "import" writing-habit-main-tests--csv
                          "--format" "csv" "--db" dbf))))
          (should (string-prefix-p
                   "Added session"
                   (writing-habit--dispatch
                    (list "track" "add" "--day" "2026-01-22" "--project" "A"
                          "--minutes" "40" "--category" "generative" "--db" dbf))))
          (let ((report (writing-habit--dispatch
                         (list "compare" "--week" "2026-01-19" "--db" dbf))))
            (should (string-match-p "#\\+TITLE: Writing week beginning 2026-01-19" report))
            (should (string-match-p "streak of consecutive writing days" report))))
      (delete-file dbf))))

(ert-deftest writing-habit-dispatch-plan-import ()
  "The plan import command runs through the writing-schedule parser."
  (skip-unless (sqlite-available-p))
  (skip-unless (featurep 'writing-schedule))
  (let ((dbf (make-temp-file "wh-cli-plan" nil ".db")))
    (unwind-protect
        (progn
          (writing-habit--dispatch (list "initdb" "--db" dbf))
          (should (string-match-p
                   "Imported 22 planned blocks"
                   (writing-habit--dispatch
                    (list "plan" "import" writing-habit-main-tests--table
                          "--week" "2026-01-19" "--db" dbf)))))
      (delete-file dbf))))


;;;; Cross-port interoperability guard

(ert-deftest writing-habit-reads-python-built-database ()
  "The Elisp compare layer reads a database written by the Python port."
  (skip-unless (sqlite-available-p))
  (skip-unless (file-exists-p writing-habit-main-tests--xport-db))
  (let ((db (writing-habit-db-connect writing-habit-main-tests--xport-db)))
    (unwind-protect
        (let* ((proj (writing-habit-compare-week-project db "2026-01-19"))
               (a (seq-find (lambda (r) (equal (cdr (assoc "code" r)) "A")) proj))
               (w (seq-find (lambda (r) (equal (cdr (assoc "code" r)) "W")) proj)))
          (should (= (length proj) 5))
          (should (= (cdr (assoc "planned_min" a)) 720))
          (should (= (cdr (assoc "actual_min" a)) 310))
          (should (< (abs (- (cdr (assoc "adherence" a)) 0.43)) 0.001))
          (should (equal (cdr (assoc "risk_class" w)) "speculative"))
          (should (= (writing-habit-compare-current-streak db) 3)))
      (writing-habit-db-close db))))

(provide 'writing-habit-main-tests)
;;; writing-habit-main-tests.el ends here
