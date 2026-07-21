;;; writing-habit-db-tests.el --- ERT tests for writing-habit-db -*- lexical-binding: t; -*-

;;; Commentary:

;; Ports tests/test_schema.py to ERT and adds coverage for the ported db API.
;; Run from the repository root with:
;;
;;   emacs -batch -L elisp -l ert \
;;     -l elisp/test/writing-habit-db-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; These tests are skipped when Emacs was built without SQLite support.

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-db)

(defconst writing-habit-db-tests--root
  (locate-dominating-file
   (file-name-directory (or load-file-name buffer-file-name)) "schema.sql")
  "Repository root, found by walking up to schema.sql, so tests work in any layout.")

(defconst writing-habit-db-tests--schema
  (expand-file-name "schema.sql" writing-habit-db-tests--root)
  "The shared schema, reused verbatim from the Python package.")

(defun writing-habit-db-tests--fresh ()
  "Return a fresh in-memory database with the schema loaded."
  (let ((db (writing-habit-db-connect)))
    (writing-habit-db-init db writing-habit-db-tests--schema)
    db))

(defmacro writing-habit-db-tests--with-db (var &rest body)
  "Bind VAR to a fresh in-memory database, run BODY, then close it."
  (declare (indent 1))
  `(let ((,var (writing-habit-db-tests--fresh)))
     (unwind-protect (progn ,@body)
       (writing-habit-db-close ,var))))


(ert-deftest writing-habit-db-tables-and-views-exist ()
  "All five tables and four views are created by the schema."
  (skip-unless (sqlite-available-p))
  (writing-habit-db-tests--with-db db
    (let ((names (mapcar #'car
                         (sqlite-select
                          db "SELECT name FROM sqlite_master WHERE type IN ('table','view')"))))
      (dolist (expected '("category" "project" "plan_block" "session" "import_log"
                          "v_week_project" "v_week_category" "v_week_barbell" "v_day_actual"))
        (should (member expected names))))))

(ert-deftest writing-habit-db-generated-columns ()
  "planned_min and week_start are computed by the generated columns."
  (skip-unless (sqlite-available-p))
  (writing-habit-db-tests--with-db db
    (sqlite-execute db "INSERT INTO project(code, risk_class) VALUES ('A','safe')")
    (let ((pid (caar (sqlite-select db "SELECT project_id FROM project"))))
      (sqlite-execute
       db (concat "INSERT INTO plan_block(day, start_time, end_time, project_id, category_id)"
                  " VALUES ('2026-01-21','04:00','05:30',?,1)")
       (list pid))
      (let ((row (car (sqlite-select db "SELECT planned_min, week_start FROM plan_block"))))
        (should (= (nth 0 row) 90))
        (should (equal (nth 1 row) "2026-01-19"))))))  ; Monday of that week

(ert-deftest writing-habit-db-foreign-key-enforced ()
  "A session that names a missing project is rejected."
  (skip-unless (sqlite-available-p))
  (writing-habit-db-tests--with-db db
    (should-error
     (sqlite-execute
      db (concat "INSERT INTO session(day, actual_min, project_id, source)"
                 " VALUES ('2026-01-19', 10, 999, 'manual')"))
     :type 'sqlite-error)))

(ert-deftest writing-habit-db-get-category-id ()
  "The seeded activities resolve, and blank or unknown names return nil."
  (skip-unless (sqlite-available-p))
  (writing-habit-db-tests--with-db db
    (should (= (writing-habit-db-get-category-id db "generative") 1))
    (should (= (writing-habit-db-get-category-id db "editing") 2))
    (should (= (writing-habit-db-get-category-id db "support") 3))
    (should (null (writing-habit-db-get-category-id db "")))
    (should (null (writing-habit-db-get-category-id db nil)))
    (should (null (writing-habit-db-get-category-id db "nonsense")))))

(ert-deftest writing-habit-db-get-or-create-project-backfills ()
  "A bare project is enriched later, but an existing value is never replaced."
  (skip-unless (sqlite-available-p))
  (writing-habit-db-tests--with-db db
    (let ((pid (writing-habit-db-get-or-create-project db "A")))
      (should (integerp pid))
      (should (= pid (writing-habit-db-get-or-create-project db "A")))
      ;; The schedule legend enriches the empty fields.
      (writing-habit-db-get-or-create-project db "A" "DNPH1 docking" "safe")
      (let ((row (car (sqlite-select
                       db "SELECT description, risk_class FROM project WHERE code='A'"))))
        (should (equal (nth 0 row) "DNPH1 docking"))
        (should (equal (nth 1 row) "safe")))
      ;; A later, different value must not overwrite the existing one.
      (writing-habit-db-get-or-create-project db "A" "changed" "speculative")
      (let ((row (car (sqlite-select
                       db "SELECT description, risk_class FROM project WHERE code='A'"))))
        (should (equal (nth 0 row) "DNPH1 docking"))
        (should (equal (nth 1 row) "safe"))))))

(ert-deftest writing-habit-db-minutes-between ()
  "Minute arithmetic matches the schema's planned_min expression."
  (should (= (writing-habit-db-minutes-between "04:00" "05:30") 90))
  (should (= (writing-habit-db-minutes-between "09:15" "10:45") 90))
  (should (= (writing-habit-db-minutes-between "13:00" "13:00") 0)))

(ert-deftest writing-habit-db-log-import ()
  "An import row records its provenance."
  (skip-unless (sqlite-available-p))
  (writing-habit-db-tests--with-db db
    (writing-habit-db-log-import db "org" "my-week.org" 5 5 "0.1.0" "test")
    (let ((row (car (sqlite-select
                     db (concat "SELECT source_type, source_name, rows_read, rows_inserted"
                                " FROM import_log")))))
      (should (equal (nth 0 row) "org"))
      (should (equal (nth 1 row) "my-week.org"))
      (should (= (nth 2 row) 5))
      (should (= (nth 3 row) 5)))))

(provide 'writing-habit-db-tests)
;;; writing-habit-db-tests.el ends here
