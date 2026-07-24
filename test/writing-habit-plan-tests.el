;;; writing-habit-plan-tests.el --- ERT tests for writing-habit-plan -*- lexical-binding: t; -*-

;;; Commentary:

;; Ports the plan-import behavior of the Python package to ERT and adds
;; coverage for the pure helpers.  Run from the repository root with:
;;
;;   emacs -batch -L elisp -l ert \
;;     -l elisp/test/writing-habit-plan-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The import tests need writing-schedule.el.  They try `require' first, then
;; the directory named by the WRITING_SCHEDULE_DIR environment variable, and
;; skip themselves when neither makes the package available.  The pure-helper
;; tests always run.

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-db)
(require 'writing-habit-plan)

;; Best effort to make writing-schedule.el loadable for the import tests.
(unless (require 'writing-schedule nil t)
  (let ((dir (getenv "WRITING_SCHEDULE_DIR")))
    (when (and dir (file-directory-p dir))
      (add-to-list 'load-path dir)
      (require 'writing-schedule nil t))))

(defconst writing-habit-plan-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst writing-habit-plan-tests--root
  (locate-dominating-file writing-habit-plan-tests--dir "schema.sql")
  "Repository root, found by walking up to schema.sql, so tests work in any layout.")

(defconst writing-habit-plan-tests--schema
  (expand-file-name "schema.sql" writing-habit-plan-tests--root))

(defconst writing-habit-plan-tests--table
  (expand-file-name "examples/my-week.org" writing-habit-plan-tests--root))

(defun writing-habit-plan-tests--fresh ()
  "Return a fresh in-memory database with the schema loaded."
  (let ((db (writing-habit-db-connect)))
    (writing-habit-db-init db writing-habit-plan-tests--schema)
    db))


;;;; Pure helpers, no database and no writing-schedule needed

(ert-deftest writing-habit-plan-split-risk ()
  "A trailing risk tag is split out in either form, leaving a clean name."
  (should (equal (writing-habit-plan--split-risk "DNPH1 docking :safe:")
                 '("DNPH1 docking" . "safe")))
  (should (equal (writing-habit-plan--split-risk "2026words (speculative)")
                 '("2026words" . "speculative")))
  (should (equal (writing-habit-plan--split-risk "email :SUPPORT:")
                 '("email")))
  (should (equal (writing-habit-plan--split-risk "no tag here")
                 '("no tag here" . nil)))
  (should (equal (writing-habit-plan--split-risk nil) '(nil)))
  (should (equal (writing-habit-plan--split-risk "   ") '(nil))))

(ert-deftest writing-habit-plan-category-for ()
  "Section headers map to the three activities; unknown ones fall back."
  (should (equal (writing-habit-plan--category-for "Generative") '("generative" . t)))
  (should (equal (writing-habit-plan--category-for "Rewriting") '("editing" . t)))
  (should (equal (writing-habit-plan--category-for "Supporting") '("support" . t)))
  (should (equal (writing-habit-plan--category-for "Writing") '("generative" . t)))
  (should (equal (writing-habit-plan--category-for "Meetings") '("generative" . nil))))


;;;; Import against the real writing-schedule parser

(ert-deftest writing-habit-plan-import-counts-and-days ()
  "Every filled block becomes a plan_block on the right day."
  (skip-unless (sqlite-available-p))
  (skip-unless (featurep 'writing-schedule))
  (let ((db (writing-habit-plan-tests--fresh)))
    (unwind-protect
        (let ((n (writing-habit-plan-import db writing-habit-plan-tests--table "2026-01-19")))
          (should (= n 22))
          ;; The Monday 04:00 generative block for A lands with the right category.
          (let ((row (car (sqlite-select
                           db (concat "SELECT p.code, c.name FROM plan_block b "
                                      "JOIN project p ON p.project_id=b.project_id "
                                      "JOIN category c ON c.category_id=b.category_id "
                                      "WHERE b.day='2026-01-19' AND b.start_time='04:00'")))))
            (should (equal (nth 0 row) "A"))
            (should (equal (nth 1 row) "generative")))
          ;; All rows fall in the one week.
          (should (= n (caar (sqlite-select
                              db "SELECT COUNT(*) FROM plan_block WHERE week_start='2026-01-19'")))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-plan-import-strips-risk-and-maps-sections ()
  "Risk tags are stripped into risk_class, and each section maps correctly."
  (skip-unless (sqlite-available-p))
  (skip-unless (featurep 'writing-schedule))
  (let ((db (writing-habit-plan-tests--fresh)))
    (unwind-protect
        (progn
          (writing-habit-plan-import db writing-habit-plan-tests--table "2026-01-19")
          ;; A: description has no tag; risk_class is safe.
          (let ((a (car (sqlite-select
                         db "SELECT description, risk_class FROM project WHERE code='A'"))))
            (should (equal (nth 0 a) "DNPH1 docking"))
            (should (equal (nth 1 a) "safe")))
          ;; W is speculative.
          (should (equal (caar (sqlite-select
                                db "SELECT risk_class FROM project WHERE code='W'"))
                         "speculative"))
          ;; A Rewriting block is editing; a Supporting block is support.
          (should (equal (caar (sqlite-select
                                db (concat "SELECT c.name FROM plan_block b "
                                           "JOIN category c ON c.category_id=b.category_id "
                                           "WHERE b.start_time='09:15' LIMIT 1")))
                         "editing"))
          (should (equal (caar (sqlite-select
                                db (concat "SELECT c.name FROM plan_block b "
                                           "JOIN category c ON c.category_id=b.category_id "
                                           "WHERE b.start_time='13:15' LIMIT 1")))
                         "support")))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-plan-import-is-idempotent ()
  "A second import of the same week adds no rows."
  (skip-unless (sqlite-available-p))
  (skip-unless (featurep 'writing-schedule))
  (let ((db (writing-habit-plan-tests--fresh)))
    (unwind-protect
        (let ((first (writing-habit-plan-import db writing-habit-plan-tests--table "2026-01-19"))
              (second (writing-habit-plan-import db writing-habit-plan-tests--table "2026-01-19")))
          (should (= first 22))
          (should (= second first)))
      (writing-habit-db-close db))))

(provide 'writing-habit-plan-tests)
;;; writing-habit-plan-tests.el ends here
