;;; writing-habit-objectives-tests.el --- ERT tests for a block's objectives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ports the objective tests of the Python review panel: the rows of a
;; block, checking one off, the facts the coach sees, and Suggest, with
;; the model and the chooser replaced.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))

(require 'writing-habit-objectives)

(defun writing-habit-objectives-tests--db ()
  "Return the path of a new habit database in a temporary file."
  (let* ((path (make-temp-file "wh-objectives" nil ".db"))
         (db (writing-habit-db-connect path)))
    (writing-habit-db-init db)
    (writing-habit-db-close db)
    path))

(defun writing-habit-objectives-tests--block ()
  "Return a block of the R01 on Monday."
  (writing-habit-objectives-block-create
   :day "2026-11-02" :start "05:00" :end "07:00" :code "B" :name "1002RNAeditingR01"
   :due "2027-02-05" :minutes 120 :week-start "2026-11-02"
   :waiting '("find three citations")))

(ert-deftest writing-habit-objectives/add-days-crosses-a-month ()
  "Day offsets give the ISO date of the block."
  (should (equal (writing-habit-objectives-add-days "2026-11-02" 0) "2026-11-02"))
  (should (equal (writing-habit-objectives-add-days "2026-11-30" 3) "2026-12-03")))

(ert-deftest writing-habit-objectives/add-list-and-mark ()
  "Objectives keep their order, and a check is written at once."
  (skip-unless (sqlite-available-p))
  (let* ((path (writing-habit-objectives-tests--db))
         (db (writing-habit-db-connect path)))
    (unwind-protect
        (let ((first (writing-habit-objectives-add db "2026-11-02" "05:00" "B" " Draft aim 2 "))
              (_second (writing-habit-objectives-add db "2026-11-02" "05:00" "B" "Fix Fig 1" "ai")))
          (writing-habit-objectives-add db "2026-11-03" "05:00" "B" "Another day")
          (writing-habit-objectives-mark db first t)
          (let ((rows (writing-habit-objectives-rows db "2026-11-02" "05:00" "B")))
            (should (equal (mapcar (lambda (r) (nth 1 r)) rows) '("Draft aim 2" "Fix Fig 1")))
            (should (equal (mapcar (lambda (r) (nth 2 r)) rows) '(1 0)))
            (should (equal (nth 3 (nth 1 rows)) "ai")))
          (should-error (writing-habit-objectives-add db "2026-11-02" "05:00" "B" "  ")
                        :type 'user-error))
      (writing-habit-db-close db)
      (delete-file path))))

(ert-deftest writing-habit-objectives/facts-match-the-python-port ()
  "The facts carry the same lines, in the same order, as coach.objective_facts."
  (should (equal (writing-habit-objectives-facts
                  "work on   the R01" "1002RNAeditingR01" 120 "2027-02-05"
                  '("find three citations") "Draft the budget" "It keeps slipping.")
                 (concat "Task: work on the R01\n"
                         "Project: 1002RNAeditingR01\n"
                         "Block length: 120 minutes\n"
                         "Stuck: It keeps slipping. Break the task into smaller steps.\n"
                         "Project deadline: 2027-02-05\n"
                         "Last next-step note: Draft the budget\n"
                         "Waiting tasks for this project:\n"
                         "  - find three citations")))
  (should (equal (writing-habit-objectives-facts "t" "p" 30)
                 "Task: t\nProject: p\nBlock length: 30 minutes")))

(ert-deftest writing-habit-objectives/parse-keeps-three ()
  "At most three objectives are kept, and an empty reply is an error."
  (let ((reply (make-hash-table :test #'equal)))
    (puthash "objectives" '("a" "" "b" "c" "d") reply)
    (puthash "if_then" "If I stall, then I outline." reply)
    (should (equal (writing-habit-objectives-parse reply)
                   '(("a" "b" "c") . "If I stall, then I outline.")))
    (puthash "objectives" '() reply)
    (should-error (writing-habit-objectives-parse reply))))

(ert-deftest writing-habit-objectives/string-shows-checks-and-limit ()
  "The buffer text shows each check, the tally, and the limit note."
  (let ((text (writing-habit-objectives-string
               (writing-habit-objectives-tests--block)
               '((1 "One" 1 "typed") (2 "Two" 0 "ai") (3 "Three" 0 "typed") (4 "Four" 0 "typed"))
               "If I stall, then I outline.")))
    (should (string-match-p "Objectives for B 05:00-07:00 1002RNAeditingR01" text))
    (should (string-match-p "- \\[X\\] One" text))
    (should (string-match-p "- \\[ \\] Two   (ai)" text))
    (should (string-match-p "Met 1 of 4" text))
    (should (string-match-p "Three or fewer keep the block winnable" text))
    (should (string-match-p "If-then plan: If I stall" text))))

(ert-deftest writing-habit-objectives/suggest-saves-the-accepted ()
  "Suggest saves the accepted objectives with the origin ai and logs the call."
  (skip-unless (sqlite-available-p))
  (let ((path (writing-habit-objectives-tests--db))
        (sent nil)
        (kill-ring nil))
    (unwind-protect
        (cl-letf (((symbol-function 'writing-habit-coach-ask)
                   (lambda (_task facts &rest _)
                     (setq sent facts)
                     (let ((h (make-hash-table :test #'equal)))
                       (puthash "objectives" '("Draft aim 2" "List three citations") h)
                       (puthash "if_then" "If I stall, then I outline." h)
                       h)))
                  (writing-habit-coach-chooser (lambda (_title _note _items) '(0 2)))
                  (writing-habit-coach-ai-off nil))
          (should (= (writing-habit-objectives-suggest
                      (writing-habit-objectives-tests--block) path "work on the R01")
                     1))
          (should (string-match-p "Waiting tasks for this project:\n  - find three citations" sent))
          (should (equal (car kill-ring) "If I stall, then I outline."))
          (let ((db (writing-habit-db-connect path)))
            (unwind-protect
                (progn
                  (should (equal (mapcar #'cdr (mapcar (lambda (r) (list (nth 1 r) (nth 3 r)))
                                                       (writing-habit-objectives-rows
                                                        db "2026-11-02" "05:00" "B")))
                                 '(("ai"))))
                  (should (equal (car (sqlite-select db "SELECT task, accepted, week_start FROM ai_log"))
                                 '("objectives" 1 "2026-11-02"))))
              (writing-habit-db-close db))))
      (when (get-buffer "*writing-habit objectives*")
        (kill-buffer "*writing-habit objectives*"))
      (delete-file path))))

(ert-deftest writing-habit-objectives/suggest-quiet-in-an-ai-off-week ()
  "Suggest refuses in an AI-off week."
  (let ((writing-habit-coach-ai-off t))
    (should-error (writing-habit-objectives-suggest
                   (writing-habit-objectives-tests--block) "/nonexistent.db" "x")
                  :type 'user-error)))

(provide 'writing-habit-objectives-tests)
;;; writing-habit-objectives-tests.el ends here
