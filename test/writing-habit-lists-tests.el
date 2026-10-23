;;; writing-habit-lists-tests.el --- Tests for the semester project lists -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The list model is pure, so these tests need no writing-schedule and no
;; database.  They mirror tests/test_project_lists.py of the Python package.

;;; Code:

(require 'ert)
(require 'writing-habit-lists)

(defconst writing-habit-lists-tests--after
  '(""
    "#+NAME: current-semester-projects"
    "| A: 1094RNAstrucDetResearchProgram :safe: |"
    "| B: 1006AIrxOpt 2026-12-15 :safe:         |"
    "| X: 0374qflip 2026-11-21 :risky:          |"
    ""
    "#+NAME: next-semester-projects"
    "| - 1452PHFSboXt 2027-03-01 :safe:          |"
    "| Z: 1008ToolsForQCrOfRNA @editing :safe:   |")
  "The lines after a weekly table that carries two lists.")

(ert-deftest writing-habit-lists/read ()
  "Lists are read with their codes, dashes, deadlines, and activities."
  (let ((lines writing-habit-lists-tests--after))
    (should (equal (writing-habit-lists-present lines)
                   '("current-semester-projects" "next-semester-projects")))
    (should (equal (mapcar #'writing-habit-lists-entry-code
                           (writing-habit-lists-entries lines writing-habit-lists-current))
                   '("A" "B" "X")))
    (let ((first (car (writing-habit-lists-entries lines "next-semester-projects")))
          (second (cadr (writing-habit-lists-entries lines "next-semester-projects"))))
      (should-not (writing-habit-lists-entry-code first))
      (should (equal (writing-habit-lists-entry-project first) "1452PHFSboXt"))
      (should (equal (writing-habit-lists-entry-deadline first) "2027-03-01"))
      (should (equal (writing-habit-lists-entry-risk first) "safe"))
      (should (equal (writing-habit-lists-entry-activity second) "editing")))
    (should-not (writing-habit-lists-entries lines "next-next-semester-projects"))))

(ert-deftest writing-habit-lists/set-entry-rewrites-one-row ()
  "Editing an entry changes only its own line and keeps the cell width."
  (let* ((lines writing-habit-lists-tests--after)
         (entry (car (writing-habit-lists-entries lines writing-habit-lists-current))))
    (setf (writing-habit-lists-entry-description entry) "1094RNAstrucDetResearchProgram 2027-05-01")
    (let ((new (writing-habit-lists-set-entry lines writing-habit-lists-current 0 entry)))
      (should (equal (cl-remove-if (lambda (pair) (equal (car pair) (cdr pair)))
                                   (cl-mapcar #'cons new lines))
                     '(("| A: 1094RNAstrucDetResearchProgram 2027-05-01 :safe: |"
                        . "| A: 1094RNAstrucDetResearchProgram :safe: |")))))))

(ert-deftest writing-habit-lists/insert-move-remove-and-create ()
  "A list is created in horizon order, and rows insert, move, and leave."
  (let ((lines '()))
    (setq lines (car (writing-habit-lists-insert
                      lines "next-next-semester-projects" nil
                      (writing-habit-lists-entry-create :description "0001idea"))))
    (setq lines (car (writing-habit-lists-insert
                      lines writing-habit-lists-current nil
                      (writing-habit-lists-entry-create :code "Q" :description "3292ABRF"
                                                        :risk "safe"))))
    (setq lines (car (writing-habit-lists-insert
                      lines writing-habit-lists-current 0
                      (writing-habit-lists-entry-create :code "R"
                                                        :description "0128NoSpherA2qRNA"))))
    (should (< (cl-position "#+NAME: current-semester-projects" lines :test #'equal)
               (cl-position "#+NAME: next-next-semester-projects" lines :test #'equal)))
    (let ((moved (writing-habit-lists-move lines writing-habit-lists-current 0 nil)))
      (should (= (cdr moved) 1))
      (setq lines (car moved)))
    (should (equal (mapcar #'writing-habit-lists-entry-code
                           (writing-habit-lists-entries lines writing-habit-lists-current))
                   '("Q" "R")))
    (let ((removed (writing-habit-lists-remove lines writing-habit-lists-current 0)))
      (should (equal (writing-habit-lists-entry-code (cdr removed)) "Q"))
      (setq lines (car removed)))
    (should (equal (mapcar #'writing-habit-lists-entry-text
                           (writing-habit-lists-entries lines writing-habit-lists-current))
                   '("R: 0128NoSpherA2qRNA")))))

(ert-deftest writing-habit-lists/sort-by-number ()
  "A sort orders by project number, with unnumbered projects last."
  (let* ((lines (car (writing-habit-lists-insert
                      writing-habit-lists-tests--after writing-habit-lists-current nil
                      (writing-habit-lists-entry-create :code "E" :description "email"
                                                        :activity "support"))))
         (sorted (writing-habit-lists-sort lines writing-habit-lists-current)))
    (should (cdr sorted))
    (should (equal (mapcar #'writing-habit-lists-entry-project
                           (writing-habit-lists-entries (car sorted) writing-habit-lists-current))
                   '("0374qflip" "1006AIrxOpt" "1094RNAstrucDetResearchProgram" "email")))
    (should-not (cdr (writing-habit-lists-sort (car sorted) writing-habit-lists-current)))))

(ert-deftest writing-habit-lists/transfer-to-completed ()
  "Marking a project completed moves it once and never duplicates it."
  (let* ((moved (writing-habit-lists-transfer writing-habit-lists-tests--after
                                              writing-habit-lists-current 1
                                              writing-habit-lists-completed))
         (lines (car moved)))
    (should (equal (writing-habit-lists-entry-project (cdr moved)) "1006AIrxOpt"))
    (should (equal (mapcar #'writing-habit-lists-entry-code
                           (writing-habit-lists-entries lines writing-habit-lists-current))
                   '("A" "X")))
    (should (equal (mapcar #'writing-habit-lists-entry-text
                           (writing-habit-lists-entries lines writing-habit-lists-completed))
                   '("B: 1006AIrxOpt 2026-12-15 :safe:")))
    (should (< (cl-position "#+NAME: completed-this-semester" lines :test #'equal)
               (cl-position "#+NAME: next-semester-projects" lines :test #'equal)))
    (let ((again (writing-habit-lists-insert lines writing-habit-lists-current nil
                                             (writing-habit-lists-entry-create
                                              :code "B" :description "1006AIrxOpt 2026-12-15"))))
      (setq lines (car (writing-habit-lists-transfer (car again) writing-habit-lists-current
                                                     2 writing-habit-lists-completed))))
    (should (= (length (writing-habit-lists-entries lines writing-habit-lists-completed)) 1))))

(ert-deftest writing-habit-lists/semester-numbers-and-a-row-without-a-closing-bar ()
  "The current and completed lists both count, even a row missing its last bar."
  (let ((lines (append writing-habit-lists-tests--after
                       '("" "#+NAME: completed-this-semester"
                         "| Q: 3292ABRF-SC3-2026 2026-10-09 :safe: |"
                         "| F: 4072UsersMeeting2026 2026-09-23 :safe:"))))
    (should (equal (mapcar #'writing-habit-lists-entry-deadline
                           (writing-habit-lists-entries lines writing-habit-lists-completed))
                   '("2026-10-09" "2026-09-23")))
    (should (equal (sort (writing-habit-lists-semester-numbers lines) #'string<)
                   '("0374" "1006" "1094" "3292" "4072")))))

(ert-deftest writing-habit-lists/digit-codes ()
  "A list entry may carry a digit code."
  (let ((lines (car (writing-habit-lists-insert
                     '() "next-next-next-next-semester-projects" nil
                     (writing-habit-lists-entry-create :code "7"
                                                       :description "1452PHFSboXt 2027-03-01")))))
    (should (equal (writing-habit-lists-entry-code
                    (car (writing-habit-lists-entries lines "next-next-next-next-semester-projects")))
                   "7"))))

(ert-deftest writing-habit-lists/seven-horizons-reach-two-years-out ()
  "The two furthest lists are read, kept in horizon order, and show their titles.
These match the lists of the Python editor, so a file edited in either
package keeps the same lists."
  (should (equal (last (writing-habit-lists-names) 2)
                 '("next-next-next-next-next-semester-projects"
                   "next-next-next-next-next-next-semester-projects")))
  (let ((lines '("" "#+NAME: next-next-next-next-next-next-semester-projects"
                 "#+CAPTION: Fall 2028 projects"
                 "| Z: 1008ToolsForQCrOfRNA 2028-11-05 :safe: |"
                 "" "#+NAME: next-next-next-next-next-semester-projects"
                 "#+CAPTION: Summer 2028 projects"
                 "| Y: 1008ToolsForQCrOfRNA 2028-11-05 :safe: |")))
    (should (equal (writing-habit-lists-present lines)
                   '("next-next-next-next-next-semester-projects"
                     "next-next-next-next-next-next-semester-projects")))
    (should (equal (writing-habit-lists-entry-code
                    (car (writing-habit-lists-entries
                          lines "next-next-next-next-next-next-semester-projects")))
                   "Z"))
    (should (equal (writing-habit-lists-normalize-title "6*Next semester projects (1)")
                   "next-next-next-next-next-next-semester-projects"))))

(provide 'writing-habit-lists-tests)
;;; writing-habit-lists-tests.el ends here
