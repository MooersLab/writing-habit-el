;;; writing-habit-horizons-tests.el --- ERT tests for the lists of the weeks and semesters ahead -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Finding, counting, and adding the week and semester lists, and the two
;; menus that mirror the button rows of the Python Schedule tab.

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))

(require 'writing-habit-horizons)

(defconst writing-habit-horizons-tests--text
  (concat "| Time        | M |\n"
          "|-------------+---|\n"
          "| 09:00-10:00 | A |\n"
          "\n"
          "#+NAME: current-semester-projects\n"
          "| A: 1006AIrxOpt 2026-12-15 :safe: |\n"
          "| - 1002RNAeditingR01 2027-02-05 :safe: |\n"
          "\n"
          "#+NAME: Next week\n"
          "#+CAPTION: Next week\n"
          "| Code | Project Number | Project Name | Deadline | Risk tag | Default activity |\n"
          "|------+----------------+--------------+----------+----------+------------------|\n"
          "| A:   | 1006           | AIrxOpt      |          | :safe:   |                  |\n"
          "\n"
          "#+NAME: two weeks out\n"
          "\n"
          "#+NAME: grasscatcher\n"
          "| A: find three citations |\n")
  "A week file with lists in both layouts.")

(defmacro writing-habit-horizons-tests--with (&rest body)
  "Run BODY in a buffer holding the test week file."
  (declare (indent 0))
  `(with-temp-buffer
     (insert writing-habit-horizons-tests--text)
     (goto-char (point-min))
     ,@body))

(ert-deftest writing-habit-horizons/rows-match-the-python-lists ()
  "The two rows hold the lists of the Python editor, in its order."
  (should (equal (mapcar #'car writing-habit-horizons-weeks)
                 '("next week" "two weeks out" "three weeks out" "four weeks out"
                   "five weeks out" "six weeks out" "seven weeks out" "eight weeks out"
                   "grasscatcher")))
  (should (equal (length writing-habit-horizons-semesters) 9))
  (dolist (pair writing-habit-lists)
    (should (assoc (car pair) writing-habit-horizons-semesters))))

(ert-deftest writing-habit-horizons/counts-each-layout ()
  "One-cell rows are counted, and a header above a rule is not."
  (writing-habit-horizons-tests--with
    (should (= (writing-habit-horizons-count "current-semester-projects") 2))
    (should (= (writing-habit-horizons-count "next week") 1))
    (should (= (writing-habit-horizons-count "two weeks out") 0))
    (should (= (writing-habit-horizons-count "grasscatcher") 1))
    (should-not (writing-habit-horizons-count "eight weeks out"))))

(ert-deftest writing-habit-horizons/finds-a-list-named-by-its-title ()
  "A #+NAME: line that holds the title finds the list."
  (writing-habit-horizons-tests--with
    (should (writing-habit-horizons-find "next week"))
    (should (writing-habit-horizons-find "current-semester-projects"))
    (should-not (writing-habit-horizons-find "next-semester-projects"))))

(ert-deftest writing-habit-horizons/label-says-how-many ()
  "The menu label names the list and its rows."
  (writing-habit-horizons-tests--with
    (should (equal (writing-habit-horizons-label "current-semester-projects")
                   "Current semester projects (2)"))
    (should (equal (writing-habit-horizons-label "six weeks out")
                   "Six weeks out (not in this file)"))))

(ert-deftest writing-habit-horizons/goto-lands-on-the-first-row ()
  "Going to a list puts point inside its first project row."
  (writing-habit-horizons-tests--with
    (writing-habit-horizons-goto-week-1)
    (should (looking-at-p "A:   | 1006"))
    (writing-habit-horizons-goto-current)
    (should (looking-at-p "A: 1006AIrxOpt"))
    (writing-habit-horizons-goto-grasscatcher)
    (should (looking-at-p "A: find three"))))

(ert-deftest writing-habit-horizons/goto-adds-a-missing-list ()
  "A missing list is added at the end of the file when the writer agrees."
  (writing-habit-horizons-tests--with
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
      (writing-habit-horizons-goto-semester-1))
    (should (= (writing-habit-horizons-count "next-semester-projects") 0))
    (should (string-suffix-p "\n\n#+NAME: next-semester-projects\n#+CAPTION: Next semester projects\n"
                             (buffer-string)))
    (should (eobp))))

(ert-deftest writing-habit-horizons/goto-refused-adds-nothing ()
  "Declining to add a missing list leaves the buffer alone."
  (writing-habit-horizons-tests--with
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (should-error (writing-habit-horizons-goto-week-8) :type 'user-error))
    (should (equal (buffer-string) writing-habit-horizons-tests--text))))

(ert-deftest writing-habit-horizons/menus-offer-every-list ()
  "The two menus offer a key for each list of their row."
  (let ((weeks (format "%S" (get 'writing-habit-horizons-weeks-menu 'transient--layout)))
        (semesters (format "%S" (get 'writing-habit-horizons-semesters-menu 'transient--layout))))
    (dolist (n '(1 2 3 4 5 6 7 8))
      (should (string-match-p (format "writing-habit-horizons-goto-week-%d" n) weeks)))
    (should (string-match-p "writing-habit-horizons-goto-grasscatcher" weeks))
    (dolist (n '(1 2 3 4 5 6))
      (should (string-match-p (format "writing-habit-horizons-goto-semester-%d" n) semesters)))
    (dolist (c '("current" "carried" "completed"))
      (should (string-match-p (format "writing-habit-horizons-goto-%s" c) semesters)))))

(provide 'writing-habit-horizons-tests)
;;; writing-habit-horizons-tests.el ends here
