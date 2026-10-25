;;; writing-habit-checks-tests.el --- ERT tests for the planning checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ports test_deadlines.py, test_energy.py, test_rescue.py,
;; test_semester.py, and the partner summary tests of test_coach.py, with
;; the same inputs and the same expected text.  The model is never asked,
;; because `writing-habit-coach-use-model' stays nil.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst writing-habit-checks-tests--have-schedule
  (and (require 'writing-schedule nil t) (fboundp 'writing-schedule-split-row))
  "Non-nil when writing-schedule.el is available.")

(when writing-habit-checks-tests--have-schedule
  (require 'writing-habit-checks)
  (require 'writing-habit-table-mode))

(defmacro writing-habit-checks-tests--deftest (name &rest body)
  "Define test NAME running BODY when writing-schedule.el is available."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless writing-habit-checks-tests--have-schedule)
     ,@body))

(defun writing-habit-checks-tests--slot (offset start end code category minutes row column)
  "Make a slot from OFFSET START END CODE CATEGORY MINUTES ROW COLUMN."
  (writing-habit-checks-slot-create :offset offset :start start :end end :code code
                                    :category category :minutes minutes :row row :column column))

;;;; Deadline coverage

(defconst writing-habit-checks-tests--legend
  '(("A" . "1006AIrxOpt 2026-10-30") ("B" . "1002RNAediting 2026-10-23")
    ("C" . "2003reviewArticle") ("D" . "0055talk 2027-03-01") ("E" . "email")))

(defconst writing-habit-checks-tests--lists
  '(("current-semester-projects" nil "1452PHFSboXt 2026-11-02")
    ("current-semester-projects" "B" "1002RNAediting 2026-10-23")
    ("next-semester-projects" nil "9999later 2027-02-01")))

(defun writing-habit-checks-tests--coverage-slots ()
  "Return the week of test_deadlines.py."
  (list (writing-habit-checks-tests--slot 0 "05:00" "07:00" "A" "generative" 120 1 1)
        (writing-habit-checks-tests--slot 2 "05:00" "06:30" "B" "generative" 90 1 3)
        (writing-habit-checks-tests--slot 3 "05:00" "06:30" "C" "generative" 90 1 4)
        (writing-habit-checks-tests--slot 4 "05:00" "06:30" "C" "generative" 90 1 5)
        (writing-habit-checks-tests--slot 3 "09:00" "10:00" "D" "editing" 60 2 4)
        (writing-habit-checks-tests--slot 4 "13:00" "13:45" "E" "support" 45 3 5)))

(writing-habit-checks-tests--deftest writing-habit-checks/projects-due
  (let ((due (writing-habit-checks-projects-due writing-habit-checks-tests--legend
                                                writing-habit-checks-tests--lists "2026-10-14" 28)))
    (should (equal (mapcar (lambda (p) (cons (writing-habit-checks-project-name p)
                                             (writing-habit-checks-project-in-legend p)))
                           due)
                   '(("1002RNAediting" . t) ("1006AIrxOpt" . t) ("1452PHFSboXt")))))
  (should-not (writing-habit-checks-projects-due '(("Z" . "old 2026-10-01")) nil "2026-10-14" 28)))

(writing-habit-checks-tests--deftest writing-habit-checks/coverage-donors
  (let* ((result (writing-habit-checks-coverage-check
                  writing-habit-checks-tests--legend writing-habit-checks-tests--lists
                  (writing-habit-checks-tests--coverage-slots) "2026-10-12" "2026-10-14" 28))
         (risks (writing-habit-checks-coverage-risks result))
         (a (seq-find (lambda (r) (equal (writing-habit-checks-project-name (writing-habit-checks-risk-project r))
                                         "1006AIrxOpt"))
                      risks))
         (other (seq-find (lambda (r) (equal (writing-habit-checks-project-name (writing-habit-checks-risk-project r))
                                             "1452PHFSboXt"))
                          risks)))
    (should (= (length risks) 2))
    (should (equal (list (writing-habit-checks-risk-planned a) (writing-habit-checks-risk-ahead a)
                         (writing-habit-checks-risk-days-left a))
                   '(120 0 16)))
    (should (equal (list (writing-habit-checks-slot-code (writing-habit-checks-risk-donor a))
                         (writing-habit-checks-slot-offset (writing-habit-checks-risk-donor a))
                         (writing-habit-checks-risk-donor-keeps a))
                   '("C" 3 90)))
    (should (equal (writing-habit-checks-slot-offset (writing-habit-checks-risk-donor other)) 4))
    (should (equal (mapcar (lambda (c) (writing-habit-checks-project-name (car c)))
                           (writing-habit-checks-coverage-covered result))
                   '("1002RNAediting")))))

(writing-habit-checks-tests--deftest writing-habit-checks/coverage-keeps-due-and-last-blocks
  (let ((one (list (writing-habit-checks-tests--slot 3 "05:00" "06:30" "B" "generative" 90 1 4))))
    (should-not (writing-habit-checks-risk-donor
                 (seq-find (lambda (r) (equal (writing-habit-checks-project-code (writing-habit-checks-risk-project r)) "A"))
                           (writing-habit-checks-coverage-risks
                            (writing-habit-checks-coverage-check writing-habit-checks-tests--legend nil one
                                                                 "2026-10-12" "2026-10-14" 28))))))
  (let* ((legend '(("A" . "1006AIrxOpt 2026-10-30") ("D" . "0055talk 2027-03-01")))
         (one (list (writing-habit-checks-tests--slot 3 "09:00" "10:00" "D" "editing" 60 2 4)))
         (two (append one (list (writing-habit-checks-tests--slot 4 "09:00" "10:00" "D" "editing" 60 2 5)))))
    (should-not (writing-habit-checks-risk-donor
                 (car (writing-habit-checks-coverage-risks
                       (writing-habit-checks-coverage-check legend nil one "2026-10-12" "2026-10-14" 28)))))
    (should (equal (writing-habit-checks-slot-code
                    (writing-habit-checks-risk-donor
                     (car (writing-habit-checks-coverage-risks
                           (writing-habit-checks-coverage-check legend nil two "2026-10-12" "2026-10-14" 28)))))
                   "D"))))

(writing-habit-checks-tests--deftest writing-habit-checks/coverage-plain-and-facts
  (let* ((result (writing-habit-checks-coverage-check
                  writing-habit-checks-tests--legend writing-habit-checks-tests--lists
                  (writing-habit-checks-tests--coverage-slots) "2026-10-12" "2026-10-14" 28))
         (text (writing-habit-checks-coverage-plain result))
         (facts (writing-habit-checks-coverage-facts result)))
    (should (string-search "A 1006AIrxOpt is due 2026-10-30, in 16 days, and has no block left this week. Give it Thursday 05:00-06:30, now planned for C 2003reviewArticle, which has no deadline and keeps 1 h 30 min this week." text))
    (should (string-search "It waits on the current-semester-projects list." text))
    (should (string-search "1 other project due soon has time left this week." text))
    (should (string-search "Covered: B 1002RNAediting, due 2026-10-23, 90 minutes" facts))
    (should (string-prefix-p "Give Thursday 05:00-06:30 (generative) to A 1006AIrxOpt"
                             (writing-habit-checks-coverage-line
                              (car (writing-habit-checks-coverage-risks result)))))
    (should (string-prefix-p "No project in the legend or the lists"
                             (writing-habit-checks-coverage-plain
                              (writing-habit-checks-coverage-check '(("C" . "2003review"))
                                                                   nil (writing-habit-checks-tests--coverage-slots)
                                                                   "2026-10-12" "2026-10-14" 28))))))

;;;; Energy

(defun writing-habit-checks-tests--energy-week ()
  "Return the week of test_energy.py."
  (list (writing-habit-checks-tests--slot 0 "05:00" "06:30" "E" "support" 90 1 1)
        (writing-habit-checks-tests--slot 0 "14:00" "15:30" "A" "generative" 90 2 1)
        (writing-habit-checks-tests--slot 0 "09:00" "10:00" "B" "editing" 60 3 1)
        (writing-habit-checks-tests--slot 1 "05:00" "07:00" "A" "generative" 120 1 2)))

(writing-habit-checks-tests--deftest writing-habit-checks/profile-json-and-fit
  (let ((profile (writing-habit-checks-default-profile)))
    (should (= (aref (cdr (assoc "generative" profile)) 6) 2))
    (should (= (aref (cdr (assoc "support" profile)) 14) 2))
    (should (equal (writing-habit-checks-profile-from-json "not json") profile))
    (should (equal (writing-habit-checks-profile-from-json "{\"generative\": [9, 9]}") profile))
    (aset (cdr (assoc "editing" profile)) 3 2)
    (should (= (aref (cdr (assoc "editing" (writing-habit-checks-profile-from-json
                                            (writing-habit-checks-profile-json profile))))
                     3)
               2))
    (should (= (writing-habit-checks-fit profile "generative" "05:00" "07:00") 2.0))
    (should (= (writing-habit-checks-fit profile "generative" "09:30" "10:30") 1.5))
    (should (= (writing-habit-checks-fit profile "generative" "23:00" "01:00") 0.0))
    (should (equal (writing-habit-checks-word 1.5) "mostly high"))))

(writing-habit-checks-tests--deftest writing-habit-checks/misfits-and-swaps
  (let* ((profile (writing-habit-checks-default-profile))
         (found (writing-habit-checks-misfits profile (writing-habit-checks-tests--energy-week)))
         (proposed (writing-habit-checks-swaps profile (writing-habit-checks-tests--energy-week))))
    (should (equal (mapcar (lambda (m) (cons (writing-habit-checks-slot-code (car m)) (nth 1 m))) found)
                   '(("E" . "peak") ("A" . "low"))))
    (should (string-search "support E uses your high hours for generative work"
                           (writing-habit-checks-misfit-line profile (car found))))
    (should (= (length proposed) 1))
    (should (equal (writing-habit-checks-swap-line (car proposed))
                   "Monday: swap support E at 05:00-06:30 with generative A at 14:00-15:30"))
    (should-not (writing-habit-checks-swaps profile (writing-habit-checks-tests--energy-week)
                                            (lambda (s) (> (writing-habit-checks-slot-offset s) 0))))
    (let ((week (writing-habit-checks-tests--energy-week)))
      (setf (nth 1 week) (writing-habit-checks-tests--slot 0 "14:00" "15:00" "A" "generative" 60 2 1))
      (should (equal (mapcar (lambda (w) (sort (list (writing-habit-checks-slot-code (nth 0 w))
                                                     (writing-habit-checks-slot-code (nth 1 w)))
                                               #'string<))
                             (writing-habit-checks-swaps profile week))
                     '(("A" "B")))))
    (let ((text (writing-habit-checks-energy-plain profile found proposed))
          (facts (writing-habit-checks-energy-facts profile found proposed)))
      (should (string-prefix-p "2 blocks do not fit your energy profile." text))
      (should (string-search "keeps every project's minutes" text))
      (should (string-search "generative A sits in high hours for generative work" facts))
      (should-not (string-search "1.5" facts)))))

(writing-habit-checks-tests--deftest writing-habit-checks/profile-from-reviews
  (let ((db (writing-habit-db-connect nil)))
    (unwind-protect
        (progn
          (writing-habit-db-init db)
          (let ((generative (caar (sqlite-select db "SELECT category_id FROM category WHERE name = 'generative'")))
                (pid (writing-habit-db-get-or-create-project db "A" "project A" "safe")))
            (dolist (spec '(("2026-10-05" "14:00" "15:00" 5) ("2026-10-06" "14:00" "15:00" 5)
                            ("2026-10-07" "14:00" "15:00" 5) ("2026-10-08" "20:00" "21:00" 1)))
              (sqlite-execute db "INSERT INTO plan_block(day, start_time, end_time, project_id, category_id)
 VALUES (?, ?, ?, ?, ?)" (list (nth 0 spec) (nth 1 spec) (nth 2 spec) pid generative))
              (sqlite-execute db "INSERT INTO session_review(day, start_time, code, focus) VALUES (?, ?, 'A', ?)"
                              (list (nth 0 spec) (nth 1 spec) (nth 3 spec)))))
          (let* ((profile (writing-habit-checks-default-profile))
                 (result (writing-habit-checks-profile-from-reviews db profile)))
            (should (equal (mapcar (lambda (c) (list (nth 0 c) (nth 1 c) (nth 2 c) (nth 3 c) (nth 5 c)))
                                   (cdr result))
                           '(("generative" 14 0 2 3))))
            (should (= (aref (cdr (assoc "generative" (car result))) 20) 1))
            (should (= (aref (cdr (assoc "generative" profile)) 14) 0))))
      (writing-habit-db-close db))))

;;;; Missed-block rescue

(defun writing-habit-checks-tests--rescue-week ()
  "Return the week of test_rescue.py."
  (list (writing-habit-checks-tests--slot 0 "05:00" "06:30" "A" "generative" 90 1 1)
        (writing-habit-checks-tests--slot 0 "13:00" "13:45" "E" "support" 45 2 1)
        (writing-habit-checks-tests--slot 1 "05:00" "07:00" "B" "generative" 120 3 2)
        (writing-habit-checks-tests--slot 1 "14:00" "15:30" "B" "generative" 90 4 2)
        (writing-habit-checks-tests--slot 2 "05:00" "07:00" "B" "generative" 120 3 3)))

(defconst writing-habit-checks-tests--outcomes
  '((:day "2026-10-12" :start "05:00" :code "A" :status "missed")
    (:day "2026-10-12" :start "13:00" :code "E" :status "missed")
    (:day "2026-10-13" :start "05:00" :code "B" :status "kept")
    (:day "2026-10-13" :start "14:00" :code "B" :status "missed")))

(defconst writing-habit-checks-tests--rescue-legend
  '(("A" . "1001grantR01 2026-10-30") ("B" . "2002review 2027-03-01") ("E" . "email")))

(writing-habit-checks-tests--deftest writing-habit-checks/rescue
  (let* ((week (writing-habit-checks-tests--rescue-week))
         (now '("2026-10-13" . 720))
         (missed (writing-habit-checks-missed-blocks week writing-habit-checks-tests--outcomes "2026-10-12" now))
         (found (writing-habit-checks-rescue-plan week missed (writing-habit-checks-default-profile)
                                                  '(0 1 2 3 4) "2026-10-12" now
                                                  writing-habit-checks-tests--rescue-legend))
         (a (seq-find (lambda (r) (equal (writing-habit-checks-slot-code (writing-habit-checks-rescue-missed r)) "A"))
                      found))
         (e (seq-find (lambda (r) (equal (writing-habit-checks-slot-code (writing-habit-checks-rescue-missed r)) "E"))
                      found))
         (text (writing-habit-checks-rescue-plain found)))
    (should (equal (mapcar (lambda (s) (list (writing-habit-checks-slot-offset s) (writing-habit-checks-slot-start s)))
                           missed)
                   '((0 "05:00") (0 "13:00"))))
    (should (equal (list (writing-habit-checks-rescue-offset a) (writing-habit-checks-rescue-start a)
                         (writing-habit-checks-rescue-end a))
                   '(2 "07:15" "08:45")))
    (should (equal (writing-habit-checks-rescue-line a)
                   "Make up generative A from Monday 05:00-06:30 on Wednesday 07:15-08:45"))
    (should-not (writing-habit-checks-rescue-offset e))
    (should (string-prefix-p "2 blocks passed with nothing tracked. A missed block is information, not a failure." text))
    (should (string-search "Generative A from Monday 05:00-06:30 can be made up on Wednesday 07:15-08:45." text))
    (should (string-search "Let support E from Monday 13:00-13:45 go" text))
    (should (string-search "in high hours for generative work" (writing-habit-checks-rescue-facts found)))))

(writing-habit-checks-tests--deftest writing-habit-checks/rescue-slots
  (let ((late (list (cons "generative" (writing-habit-checks--levels '(19 21 2))) (cons "editing" (make-vector 24 0))
                    (cons "support" (make-vector 24 0))))
        (high (list (cons "generative" (make-vector 24 2)) (cons "editing" (make-vector 24 2))
                    (cons "support" (make-vector 24 2))))
        (week (writing-habit-checks-tests--rescue-week)))
    (should (equal (seq-take (writing-habit-checks-find-slot (car week) week late '(0 1 2 3 4)
                                                             "2026-10-12" '("2026-10-12" . 547))
                             3)
                   '(0 "19:00" "20:30")))
    (should (equal (seq-take (writing-habit-checks-find-slot (car week) week high '(0)
                                                             "2026-10-12" '("2026-10-12" . 547))
                             3)
                   '(0 "09:15" "10:45")))
    (let* ((slots (mapcar (lambda (h) (writing-habit-checks-tests--slot 0 (format "%02d:00" h) (format "%02d:30" h)
                                                                        "A" "generative" 30 h 1))
                          '(5 7 9)))
           (outcomes (mapcar (lambda (s) (list :day "2026-10-12" :start (writing-habit-checks-slot-start s)
                                               :code "A" :status "missed"))
                             slots))
           (missed (writing-habit-checks-missed-blocks slots outcomes "2026-10-12" '("2026-10-13" . 720)))
           (found (writing-habit-checks-rescue-plan slots missed high '(0 1 2 3 4) "2026-10-12"
                                                    '("2026-10-13" . 720) writing-habit-checks-tests--rescue-legend)))
      (should (= (seq-count #'writing-habit-checks-rescue-offset found) 2)))
    (should (equal (writing-habit-checks-rescue-plain nil)
                   "No block of this week has been missed so far. Nothing needs making up."))))

;;;; The partner summary and the semester review

(defun writing-habit-checks-tests--db ()
  "Return an in-memory database with the semester of test_semester.py."
  (let* ((db (writing-habit-db-connect nil)))
    (writing-habit-db-init db)
    (let ((generative (caar (sqlite-select db "SELECT category_id FROM category WHERE name = 'generative'")))
          (a (writing-habit-db-get-or-create-project db "A" "1001grantR01 2026-10-30" "safe"))
          (b (writing-habit-db-get-or-create-project db "B" "0142RETKinase" "safe")))
      (dolist (day '("2026-09-07" "2026-09-14" "2026-10-05"))
        (sqlite-execute db "INSERT INTO plan_block(day, start_time, end_time, project_id, category_id)
 VALUES (?, '05:00', '10:00', ?, ?)" (list day a generative)))
      (dolist (s (list (list "2026-09-07" 240 a) (list "2026-09-14" 120 b)
                       (list "2026-10-05" 60 a) (list "2026-06-01" 500 b)))
        (sqlite-execute db "INSERT INTO session(day, actual_min, project_id, source) VALUES (?, ?, ?, 'manual')" s))
      (dolist (o '(("2026-09-07" "Draft aim 1" 1) ("2026-09-07" "Find three citations" 0)
                   ("2026-09-14" "Revise figure 2." 1)))
        (sqlite-execute db "INSERT INTO block_objective(day, start_time, code, text, done)
 VALUES (?, '05:00', 'A', ?, ?)" o)))
    db))

(writing-habit-checks-tests--deftest writing-habit-checks/semester
  (let ((db (writing-habit-checks-tests--db)))
    (unwind-protect
        (let* ((s (writing-habit-checks-semester-gather db "Fall 2026" "2026-08-17" "2026-12-18"
                                                        '("0131RETKinase2018BrP 2026-09-30")))
               (text (writing-habit-checks-semester-plain s)))
          (should (equal (list (plist-get s :planned) (plist-get s :actual) (plist-get s :weeks)
                               (plist-get s :weeks-written))
                         '(900 420 3 3)))
          (should (= (writing-habit-checks-semester-pct s) 47))
          (should (equal (plist-get s :months) '(("2026-09" 600 360) ("2026-10" 300 60))))
          (should (equal (plist-get s :projects) '(("1001grantR01" . 300) ("0142RETKinase" . 120))))
          (should (equal (plist-get s :done) '("Draft aim 1" "Revise figure 2.")))
          (should (string-prefix-p "During Fall 2026, I planned 15 h 00 min of writing over 3 weeks and wrote 7 h 00 min, which is 47% of the plan." text))
          (should (string-search "highest in September, at 60%, and lowest in October, at 20%." text))
          (should (string-search "Most of the time went to 1001grantR01 (5 h 00 min) and 0142RETKinase (2 h 00 min)." text))
          (should (string-search "I completed 0131RETKinase2018BrP." text))
          (should (string-search "I met 2 of the 3 block objectives I committed to." text))
          (should (string-search "Objectives I met include \"Draft aim 1\" and \"Revise figure 2\"." text))
          (should-not (writing-habit-checks-semester-gather db "Spring 2026" "2026-01-05" "2026-05-08"))
          (let ((writing-habit-coach-use-model nil))
            (should (equal (plist-get (writing-habit-coach-note "semester" (writing-habit-checks-semester-facts s) text)
                                      :text)
                           text))))
      (writing-habit-db-close db))))

(writing-habit-checks-tests--deftest writing-habit-checks/partner
  (let ((db (writing-habit-checks-tests--db)))
    (unwind-protect
        (let* ((week (writing-habit-checks-partner-week db "2026-09-07" "2026-09-20"))
               (text (writing-habit-checks-partner-plain week)))
          (should (equal (plist-get week :kept-pct) 80))
          (should (string-prefix-p "Here is my writing week of September 7. I planned 5 h 00 min of writing and wrote 4 h 00 min, which is 80% of the plan. I kept 1 of the 1 planned blocks that have passed." text))
          (should (string-search "I met 1 of the 2 objectives I committed to. The objective I met was \"Draft aim 1\"." text))
          (should (string-search "Next week I have planned 5 h 00 min." text))
          (should (string-search "Minutes planned: 300 (5 h 00 min)" (writing-habit-checks-partner-facts week)))
          (should-not (writing-habit-checks-partner-week db "2026-01-05")))
      (writing-habit-db-close db))))

;;;; Commands on a table

(defconst writing-habit-checks-tests--table
  (concat "#+TITLE: plan checks\n"
          "|        Time | M  | Tu | W  | Th | F  |\n"
          "|-------------+----+----+----+----+----|\n"
          "| 05:00-06:30 | gA | sE | gC | sE | gC |\n"
          "| 14:00-15:30 |    | gB |    | gC |    |\n"
          "|-------------+----+----+----+----+----|\n"
          "| A: 1006AIrxOpt 2026-10-30 :safe: |  |  |  |  |  |\n"
          "| C: 2003reviewArticle :safe: |  |  |  |  |  |\n"
          "| E: email |  |  |  |  |  |\n")
  "A small week, the one of test_gui_plan_checks.py without B.")

(writing-habit-checks-tests--deftest writing-habit-checks/apply-swaps-and-place
  (let* ((table (writing-habit-table-from-text writing-habit-checks-tests--table))
         (slots (writing-habit-checks-slots table))
         (mon (seq-find (lambda (s) (= (writing-habit-checks-slot-offset s) 0)) slots))
         (tue (seq-find (lambda (s) (= (writing-habit-checks-slot-offset s) 1)) slots)))
    (cl-letf (((symbol-function 'writing-habit-table--apply) #'ignore))
      (writing-habit-checks-apply-swaps table (list (list mon tue 0 1)))
      (should (equal (writing-habit-table-cell table (writing-habit-checks-slot-row mon)
                                               (writing-habit-checks-slot-column mon))
                     "sE"))
      (let ((row (writing-habit-checks-place-block table 3 "07:15" "08:45" "gA")))
        (should (equal (writing-habit-table-row-parsed (writing-habit-table-row-at table row))
                       '("07:15" . "08:45")))
        (should (equal (mapcar (lambda (i) (car (writing-habit-table-row-parsed (writing-habit-table-row-at table i))))
                               (writing-habit-table-block-rows table))
                       '("05:00" "07:15" "14:00")))
        (should (equal (writing-habit-table-cell table row 4) "gA"))))))

;;;; Why a block was missed, and the Monday message

(defun writing-habit-checks-tests--miss-db ()
  "Return a database with the missed blocks of test_misses.py."
  (let ((db (writing-habit-db-connect nil)))
    (writing-habit-db-init db)
    (let ((ids (sqlite-select db "SELECT name, category_id FROM category"))
          (a (writing-habit-db-get-or-create-project db "A" "1001grantR01 2026-10-30" "safe"))
          (e (writing-habit-db-get-or-create-project db "E" "email" nil)))
      (dolist (spec (list (list "2026-10-06" a "generative" "05:00") (list "2026-10-13" a "generative" "05:00")
                          (list "2026-10-12" e "support" "13:00") (list "2026-10-14" a "generative" "07:00")))
        (sqlite-execute db "INSERT INTO plan_block(day, start_time, end_time, project_id, category_id)
 VALUES (?, ?, strftime('%H:%M', ?, '+90 minutes'), ?, ?)"
                        (list (nth 0 spec) (nth 3 spec) (nth 3 spec) (nth 1 spec)
                              (cadr (assoc (nth 2 spec) ids)))))
      (sqlite-execute db "INSERT INTO session(day, start_time, end_time, actual_min, project_id, source)
 VALUES ('2026-10-14', '07:00', '08:30', 90, ?, 'manual')" (list a)))
    db))

(writing-habit-checks-tests--deftest writing-habit-checks/misses
  (let ((db (writing-habit-checks-tests--miss-db)))
    (unwind-protect
        (let ((found (writing-habit-checks-misses-find db "2026-10-12" "2026-10-16")))
          (should (equal (mapcar (lambda (m) (list (plist-get m :day) (plist-get m :code) (plist-get m :streak)))
                                 found)
                         '(("2026-10-12" "E" 1) ("2026-10-13" "A" 2))))
          (should (equal (writing-habit-checks-miss-question (nth 1 found))
                         "The Tuesday 05:00-06:30 block of A 1001grantR01 was missed for the second week in a row. What took that slot?"))
          (should (equal (writing-habit-checks-miss-question (nth 0 found))
                         "The Monday 13:00-14:30 block of E email was missed. What took that slot?"))
          (should (equal (nth 1 (split-string (writing-habit-checks-misses-facts found) "\n"))
                         "Missed block 2: Tuesday 05:00-06:30, generative, A 1001grantR01, missed in this slot 2 weeks in a row"))
          (writing-habit-checks-miss-save db (nth 1 found) "q" "Committee meeting moved")
          (writing-habit-checks-miss-save db (nth 1 found) "q" "  The committee met at 5  ")
          (should (equal (plist-get (nth 1 (writing-habit-checks-misses-find db "2026-10-12" "2026-10-16")) :reason)
                         "The committee met at 5"))
          (writing-habit-checks-miss-save db (nth 1 found) "q" "")
          (should (= (caar (sqlite-select db "SELECT COUNT(*) FROM block_miss")) 0))
          (should (equal (writing-habit-checks-misses-parse "1. What took Monday?\n- What took Tuesday?" found)
                         '("What took Monday?" "What took Tuesday?")))
          (should (stringp (writing-habit-checks-misses-parse "What happened?" found)))
          (let ((writing-habit-coach-use-model nil))
            (should (equal (plist-get (writing-habit-checks-misses-questions found) :questions)
                           (mapcar #'writing-habit-checks-miss-question found))))
          (let ((writing-habit-coach-use-model t) (writing-habit-coach-ai-off nil))
            (cl-letf (((symbol-function 'writing-habit-coach-ask)
                       (lambda (&rest _) "What took it on day 9?\nWhat took Tuesday?")))
              (should (string-search "number" (plist-get (writing-habit-checks-misses-questions found) :source))))))
      (writing-habit-db-close db))))

(writing-habit-checks-tests--deftest writing-habit-checks/plan-message
  (let* ((legend '(("A" . "1006AIrxOpt 2026-10-30") ("B" . "1002RNAediting 2027-03-01")
                   ("C" . "2003reviewArticle") ("E" . "email")))
         (minutes '(("A" . 90) ("B" . 180) ("C" . 270) ("E" . 135)))
         (plan (writing-habit-checks-plan-gather "2026-10-12" minutes legend 8 5
                                                 '(("C" . "Outline the review") ("A" . "Draft aim 1.")))))
    (should (equal (nth 0 (plist-get plan :key)) "A"))
    (should (equal (writing-habit-checks-plan-plain plan)
                   (concat "This week I plan 11 h 15 min of writing in 8 blocks over 5 days. "
                           "Most of it goes to 2003reviewArticle (4 h 30 min), 1002RNAediting "
                           "(3 h 00 min), and email (2 h 15 min).\n\n"
                           "The one that matters most is 1006AIrxOpt, due October 30. My first "
                           "objective there is \"Draft aim 1\". I will send you how it went on Friday.")))
    (should (string-search "Project that matters most: A 1006AIrxOpt, due 2026-10-30 (October 30)"
                           (writing-habit-checks-plan-facts plan)))
    (should (string-prefix-p
             "This week I plan 2 h 00 min of writing in 1 block over 1 day. All of it goes to 2003reviewArticle (2 h 00 min).\n\nThe one that matters most is 2003reviewArticle, which has the most time."
             (writing-habit-checks-plan-plain
              (writing-habit-checks-plan-gather "2026-10-12" '(("C" . 120)) '(("C" . "2003reviewArticle")) 1 1))))
    (should-not (writing-habit-checks-plan-gather "2026-10-12" nil nil 0 0))
    (should (equal (nth 0 (plist-get (writing-habit-checks-plan-gather
                                      "2026-10-12" '(("A" . 60) ("C" . 90))
                                      '(("A" . "old 2026-09-01") ("C" . "2003review")) 2 2)
                                     :key))
                   "C"))))

(provide 'writing-habit-checks-tests)
;;; writing-habit-checks-tests.el ends here
