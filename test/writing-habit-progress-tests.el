;;; writing-habit-progress-tests.el --- ERT tests for the progress table -*- lexical-binding: t; -*-

;;; Commentary:

;; The cases of the Python tests test_progress.py and test_gui_progress.py
;; that concern the table, the hours from mytime.db, the deadlines, and the
;; dashboard, with the same inputs and the same expected text.

;;; Code:

(require 'ert)
(require 'writing-habit-progress)

(defmacro writing-habit-progress-tests--on (day &rest body)
  "Run BODY with today fixed at DAY."
  (declare (indent 1))
  `(let ((writing-habit-progress-today-function (lambda () ,day)))
     ,@body))

(defun writing-habit-progress-tests--row (number name &rest keys)
  "Make a row NUMBER NAME with the fields in KEYS."
  (apply #'writing-habit-progress-row-create :number number :name name keys))

(defconst writing-habit-progress-tests--table
  '("#+NAME: current-semester-projects"
    "| A: 1094RNAstrucDetResearchProgram :safe: |"
    ""
    "#+NAME: progress"
    "| Project ID | Project name | Est. hours | Hours spent | Weight |"
    "|------------+--------------+------------+-------------+--------|"
    "| 0201       | dusp1        |         40 |          10 |      3 |"
    "| 1006       | AIrxOpt      |        100 |         2.5 |      5 |"
    "| 0655       | VirtIssue25  |            |             |        |"))

(ert-deftest writing-habit-progress/read ()
  (let ((rows (writing-habit-progress-read writing-habit-progress-tests--table)))
    (should (equal (mapcar #'writing-habit-progress-row-number rows) '("0201" "1006" "0655")))
    (should (= (writing-habit-progress-remaining (nth 1 rows)) 97.5))
    (should (null (writing-habit-progress-row-estimate (nth 2 rows))))
    (should (writing-habit-progress-row-planned (nth 0 rows)))
    (should (equal (writing-habit-progress-label (nth 2 rows)) "0655 VirtIssue25"))))

(ert-deftest writing-habit-progress/read-without-a-table ()
  (should (null (writing-habit-progress-read (seq-take writing-habit-progress-tests--table 2)))))

(ert-deftest writing-habit-progress/columns-by-header ()
  (writing-habit-progress-tests--on "2026-10-27"
    (let ((row (car (writing-habit-progress-read
                     '("#+NAME: progress"
                       "| Project ID | Deadline | Weight | Project name | Est. hours | Hours spent |"
                       "| 1006 | 2026-10-20 | 0.5 | AIrxOpt | 100 | 3 |")))))
      (should (equal (list (writing-habit-progress-row-name row) (writing-habit-progress-row-estimate row)
                           (writing-habit-progress-row-spent row) (writing-habit-progress-row-weight row)
                           (writing-habit-progress-row-deadline row))
                     '("AIrxOpt" 100 3 0.5 "2026-10-20")))
      (should (= (writing-habit-progress-days-left row) -7)))))

(ert-deftest writing-habit-progress/bad-dates ()
  (should (equal (mapcar #'writing-habit-progress-row-deadline
                         (writing-habit-progress-read
                          '("#+NAME: progress" "| Project ID | Project name | Deadline |"
                            "| 1006 | AIrxOpt | soon |" "| 1007 | x | 2026-02-30 |")))
                 '(nil nil))))

(ert-deftest writing-habit-progress/write-matches-the-python-port ()
  (writing-habit-progress-tests--on "2026-10-27"
    (let* ((lines '("#+NAME: progress"
                    "| Project ID | Project name | Est. hours | Hours spent | Weight | Next week |"
                    "|---|"
                    "| 0201 | dusp1 | 40 | 10 | 0.9 | no |"))
           (rows (writing-habit-progress-read lines)))
      (setf (writing-habit-progress-row-deadline (car rows)) "2026-11-06")
      (let ((out (writing-habit-progress-write lines rows)))
        (should (equal out
                       '("#+NAME: progress"
                         "| Project ID | Project name | Est. hours | Hours spent | Weight | Deadline   | Days to deadline | Next week |"
                         "|------------+--------------+------------+-------------+--------+------------+------------------+-----------|"
                         "| 0201       | dusp1        |         40 |          10 |    0.9 | 2026-11-06 |               10 | no        |")))
        (should (equal (writing-habit-progress-write out (writing-habit-progress-read out)) out))))))

(ert-deftest writing-habit-progress/write-adds-the-table-at-the-end ()
  (writing-habit-progress-tests--on "2026-10-27"
    (let ((out (writing-habit-progress-write
                '("| A: x |" "" "")
                (list (writing-habit-progress-tests--row "1094" "RNAstruc" :estimate 50)))))
      (should (equal (seq-take out 4) '("| A: x |" "" "#+NAME: progress"
                                        "| Project ID | Project name | Est. hours | Hours spent | Weight | Deadline | Days to deadline | Next week |")))
      (should (equal (car (last out)) ""))
      (should (= (writing-habit-progress-row-estimate (car (writing-habit-progress-read out))) 50)))))

(ert-deftest writing-habit-progress/days-text ()
  (should (equal (writing-habit-progress-days-text nil) ""))
  (should (equal (writing-habit-progress-days-text 0) "due today"))
  (should (equal (writing-habit-progress-days-text 12) "12 d"))
  (should (equal (writing-habit-progress-days-text -3) "3 d late")))

(ert-deftest writing-habit-progress/seed-and-fill ()
  (let* ((entries (list (writing-habit-lists-entry-create :code "A" :description "1094RNAstruc")
                        (writing-habit-lists-entry-create :code nil :description "email")
                        (writing-habit-lists-entry-create :code "B" :description "0201dusp1 2026-10-15")))
         (rows (writing-habit-progress-seed
                entries (list (writing-habit-progress-tests--row "0201" "dusp1" :estimate 40)))))
    (should (equal (mapcar #'writing-habit-progress-label rows) '("0201 dusp1" "1094 RNAstruc")))
    (should (= (writing-habit-progress-row-estimate (car rows)) 40))
    (should (= (writing-habit-progress-fill-deadlines rows entries) 1))
    (should (equal (writing-habit-progress-row-deadline (car rows)) "2026-10-15")))
  (let ((rows (list (writing-habit-progress-tests--row "1006" "AIrxOpt")
                    (writing-habit-progress-tests--row "0201" "dusp1" :deadline "2026-12-01"))))
    (writing-habit-progress-fill-deadlines
     rows (list (writing-habit-lists-entry-create :code "A" :description "1006AIrxOpt 2026-12-15")
                (writing-habit-lists-entry-create :code "B" :description "0201dusp1 2026-10-15")))
    (should (equal (mapcar #'writing-habit-progress-row-deadline rows) '("2026-12-15" "2026-12-01")))))

(defun writing-habit-progress-tests--mytime ()
  "Write a small mytime.db and return its path."
  (let* ((path (make-temp-file "wh-mytime" nil ".db"))
         (db (sqlite-open path)))
    (sqlite-execute db "CREATE TABLE zTimeSpent (id INTEGER PRIMARY KEY, DateDashed DATE, Start TIME, \"End\" TIME, ProjectID INT, ProjectDirectory TEXT, Description TEXT, Activity TEXT)")
    (dolist (r '(("2026-10-05" "05:30" "07:00" 1006 "1006AIrxOpt")
                 ("2026-10-06" "5:00" "6:00" nil "1006AIrxOpt")
                 ("2026-10-06" "23:30" "00:15" 201 "0201dusp1")
                 ("2026-10-07" "" "" 201 "0201dusp1")
                 ("2026-10-07" "08:00" "09:00" 9999 "9999other")))
      (sqlite-execute db "INSERT INTO zTimeSpent VALUES (NULL, ?, ?, ?, ?, ?, '', '')" r))
    (sqlite-close db)
    path))

(ert-deftest writing-habit-progress/hours-spent ()
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((path (writing-habit-progress-tests--mytime)))
    (unwind-protect
        (should (equal (writing-habit-progress-hours-spent path '("1006" "0201" "0655"))
                       '(("1006" . 2.5) ("0201" . 0.8) ("0655" . 0.0))))
      (delete-file path))))

(ert-deftest writing-habit-progress/bar ()
  (let ((half (writing-habit-progress-tests--row "0001" "a" :estimate 10 :spent 5))
        (over (writing-habit-progress-tests--row "0002" "b" :estimate 10 :spent 12))
        (open (writing-habit-progress-tests--row "0003" "c" :spent 4)))
    (should (equal (substring-no-properties (writing-habit-progress-bar half 20 20)) "█████░░░░░"))
    (should (equal (substring-no-properties (writing-habit-progress-bar over 20 20)) "██████████|▓▓"))
    (should (equal (substring-no-properties (writing-habit-progress-bar open 20 20)) "████ no estimate"))))

(ert-deftest writing-habit-progress/dashboard ()
  (writing-habit-progress-tests--on "2026-10-14"
    (let* ((rows (list (writing-habit-progress-tests--row "1006" "AIrxOpt" :estimate 100 :spent 3
                                                          :weight 0.9 :deadline "2026-12-15")
                       (writing-habit-progress-tests--row "0201" "dusp1" :estimate 40 :spent 10
                                                          :weight 0.4 :deadline "2026-10-10")
                       (writing-habit-progress-tests--row "0655" "VirtIssue25" :weight 0.95 :planned nil)))
           (text (writing-habit-progress-dashboard-string rows 100))
           (lines (split-string text "\n")))
      (should (string-match-p "\\`0201 dusp1 +█+░+ 4 d late\\'" (nth 3 lines)))
      (should (string-match-p "\\`0655 VirtIssue25 +no estimate\\'" (nth 4 lines)))
      (should (string-match-p "\\`1006 AIrxOpt +█*░+ 62 d\\'" (nth 5 lines)))
      ;; Only the checked project of high weight is highlighted.
      (should (eq (get-text-property 0 'face (nth 5 lines)) 'writing-habit-progress-high))
      (should-not (get-text-property 0 'face (nth 4 lines)))
      (should (eq (get-text-property (string-search "4 d late" (nth 3 lines)) 'face (nth 3 lines))
                  'writing-habit-progress-late))
      (should (string-search "0201   dusp1" text))
      (should (string-search "2026-10-10    -4  yes" text)))))

(ert-deftest writing-habit-progress/dashboard-without-a-table ()
  (should (string-search "No progress table in this week file yet"
                         (writing-habit-progress-dashboard-string nil 80))))

(ert-deftest writing-habit-progress/commands-in-a-week-buffer ()
  (skip-unless (and (featurep 'writing-schedule) (sqlite-available-p)))
  (require 'writing-habit-table-mode)
  (let ((path (writing-habit-progress-tests--mytime)))
    (unwind-protect
        (writing-habit-progress-tests--on "2026-10-14"
          (with-temp-buffer
            (insert "|        Time | M  | Tu |\n"
                    "|-------------+----+----|\n"
                    "| 05:00-06:30 | gA | gB |\n"
                    "|-------------+----+----|\n"
                    "| A: 1006AIrxOpt 2026-12-15 :safe: |  |  |\n"
                    "| B: 0201dusp1 :safe: |  |  |\n"
                    "\n"
                    "#+NAME: current-semester-projects\n"
                    "| A: 1006AIrxOpt 2026-12-15 :safe: |\n"
                    "| B: 0201dusp1 :safe: |\n"
                    "| - email |\n")
            (writing-habit-progress-seed-command)
            (let ((rows (writing-habit-progress-rows)))
              (should (equal (mapcar #'writing-habit-progress-label rows) '("0201 dusp1" "1006 AIrxOpt")))
              (should (equal (writing-habit-progress-row-deadline (nth 1 rows)) "2026-12-15")))
            (should (string-search "| 1006       | AIrxOpt      |            |           0 |        | 2026-12-15 |               62 | yes       |"
                                   (buffer-string)))
            (should (= (writing-habit-progress-update path) 2))
            (should (equal (mapcar #'writing-habit-progress-row-spent (writing-habit-progress-rows))
                           '(0.8 2.5)))
            (should (string-search "#+NAME: current-semester-projects" (buffer-string)))))
      (delete-file path))))


(ert-deftest writing-habit-progress/weight-keeps-two-decimals ()
  (should (equal (writing-habit-progress--fmt 0.95) "0.95"))
  (should (equal (writing-habit-progress--fmt 0.9) "0.9"))
  (should (equal (writing-habit-progress--fmt 2.5) "2.5"))
  (should (equal (writing-habit-progress--fmt 40) "40")))

;;;; Next week's plan, with the expected text of the Python port

(defun writing-habit-progress-tests--three ()
  "The three rows of the Python GUI tests."
  (list (writing-habit-progress-tests--row "0201" "dusp1" :estimate 40 :spent 10 :weight 1)
        (writing-habit-progress-tests--row "1006" "AIrxOpt" :estimate 100 :spent 3 :weight 3)
        (writing-habit-progress-tests--row "0655" "VirtIssue25")))

(ert-deftest writing-habit-progress/share-follows-weights-and-time-left ()
  (let ((rows (list (writing-habit-progress-tests--row "0001" "heavy" :estimate 100 :weight 3)
                    (writing-habit-progress-tests--row "0002" "light" :estimate 100 :weight 1)
                    (writing-habit-progress-tests--row "0003" "nearly done" :estimate 10 :spent 9.5 :weight 5)
                    (writing-habit-progress-tests--row "0004" "no weight" :estimate 10)
                    (writing-habit-progress-tests--row "0005" "no estimate" :weight 4))))
    (should (equal (mapcar (lambda (s) (cons (writing-habit-progress-row-number (car s)) (cdr s)))
                           (writing-habit-progress-share rows 600))
                   '(("0003" . 30) ("0001" . 420) ("0002" . 150))))))

(ert-deftest writing-habit-progress/plan-matches-the-python-port ()
  (let ((plan (writing-habit-progress-plan (writing-habit-progress-tests--three) 450
                                           (writing-habit-checks-default-profile))))
    (should (equal (writing-habit-progress-plan-facts plan)
                   "Minutes to plan next week: 450 (7 h 30 min)\nProject 1006 AIrxOpt: weight 3, 97 hours left of 100, planned 330 minutes (5 h 30 min)\nProject 0201 dusp1: weight 1, 30 hours left of 40, planned 120 minutes (2 h 00 min)\nBlock: Monday 05:00-06:30  1006 AIrxOpt, in high hours for new writing\nBlock: Monday 06:30-07:00  0201 dusp1, in high hours for new writing\nBlock: Tuesday 05:00-06:30  1006 AIrxOpt, in high hours for new writing\nBlock: Wednesday 05:00-06:30  1006 AIrxOpt, in high hours for new writing\nBlock: Thursday 05:00-06:00  1006 AIrxOpt, in high hours for new writing\nBlock: Friday 05:00-06:30  0201 dusp1, in high hours for new writing\nLeft out: 0655 VirtIssue25, because it has no estimate"))
    (should (equal (writing-habit-progress-plan-plain plan)
                   "Next week's 7 h 30 min go by weight to 1006 AIrxOpt (5 h 30 min) and 0201 dusp1 (2 h 00 min). The heaviest projects take the hours your energy profile rates best for new writing, in blocks of at most 90 minutes with a break after every two hours. 1 project without an estimate or a weight was left out."))
    (should (equal (mapcar (lambda (r) (writing-habit-progress-item-line plan r))
                           (writing-habit-progress-plan-eligible plan))
                   '("1006 AIrxOpt: 5 h 30 min (Monday 05:00-06:30; Tuesday 05:00-06:30; Wednesday 05:00-06:30; Thursday 05:00-06:00)"
                     "0201 dusp1: 2 h 00 min (Monday 06:30-07:00; Friday 05:00-06:30)")))))

(ert-deftest writing-habit-progress/plan-of-the-practice-week ()
  (let* ((rows (list (writing-habit-progress-tests--row "1001" "grantR01" :estimate 120 :spent 6.7 :weight 0.9)
                     (writing-habit-progress-tests--row "2002" "reviewArticle" :estimate 40 :spent 4.3 :weight 0.6)
                     (writing-habit-progress-tests--row "3003" "methodsNote" :estimate 30 :spent 1.5 :weight 0.3)
                     (writing-habit-progress-tests--row "4004" "dataPaper" :estimate 60 :spent 2.7 :weight 0.95)
                     (writing-habit-progress-tests--row "5005" "bookChapter" :estimate 200 :spent 0 :weight 0.4)
                     (writing-habit-progress-tests--row "9001" "email" :spent 2.8)))
         (plan (writing-habit-progress-plan rows 1575 (writing-habit-checks-default-profile))))
    (should (equal (writing-habit-progress-plan-plain plan)
                   "Next week's 26 h 15 min go by weight to 4004 dataPaper (8 h 00 min), 1001 grantR01 (7 h 30 min), 2002 reviewArticle (5 h 00 min), 5005 bookChapter (3 h 00 min), and 3003 methodsNote (2 h 30 min). The heaviest projects take the hours your energy profile rates best for new writing, in blocks of at most 90 minutes with a break after every two hours. 1 project without an estimate or a weight was left out."))
    (should (equal (mapcar (lambda (r) (writing-habit-progress-item-line plan r))
                           (writing-habit-progress-plan-eligible plan))
                   '("4004 dataPaper: 8 h 00 min (Monday 05:00-06:30; Monday 06:30-07:00; Tuesday 05:00-06:30; Wednesday 05:00-06:30; Thursday 05:00-06:30; Friday 05:00-06:30)"
                     "1001 grantR01: 7 h 30 min (Monday 07:15-08:45; Tuesday 06:45-08:15; Wednesday 06:45-08:15; Thursday 06:45-08:15; Friday 06:45-08:15)"
                     "2002 reviewArticle: 5 h 00 min (Tuesday 08:30-10:00; Wednesday 08:30-10:00; Thursday 08:30-10:00; Friday 08:15-08:45)"
                     "5005 bookChapter: 3 h 00 min (Monday 09:00-10:30; Friday 09:00-10:30)"
                     "3003 methodsNote: 2 h 30 min (Tuesday 10:15-11:45; Wednesday 10:15-11:15)")))
    (should (string-search "Project 4004 dataPaper: weight 0.95, 57.3 hours left of 60, planned 480 minutes (8 h 00 min)"
                           (writing-habit-progress-plan-facts plan)))
    (should (string-search "Block: Tuesday 10:15-11:45  3003 methodsNote, in medium hours for new writing"
                           (writing-habit-progress-plan-facts plan)))))

(ert-deftest writing-habit-progress/plain-notes-with-nothing-to-share ()
  (let ((profile (writing-habit-checks-default-profile)))
    (should (equal (writing-habit-progress-plan-plain
                    (writing-habit-progress-plan
                     (list (writing-habit-progress-tests--row "0001" "a" :estimate 100 :weight 0.9 :planned nil))
                     300 profile))
                   "No project is checked for next week, so there is nothing to share out. Check the projects to work on and press Update the plan."))
    (should (equal (writing-habit-progress-plan-plain
                    (writing-habit-progress-plan (list (writing-habit-progress-tests--row "0655" "x")) 600 profile))
                   "No project in the progress table has both a weight and time left, so there is nothing to share out. Fill in the Est. hours and Weight columns on the Progress tab."))
    (should (string-prefix-p
             "All of next week's 3 h 00 min go to 0201 dusp1."
             (writing-habit-progress-plan-plain
              (writing-habit-progress-plan (list (writing-habit-progress-tests--row "0201" "dusp1" :estimate 40 :spent 10 :weight 3)
                                                 (writing-habit-progress-tests--row "0655" "VirtIssue25"))
                                           180 profile))))))

(ert-deftest writing-habit-progress/unchecked-rows-get-no-time ()
  (let* ((rows (list (writing-habit-progress-tests--row "0001" "a" :estimate 100 :weight 0.9 :planned nil)
                     (writing-habit-progress-tests--row "0002" "b" :estimate 100 :weight 0.5)))
         (plan (writing-habit-progress-plan rows 300 (writing-habit-checks-default-profile))))
    (should (equal (mapcar (lambda (s) (writing-habit-progress-row-number (car s)))
                           (writing-habit-progress-plan-shares plan))
                   '("0002")))
    (should (equal (mapcar #'writing-habit-progress-row-number (writing-habit-progress-plan-eligible plan))
                   '("0001" "0002")))
    (should (equal (writing-habit-progress-item-line plan (car rows)) "0001 a: not in this plan"))))

(ert-deftest writing-habit-progress/add-to-next-week ()
  (let* ((lines '("#+NAME: next week" "| - 0655VirtIssue25 |" ""
                  "#+NAME: current-semester-projects" "| A: 1006AIrxOpt 2026-12-15 :safe: |" "| B: 0201dusp1 :safe: |"))
         (result (writing-habit-progress-add-to-next-week
                  lines (list (writing-habit-progress-tests--row "1006" "AIrxOpt")
                              (writing-habit-progress-tests--row "0655" "VirtIssue25")
                              (writing-habit-progress-tests--row "7777" "newOne")))))
    (should (= (cdr result) 2))
    (should (equal (mapcar #'writing-habit-lists-entry-project
                           (writing-habit-lists-entries (car result) "next week"))
                   '("0655VirtIssue25" "1006AIrxOpt" "7777newOne")))
    (should (equal (writing-habit-lists-entry-deadline
                    (nth 1 (writing-habit-lists-entries (car result) "next week")))
                   "2026-12-15"))))

(ert-deftest writing-habit-progress/add-to-next-week-makes-the-list ()
  (let* ((lines '("#+PROJECT_PERIOD: Fall 2026" "#+NAME: current-semester-projects" "| A: 1006AIrxOpt :safe: |"))
         (result (writing-habit-progress-add-to-next-week
                  lines (list (writing-habit-progress-tests--row "1006" "AIrxOpt")))))
    (should (equal (car result)
                   '("#+NAME: next week" "| - 1006AIrxOpt :safe: |" "" "#+PROJECT_PERIOD: Fall 2026"
                     "#+NAME: current-semester-projects" "| A: 1006AIrxOpt :safe: |")))))

(defconst writing-habit-progress-tests--week
  (concat "#+TITLE: progress\n"
          "|        Time | M  | Tu | W  | Th | F  |\n"
          "|-------------+----+----+----+----+----|\n"
          "| 05:00-06:30 | gA | gB | gA | gB | gA |\n"
          "|-------------+----+----+----+----+----|\n"
          "| A: 1006AIrxOpt 2026-12-15 :safe: |  |  |  |  |  |\n"
          "| B: 0201dusp1 :safe: |  |  |  |  |  |\n"
          "\n"
          "#+NAME: current-semester-projects\n"
          "| A: 1006AIrxOpt 2026-12-15 :safe: |\n"
          "| B: 0201dusp1 :safe: |\n"
          "| - email |\n"
          "\n"
          "#+NAME: next week\n"
          "| - 0655VirtIssue25 |\n")
  "The week of the Python GUI tests of the plan.")

(defmacro writing-habit-progress-tests--with-plan (dusp1-planned &rest body)
  "Run BODY in the test week with its progress table, after making the plan.
DUSP1-PLANNED sets the Next week box of 0201."
  (declare (indent 1))
  `(with-temp-buffer
     (require 'writing-habit-table-mode)
     (insert writing-habit-progress-tests--week)
     (cl-letf (((symbol-function 'writing-habit-table--refresh) #'ignore)
               ((symbol-function 'pop-to-buffer) #'ignore)
               ((symbol-function 'writing-habit-checks-profile) #'writing-habit-checks-default-profile))
       (let ((writing-habit-coach-use-model nil)
             (week (current-buffer)))
         (writing-habit-progress--store
          (list (writing-habit-progress-tests--row "0201" "dusp1" :estimate 40 :spent 10 :weight 0.4
                                                   :planned ,dusp1-planned)
                (writing-habit-progress-tests--row "1006" "AIrxOpt" :estimate 100 :spent 3 :weight 0.9)
                (writing-habit-progress-tests--row "0655" "VirtIssue25")))
         (let ((plan-buffer (writing-habit-progress-plan-next-week)))
           (unwind-protect
               (with-current-buffer plan-buffer ,@body)
             (kill-buffer plan-buffer)))))))

(ert-deftest writing-habit-progress/plan-buffer-shares-the-week ()
  (skip-unless (featurep 'writing-schedule))
  (writing-habit-progress-tests--with-plan t
    (should (= (writing-habit-progress-plan-capacity writing-habit-progress--plan) 450))
    (should (equal (mapcar (lambda (s) (cons (writing-habit-progress-row-number (car s)) (cdr s)))
                           (writing-habit-progress-plan-shares writing-habit-progress--plan))
                   '(("1006" . 300) ("0201" . 150))))
    ;; The same lines as the Python port gives for these rows.
    (should (string-search "[X] 1006 AIrxOpt: 5 h 00 min (Monday 05:00-06:30; Tuesday 05:00-06:30; Wednesday 05:00-06:30; Thursday 05:00-05:30)" (buffer-string)))
    (should (string-search "[X] 0201 dusp1: 2 h 30 min (Monday 06:45-07:45; Friday 05:00-06:30)" (buffer-string)))
    (should (string-prefix-p "Next week's 7 h 30 min go by weight" writing-habit-progress--plan-note))
    ;; Only the checked project of high weight is highlighted.
    (goto-char (point-min))
    (search-forward "[X] 1006")
    (should (eq (get-text-property (point) 'face) 'writing-habit-progress-high))
    (search-forward "[X] 0201")
    (should-not (get-text-property (point) 'face))))

(ert-deftest writing-habit-progress/uncheck-update-and-accept ()
  (skip-unless (featurep 'writing-schedule))
  (writing-habit-progress-tests--with-plan t
    (goto-char (point-min))
    (search-forward "] 0201 dusp1")
    (writing-habit-progress-plan-toggle)
    (writing-habit-progress-plan-update)
    (should (string-search "[X] 1006 AIrxOpt: 7 h 30 min (" (buffer-string)))
    (should (string-search "[ ] 0201 dusp1: not in this plan" (buffer-string)))
    (should (string-prefix-p "All of next week's 7 h 30 min go to 1006 AIrxOpt."
                             writing-habit-progress--plan-note))
    (let ((week writing-habit-progress--plan-week))
      (should (= (writing-habit-progress-plan-accept) 1))
      (with-current-buffer week
        (should (equal (mapcar #'writing-habit-lists-entry-project
                               (writing-habit-lists-entries (writing-habit-progress--buffer-lines) "next week"))
                       '("0655VirtIssue25" "1006AIrxOpt")))
        (should (equal (mapcar (lambda (r) (cons (writing-habit-progress-row-number r)
                                                 (writing-habit-progress-row-planned r)))
                               (writing-habit-progress-rows))
                       '(("0201") ("1006" . t) ("0655" . t))))
        (should-not (string-search "dusp1" (current-kill 0)))
        (should (string-search "AIrxOpt" (current-kill 0)))))))

(ert-deftest writing-habit-progress/accept-after-unchecking-updates-first ()
  (skip-unless (featurep 'writing-schedule))
  (writing-habit-progress-tests--with-plan t
    (goto-char (point-min))
    (search-forward "] 1006 AIrxOpt")
    (writing-habit-progress-plan-toggle)
    (let ((week writing-habit-progress--plan-week))
      (writing-habit-progress-plan-accept)
      (with-current-buffer week
        (should (equal (mapcar #'writing-habit-lists-entry-project
                               (writing-habit-lists-entries (writing-habit-progress--buffer-lines) "next week"))
                       '("0655VirtIssue25" "0201dusp1")))))))

(ert-deftest writing-habit-progress/the-choice-carries-over ()
  (skip-unless (featurep 'writing-schedule))
  (writing-habit-progress-tests--with-plan nil
    (should (equal (mapcar #'writing-habit-progress-row-number writing-habit-progress--plan-rows)
                   '("1006" "0201")))
    (should (string-search "[X] 1006 AIrxOpt" (buffer-string)))
    (should (string-search "[ ] 0201 dusp1: not in this plan" (buffer-string)))
    (should (equal (mapcar (lambda (s) (writing-habit-progress-row-number (car s)))
                           (writing-habit-progress-plan-shares writing-habit-progress--plan))
                   '("1006")))))

(ert-deftest writing-habit-progress/discard-keeps-the-old-choices ()
  (skip-unless (featurep 'writing-schedule))
  (writing-habit-progress-tests--with-plan t
    (goto-char (point-min))
    (search-forward "] 0201 dusp1")
    (writing-habit-progress-plan-toggle)
    (writing-habit-progress-plan-update)
    (with-current-buffer writing-habit-progress--plan-week
      (should (seq-every-p #'writing-habit-progress-row-planned (writing-habit-progress-rows))))))

(provide 'writing-habit-progress-tests)
;;; writing-habit-progress-tests.el ends here
