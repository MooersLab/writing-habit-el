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

(provide 'writing-habit-progress-tests)
;;; writing-habit-progress-tests.el ends here
