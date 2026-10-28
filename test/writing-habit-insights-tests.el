;;; writing-habit-insights-tests.el --- ERT tests for rank, size, weeks, and comparison -*- lexical-binding: t; -*-

;;; Commentary:

;; The expected text comes from the Python port, run on the same records:
;; test/fixtures/insights.sql holds ten planned and tracked weeks, two
;; projects, three teaching weeks, and three weeks with coach calls.  No
;; test reaches a model server.

;;; Code:

(require 'ert)
(require 'writing-habit-insights)

(defconst writing-habit-insights-tests--sql
  (expand-file-name "fixtures/insights.sql"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The records shared with the Python check of these tests.")

(defmacro writing-habit-insights-tests--with-db (var &rest body)
  "Run BODY with VAR bound to an in-memory database holding the shared records."
  (declare (indent 1))
  `(let ((,var (writing-habit-db-connect)))
     (unwind-protect
         (progn
           (writing-habit-db-init ,var)
           (with-temp-buffer
             (insert-file-contents writing-habit-insights-tests--sql)
             (dolist (line (split-string (buffer-string) "\n" t))
               (sqlite-execute ,var line)))
           ,@body)
       (writing-habit-db-close ,var))))

(defmacro writing-habit-insights-tests--model (reply &rest body)
  "Run BODY with the model allowed and answering REPLY."
  (declare (indent 1))
  `(let ((writing-habit-coach-use-model t) (writing-habit-coach-ai-off nil))
     (cl-letf (((symbol-function 'writing-habit-coach-ask) (lambda (&rest _) ,reply)))
       ,@body)))

(defun writing-habit-insights-tests--hash (&rest pairs)
  "Return a hash table of the key and value PAIRS."
  (let ((h (make-hash-table :test #'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) h))
    h))

;;;; Patterns across weeks

(ert-deftest writing-habit-insights/weekday-rows ()
  (writing-habit-insights-tests--with-db db
    (should (equal (mapcar (lambda (r) (list (plist-get r :group) (plist-get r :adherence) (plist-get r :weeks)))
                           (writing-habit-insights-weekday-rows db))
                   '(("Monday" 0.95 10) ("Tuesday" 0.78 10) ("Wednesday" 0.2 10)
                     ("Thursday" 0.62 10) ("Friday" 0.45 10))))))

(ert-deftest writing-habit-insights/read-weeks-matches-the-python-port ()
  (writing-habit-insights-tests--with-db db
    (let* ((writing-habit-coach-use-model nil)
           (answer (writing-habit-insights-read-weeks-answer db "2026-10-05")))
      (should (equal (plist-get (plist-get answer :data) :patterns)
                     '("Your Monday blocks kept 95% of their planned minutes over 10 weeks, more than your overall 53%."
                       "Your Wednesday blocks kept 20% of their planned minutes over 10 weeks, less than your overall 53%.")))
      (should (equal (plist-get (plist-get answer :data) :change)
                     "Plan fewer minutes for your Wednesday blocks next week, and move the rest to times you keep more often."))
      (should (= (plist-get answer :overall) 0.53))
      (should (equal (plist-get answer :facts)
                     "Overall share of planned minutes kept: 53%\nPattern: weekday Monday, kept 95% of planned minutes over 10 weeks\nPattern: weekday Wednesday, kept 20% of planned minutes over 10 weeks")))))

(ert-deftest writing-habit-insights/read-weeks-keeps-true-sentences-only ()
  (writing-habit-insights-tests--with-db db
    (writing-habit-insights-tests--model
        (writing-habit-insights-tests--hash
         "patterns" '("You keep 95% of your Monday time across 10 weeks."
                      "Wednesday afternoons hold 37% of your plans.")
         "change" "Move 12 minutes to Monday.")
      (let ((data (plist-get (writing-habit-insights-read-weeks-answer db "2026-10-05") :data)))
        (should (equal (plist-get data :patterns)
                       '("You keep 95% of your Monday time across 10 weeks."
                         "Your Wednesday blocks kept 20% of their planned minutes over 10 weeks, less than your overall 53%.")))
        (should (string-prefix-p "Plan fewer minutes for your Wednesday" (plist-get data :change)))))))

(ert-deftest writing-habit-insights/read-weeks-with-no-data ()
  (let ((db (writing-habit-db-connect)))
    (unwind-protect
        (progn
          (writing-habit-db-init db)
          (should (equal (plist-get (writing-habit-insights-read-weeks-answer db) :text)
                         "There are no planned weeks to read yet.")))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-insights/early-weeks-read-as-the-python-port-reads-them ()
  ;; The month and schedule views cover every week, as in the Python port.
  (writing-habit-insights-tests--with-db db
    (let ((writing-habit-coach-use-model nil))
      (should (equal (plist-get (writing-habit-insights-read-weeks-answer db "2026-08-17") :text)
                     "Your weeks planned with the 5gA-gB schedule kept 55% of their planned minutes over 5 weeks, more than your overall 53%. 5 weeks of one writer is a small sample.\nYour weeks in 2026-08 kept 54% of their planned minutes over 5 weeks, more than your overall 53%. 5 weeks of one writer is a small sample.\nBuild more of next week's template like your weeks planned with the 5gA-gB schedule, because those are the blocks you keep.")))))

(ert-deftest writing-habit-insights/no-pattern-before-four-weeks ()
  (let ((db (writing-habit-db-connect)))
    (unwind-protect
        (progn
          (writing-habit-db-init db)
          (sqlite-execute db "INSERT INTO project(project_id, code) VALUES (1, 'A')")
          (sqlite-execute db "INSERT INTO plan_block(day, start_time, end_time, project_id, category_id) VALUES ('2026-08-03', '05:00', '06:30', 1, 1)")
          (should (equal (plist-get (writing-habit-insights-read-weeks-answer db) :text)
                         "No group has 4 weeks behind it yet, so there is no pattern to report. Keep planning and tracking, and look again.")))
      (writing-habit-db-close db))))

;;;; Sizing the week

(ert-deftest writing-habit-insights/size-matches-the-python-port ()
  (writing-habit-insights-tests--with-db db
    (let ((size (writing-habit-insights-size db "2026-10-12")))
      (should (equal (list (plist-get size :minutes) (plist-get size :basis) (plist-get size :weeks)
                           (plist-get size :median-planned) (plist-get size :kept))
                     '(285 "your last 8 planned weeks" 8 540 0.53)))
      (should (equal (writing-habit-insights-plain-size size)
                     "Plan about 4 h 45 min of writing next week. That is what you actually wrote in a typical week of your last 8 planned weeks, where you planned 9 h 00 min.")))
    (should (equal (writing-habit-insights-plain-size (writing-habit-insights-size db "2026-10-12" nil '("teaching")))
                   "Plan about 4 h 30 min of writing next week. That is what you actually wrote in a typical week of your 3 earlier teaching weeks, where you planned 9 h 00 min."))
    (should (equal (writing-habit-insights-plain-size (writing-habit-insights-size db "2026-10-12" "5gA"))
                   "Plan about 4 h 30 min of writing next week. That is what you actually wrote in a typical week of your 5 earlier weeks with the 5gA plan, where you planned 9 h 00 min."))
    (should-not (writing-habit-insights-size db "2026-08-03"))))

(ert-deftest writing-habit-insights/size-explained ()
  (writing-habit-insights-tests--with-db db
    (let* ((writing-habit-coach-use-model nil)
           (size (plist-put (writing-habit-insights-size db "2026-10-12") :current 600))
           (answer (writing-habit-insights-explain-size size)))
      (should (equal (plist-get answer :facts)
                     "Suggested minutes for next week: 285\nBased on: your last 8 planned weeks\nTypical minutes planned in those weeks: 540\nShare of planned minutes kept in those weeks: 53%\nMinutes in next week's plan as it stands: 600"))
      (should (equal (plist-get answer :data)
                     "Plan about 4 h 45 min of writing next week. That is what you actually wrote in a typical week of your last 8 planned weeks, where you planned 9 h 00 min. The week as planned holds 10 h 00 min, so trimming it toward the suggestion makes the plan easier to keep."))
      ;; A model's note that leaves out the number gets the plain note first.
      (writing-habit-insights-tests--model "Plan a little less than usual."
        (should (string-prefix-p "Plan about 4 h 45 min of writing next week."
                                 (plist-get (writing-habit-insights-explain-size size) :data))))
      (writing-habit-insights-tests--model "Aim for 285 minutes, which you can keep."
        (should (equal (plist-get (writing-habit-insights-explain-size size) :data)
                       "Aim for 285 minutes, which you can keep."))))))

;;;; Ranking the grasscatcher

(defconst writing-habit-insights-tests--tasks
  '(("A" . "Draft the aims") (nil . "Call the vendor") ("B" . "Reread comments") ("C" . "Fix figure 2")))
(defconst writing-habit-insights-tests--deadlines '(("A" . "2026-11-15") ("B") ("C" . "2026-10-30")))
(defconst writing-habit-insights-tests--planned '(("A" . 450) ("B" . 90)))

(ert-deftest writing-habit-insights/rank-matches-the-python-port ()
  (should (equal (writing-habit-insights-rank-facts writing-habit-insights-tests--tasks
                                                    writing-habit-insights-tests--deadlines
                                                    writing-habit-insights-tests--planned)
                 "There are 4 tasks, numbered 0 to 3. Rank all 4, using each number once.\nTasks:\n0. Draft the aims, project A, deadline 2026-11-15, 450 minutes planned this week\n1. Call the vendor\n2. Reread comments, project B, 90 minutes planned this week\n3. Fix figure 2, project C, deadline 2026-10-30, 0 minutes planned this week"))
  (should (equal (writing-habit-insights-plain-rank writing-habit-insights-tests--tasks
                                                    writing-habit-insights-tests--deadlines
                                                    writing-habit-insights-tests--planned)
                 '((:index 3 :text "Fix figure 2" :reason "deadline 2026-10-30")
                   (:index 0 :text "Draft the aims" :reason "deadline 2026-11-15")
                   (:index 2 :text "Reread comments" :reason "90 minutes planned this week")
                   (:index 1 :text "Call the vendor" :reason "no deadline or planned time")))))

(ert-deftest writing-habit-insights/rank-parse-keeps-every-task ()
  (let ((reply (writing-habit-insights-tests--hash
                "order" (list (writing-habit-insights-tests--hash "index" 2 "reason" "due soon")
                              (writing-habit-insights-tests--hash "index" 2 "reason" "again")
                              (writing-habit-insights-tests--hash "index" 9 "reason" "unknown")
                              (writing-habit-insights-tests--hash "index" 0 "reason" "")))))
    (should (equal (mapcar (lambda (i) (list (plist-get i :index) (plist-get i :reason)))
                           (writing-habit-insights-parse-rank reply writing-habit-insights-tests--tasks))
                   '((2 "due soon") (0 "ranked") (1 "left where it was") (3 "left where it was"))))
    (should-error (writing-habit-insights-parse-rank (writing-habit-insights-tests--hash) writing-habit-insights-tests--tasks))))

(ert-deftest writing-habit-insights/rank-schema-is-pinned ()
  (let ((json (json-encode (writing-habit-insights-rank-schema 3))))
    (should (string-search "\"minItems\":3" json))
    (should (string-search "\"enum\":[0,1,2]" json))))

(ert-deftest writing-habit-insights/reorder ()
  (should (equal (writing-habit-insights-reorder
                  '("#+NAME: grasscatcher" "| A: one |" "| B: two |" "| - three |" "")
                  "grasscatcher" '(2 0 1))
                 '("#+NAME: grasscatcher" "| - three |" "| A: one |" "| B: two |" ""))))

(ert-deftest writing-habit-insights/rank-command-reorders-on-yes ()
  (skip-unless (featurep 'writing-schedule))
  (require 'writing-habit-table-mode)
  (with-temp-buffer
    (insert "|        Time | M  | Tu |\n"
            "|-------------+----+----|\n"
            "| 05:00-06:30 | gA | gB |\n"
            "|-------------+----+----|\n"
            "| A: 1006AIrxOpt 2026-11-15 :safe: |  |  |\n"
            "| B: 0201dusp1 2026-10-30 :safe: |  |  |\n"
            "\n#+NAME: grasscatcher\n"
            "| A: Draft the aims |\n"
            "| B: Reread comments |\n")
    (cl-letf (((symbol-function 'writing-habit-table--refresh) #'ignore)
              ((symbol-function 'with-help-window) (lambda (&rest _) nil)))
      (let ((writing-habit-coach-use-model nil)
            (writing-habit-insights-confirm (lambda (_) t)))
        (let ((order (writing-habit-insights-rank)))
          (should (equal (mapcar (lambda (i) (plist-get i :reason)) order)
                         '("deadline 2026-10-30" "deadline 2026-11-15"))))
        (should (string-search "| B: Reread comments |\n| A: Draft the aims |" (buffer-string)))))))

;;;; Weeks with the coach and without it

(ert-deftest writing-habit-insights/comparison-matches-the-python-port ()
  (writing-habit-insights-tests--with-db db
    (let ((compared (writing-habit-insights-comparison db)))
      (should (equal compared '(:with-ai 3 :with-ai-kept 0.55 :without-ai 7 :without-ai-kept 0.53)))
      (should (equal (writing-habit-insights-comparison-text compared)
                     "Weeks with the coach: 3, 55% of the plan kept. Weeks without it: 7, 53% kept. A few weeks on each side describe your record. They do not prove that the coach helped.")))
    (should (equal (writing-habit-insights-comparison db "2026-08-24")
                   '(:with-ai 2 :with-ai-kept 0.53 :without-ai 2 :without-ai-kept 0.55)))
    (should-not (writing-habit-insights-comparison db "2026-08-17"))))

(provide 'writing-habit-insights-tests)
;;; writing-habit-insights-tests.el ends here
