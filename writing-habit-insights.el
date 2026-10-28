;;; writing-habit-insights.el --- Rank, size, read the weeks, and compare -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit-el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Four coach tasks of the Python port, with the same rules and the same
;; wording:
;;
;;   `writing-habit-insights-rank'        order the grasscatcher tasks (Rule 4)
;;   `writing-habit-insights-size'        size this week's plan (Rule 7)
;;   `writing-habit-insights-read-weeks'  the two strongest patterns and one
;;                                        change to the template (Rule 7)
;;   `writing-habit-insights-comparison'  the share of the plan kept in weeks
;;                                        with the coach and without it (Rule 10)
;;
;; The software computes every number.  A local model, when
;; `writing-habit-coach-use-model' allows it, only words the reply, and a
;; reply that holds a number the facts do not hold is replaced by the plain
;; one.  This is a port of the matching functions of coach.py.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-coach)
(require 'writing-habit-checks)
(require 'writing-habit-lists)
(require 'writing-habit-records)
(require 'writing-habit-table)

(declare-function writing-habit-table-current "writing-habit-table-mode")
(declare-function writing-habit-table--apply "writing-habit-table-mode" (model))

(defconst writing-habit-insights-min-weeks 4
  "A group needs this many weeks before its adherence counts as a pattern.")

(defconst writing-habit-insights-size-window 8
  "The number of past planned weeks the plan size is drawn from.")

(defvar writing-habit-insights-confirm #'y-or-n-p
  "Function that asks a yes-or-no question with a prompt.  Tests replace it.")

;;;; Numbers the way Python rounds them

(defun writing-habit-insights--round (value &optional digits)
  "Round VALUE to DIGITS places, ties to even, as Python's round does."
  (if digits
      (let ((scale (expt 10.0 digits)))
        (/ (fround (* value scale)) scale))
    (truncate (fround value))))

(defun writing-habit-insights--pct (value)
  "Return VALUE, a share, as a whole percentage."
  (writing-habit-insights--round (* value 100)))

(defun writing-habit-insights--median (values)
  "Return the median of the numbers VALUES."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (n (length sorted)))
    (if (cl-oddp n)
        (nth (/ n 2) sorted)
      (/ (+ (nth (1- (/ n 2)) sorted) (nth (/ n 2) sorted)) 2.0))))

(defun writing-habit-insights--hours (minutes)
  "Return MINUTES as 7 h 30 min, or 45 min."
  (let ((h (/ minutes 60)) (m (% minutes 60)))
    (if (> h 0) (format "%d h %02d min" h m) (format "%d min" m))))

(defun writing-habit-insights--numbers (text)
  "Return the whole numbers in TEXT."
  (writing-habit-coach-numbers text))

(defun writing-habit-insights--subset-p (text allowed)
  "Return non-nil when every number in TEXT is among ALLOWED."
  (null (seq-difference (writing-habit-insights--numbers text) allowed)))

;;;; Asking the model

(defun writing-habit-insights--ask (task facts plain parse &optional db week schema)
  "Ask TASK about FACTS and PARSE the reply, or fall back to PLAIN.
PLAIN is a function of no arguments.  PARSE turns the reply into the
answer or signals an error, and SCHEMA, when given, replaces the schema of
the prompts file.  Return a plist with :data, :source, :facts, and :log-id."
  (let ((source "plain summary written without a model")
        data)
    (when (writing-habit-coach-allowed-p)
      (condition-case err
          (setq data (funcall parse (writing-habit-coach-ask task facts schema))
                source (format "%s at %s" writing-habit-coach-model writing-habit-coach-url))
        (error (setq source (concat source ", because " (error-message-string err))))))
    (unless data (setq data (funcall plain)))
    (list :data data :source source :facts facts
          :log-id (when (and db (writing-habit-coach-allowed-p))
                    (writing-habit-coach-log db task source facts
                                             (if (stringp data) data (prin1-to-string data))
                                             week)))))

(defun writing-habit-insights--settle (db answer accepted)
  "Mark ANSWER accepted or discarded in DB when it was logged."
  (when (plist-get answer :log-id)
    (writing-habit-coach-mark-accepted db (plist-get answer :log-id) accepted)))

(defun writing-habit-insights--show (title text)
  "Show TEXT under TITLE in a help buffer."
  (with-help-window (format "*writing-habit %s*" (downcase title))
    (princ (concat title "\n\n" text "\n"))))

;;;; Rank the grasscatcher (Rule 4)

(defun writing-habit-insights-rank-facts (tasks deadlines planned)
  "Return the facts for ranking TASKS, a list of (CODE . TEXT).
DEADLINES and PLANNED are alists from a code to its deadline and to its
minutes planned this week."
  (let ((lines '()) (i 0))
    (dolist (task tasks)
      (let ((code (car task))
            (parts (list (format "%d. %s" i (writing-habit-coach-clean (cdr task))))))
        (when code
          (push (format "project %s" code) parts)
          (when (cdr (assoc code deadlines))
            (push (format "deadline %s" (cdr (assoc code deadlines))) parts))
          (push (format "%d minutes planned this week" (or (cdr (assoc code planned)) 0)) parts))
        (push (string-join (nreverse parts) ", ") lines))
      (setq i (1+ i)))
    (format "There are %d tasks, numbered 0 to %d. Rank all %d, using each number once.\nTasks:\n%s"
            (length tasks) (1- (length tasks)) (length tasks)
            (string-join (nreverse lines) "\n"))))

(defun writing-habit-insights-rank-schema (count)
  "Return the ranking schema pinned to COUNT tasks."
  `((type . "object")
    (properties . ((order . ((type . "array") (minItems . ,count) (maxItems . ,count)
                             (items . ((type . "object")
                                       (properties . ((index . ((type . "integer")
                                                                (enum . ,(vconcat (number-sequence 0 (1- count))))))
                                                      (reason . ((type . "string")))))
                                       (required . ["index" "reason"])))))))
    (required . ["order"])))

(defun writing-habit-insights-plain-rank (tasks deadlines planned)
  "Order TASKS by the nearest of DEADLINES, then by minutes PLANNED this week.
Return a list of plists (:index :text :reason)."
  (let* ((indexes (number-sequence 0 (1- (length tasks))))
         (key (lambda (i)
                (let ((code (car (nth i tasks))))
                  (list (or (cdr (assoc code deadlines)) "9999-99-99")
                        (- (or (cdr (assoc code planned)) 0))
                        i))))
         (less (lambda (a b)
                 (let ((ka (funcall key a)) (kb (funcall key b)))
                   (or (string< (nth 0 ka) (nth 0 kb))
                       (and (string= (nth 0 ka) (nth 0 kb))
                            (or (< (nth 1 ka) (nth 1 kb))
                                (and (= (nth 1 ka) (nth 1 kb)) (< (nth 2 ka) (nth 2 kb))))))))))
    (mapcar (lambda (i)
              (let* ((code (car (nth i tasks)))
                     (due (and code (cdr (assoc code deadlines))))
                     (minutes (and code (cdr (assoc code planned)))))
                (list :index i :text (cdr (nth i tasks))
                      :reason (cond (due (format "deadline %s" due))
                                    ((and minutes (> minutes 0))
                                     (format "%d minutes planned this week" minutes))
                                    (t "no deadline or planned time")))))
            (sort indexes less))))

(defun writing-habit-insights-parse-rank (reply tasks)
  "Return the order in REPLY, a parsed hash table, for TASKS.
A task the model dropped is put at the end, left where it was."
  (let ((order (and (hash-table-p reply) (gethash "order" reply)))
        (seen '()) (out '()))
    (unless order (error "The reply held no order"))
    (dolist (item order)
      (let ((index (and (hash-table-p item) (gethash "index" item))))
        (when (and (integerp index) (<= 0 index) (< index (length tasks))
                   (not (memq index seen)))
          (push index seen)
          (push (list :index index :text (cdr (nth index tasks))
                      :reason (let ((r (writing-habit-coach-clean (gethash "reason" item))))
                                (if (string-empty-p r) "ranked" r)))
                out))))
    (dotimes (i (length tasks))
      (unless (memq i seen)
        (push (list :index i :text (cdr (nth i tasks)) :reason "left where it was") out)))
    (nreverse out)))

(defun writing-habit-insights-rank-lines (order)
  "Return ORDER as lines such as 1. Draft the aims (deadline 2026-11-15)."
  (let ((n 0))
    (mapcar (lambda (item)
              (setq n (1+ n))
              (format "%d. %s (%s)" n (plist-get item :text) (plist-get item :reason)))
            order)))

(defun writing-habit-insights-reorder (lines name order)
  "Return LINES with the rows of list NAME put in ORDER, a list of positions."
  (let* ((rows (writing-habit-lists-row-indexes lines name))
         (texts (mapcar (lambda (i) (nth i lines)) rows))
         (vec (vconcat lines)))
    (cl-loop for row in rows for position in order
             do (aset vec row (nth position texts)))
    (append vec nil)))

;;;###autoload
(defun writing-habit-insights-rank ()
  "Suggest an order for the grasscatcher tasks, and use it when accepted."
  (interactive)
  (let* ((table (writing-habit-table-current))
         (after (writing-habit-table-after table))
         (entries (and (writing-habit-lists--span after "grasscatcher")
                       (writing-habit-lists-entries after "grasscatcher"))))
    (if (< (length entries) 2)
        (progn (message "The grasscatcher needs two tasks to rank") nil)
      (let* ((tasks (mapcar (lambda (e) (cons (writing-habit-lists-entry-code e)
                                              (writing-habit-lists-entry-description e)))
                            entries))
             (deadlines (delq nil (mapcar (lambda (task)
                                            (when (car task)
                                              (cons (car task)
                                                    (plist-get (writing-habit-table-project-info table (car task))
                                                               :due))))
                                          tasks)))
             (planned (let ((out '()))
                        (dolist (b (writing-habit-table-blocks table))
                          (let ((cell (assoc (writing-habit-table-block-letter b) out)))
                            (if cell
                                (setcdr cell (+ (cdr cell) (writing-habit-table-block-minutes b)))
                              (push (cons (writing-habit-table-block-letter b)
                                          (writing-habit-table-block-minutes b))
                                    out))))
                        out))
             (facts (writing-habit-insights-rank-facts tasks deadlines planned))
             (plain (lambda () (writing-habit-insights-plain-rank tasks deadlines planned)))
             (parse (lambda (reply) (writing-habit-insights-parse-rank reply tasks)))
             (answer (if (writing-habit-coach-allowed-p)
                         (writing-habit-checks--with-db db
                           (writing-habit-insights--ask
                            "grasscatcher" facts plain parse db
                            (writing-habit-checks-week-monday table)
                            (writing-habit-insights-rank-schema (length tasks))))
                       (writing-habit-insights--ask "grasscatcher" facts plain parse)))
             (order (plist-get answer :data)))
        (writing-habit-insights--show
         "Ranked grasscatcher"
         (concat (string-join (writing-habit-insights-rank-lines order) "\n")
                 (format "\n\nThe order is a %s." (plist-get answer :source))))
        (let ((accepted (funcall writing-habit-insights-confirm "Use this order? ")))
          (when accepted
            (let ((lines (writing-habit-insights-reorder
                          after "grasscatcher" (mapcar (lambda (i) (plist-get i :index)) order))))
              (writing-habit-table--set-after table lines)
              (writing-habit-table--apply table)
              (message "Put the grasscatcher in the coach's order")))
          (when (plist-get answer :log-id)
            (writing-habit-checks--with-db db
              (writing-habit-insights--settle db answer accepted)))
          order)))))

;;;; Patterns across weeks (Rule 7)

(defun writing-habit-insights-weekday-rows (db &optional end-week)
  "Return adherence by weekday in DB, over every week up to END-WEEK."
  (let* ((clause (if end-week " AND week_start <= ?" ""))
         (params (if end-week (list end-week) nil))
         (planned (sqlite-select
                   db (concat "SELECT (CAST(strftime('%w', day) AS INTEGER) + 6) % 7, SUM(planned_min),"
                              " COUNT(DISTINCT week_start) FROM plan_block WHERE 1" clause
                              " GROUP BY 1 ORDER BY 1")
                   params))
         (actual (sqlite-select
                  db (concat "SELECT (CAST(strftime('%w', day) AS INTEGER) + 6) % 7, SUM(actual_min)"
                             " FROM session WHERE 1" clause " GROUP BY 1")
                  params))
         (rows '()))
    (dolist (p planned)
      (let ((minutes (nth 1 p)))
        (when (and minutes (> minutes 0))
          (push (list :kind "weekday"
                      :group (aref writing-habit-checks-day-names (nth 0 p))
                      :adherence (writing-habit-insights--round
                                  (min (/ (float (or (cadr (assoc (nth 0 p) actual)) 0)) minutes) 9.99)
                                  2)
                      :weeks (nth 2 p))
                rows))))
    (nreverse rows)))

(defun writing-habit-insights-group-rows (db &optional end-week)
  "Return every grouping the dashboards show for DB up to END-WEEK, as one list.
END-WEEK limits the weekday rows; the month, event, and schedule views
cover every week, as in the Python port."
  (append (writing-habit-insights-weekday-rows db end-week)
          (apply #'append
                 (mapcar (lambda (q)
                           (condition-case nil
                               (delq nil (mapcar (lambda (r)
                                                   (when (nth 1 r)
                                                     (list :kind (car q) :group (nth 0 r)
                                                           :adherence (nth 1 r) :weeks (nth 2 r))))
                                                 (sqlite-select db (cdr q))))
                             (error nil)))
                         '(("month" . "SELECT month, adherence, weeks FROM v_month_overall")
                           ("event" . "SELECT tag, adherence, weeks FROM v_context_overall")
                           ("schedule shape" . "SELECT schedule_code, adherence, weeks FROM v_schedule_overall"))))))

(defun writing-habit-insights-overall (db &optional end-week)
  "Return the share of planned minutes kept in DB up to END-WEEK, or nil."
  (let ((row (car (sqlite-select
                   db (concat "SELECT SUM(planned_min), SUM(actual_min) FROM v_week_overall"
                              (if end-week " WHERE week_start <= ?" ""))
                   (if end-week (list end-week) nil)))))
    (when (and row (car row) (> (car row) 0))
      (writing-habit-insights--round (/ (float (cadr row)) (car row)) 2))))

(defun writing-habit-insights-strongest (rows overall &optional min-weeks top)
  "Return the ROWS whose adherence differs most from OVERALL.
A group with fewer than MIN-WEEKS weeks is not a pattern yet.  Return at
most TOP rows, two by default."
  (let* ((min-weeks (or min-weeks writing-habit-insights-min-weeks))
         (usable (seq-filter (lambda (r) (and (>= (plist-get r :weeks) min-weeks)
                                              (plist-get r :adherence)))
                             rows))
         (key (lambda (r) (list (- (abs (- (plist-get r :adherence) overall)))
                                (plist-get r :kind) (format "%s" (plist-get r :group))))))
    (seq-take (sort (copy-sequence usable)
                    (lambda (a b)
                      (let ((ka (funcall key a)) (kb (funcall key b)))
                        (or (< (nth 0 ka) (nth 0 kb))
                            (and (= (nth 0 ka) (nth 0 kb))
                                 (or (string< (nth 1 ka) (nth 1 kb))
                                     (and (string= (nth 1 ka) (nth 1 kb))
                                          (string< (nth 2 ka) (nth 2 kb)))))))))
              (or top 2))))

(defun writing-habit-insights-group-label (row)
  "Name the group of ROW the way a writer would say it."
  (let ((kind (plist-get row :kind)) (group (plist-get row :group)))
    (cond ((equal kind "weekday") (format "your %s blocks" group))
          ((equal kind "month") (format "your weeks in %s" group))
          ((equal kind "event") (format "your weeks tagged %s" group))
          (t (format "your weeks planned with the %s schedule" group)))))

(defun writing-habit-insights-pattern-sentence (row overall)
  "Write the pattern of ROW plainly, against OVERALL."
  (let* ((side (if (>= (plist-get row :adherence) overall) "more" "less"))
         (weeks (plist-get row :weeks))
         (label (writing-habit-insights-group-label row))
         (text (format "%s kept %d%% of their planned minutes over %d weeks, %s than your overall %d%%."
                       (concat (upcase (substring label 0 1)) (substring label 1))
                       (writing-habit-insights--pct (plist-get row :adherence)) weeks side
                       (writing-habit-insights--pct overall))))
    (if (< weeks 8)
        (format "%s %d weeks of one writer is a small sample." text weeks)
      text)))

(defun writing-habit-insights-plain-change (patterns overall)
  "Propose one change to the template from PATTERNS and OVERALL."
  (let ((low (seq-filter (lambda (p) (< (plist-get p :adherence) overall)) patterns))
        (high (seq-filter (lambda (p) (>= (plist-get p :adherence) overall)) patterns)))
    (cond (low (format "Plan fewer minutes for %s next week, and move the rest to times you keep more often."
                       (writing-habit-insights-group-label (car low))))
          (high (format "Build more of next week's template like %s, because those are the blocks you keep."
                        (writing-habit-insights-group-label (car high))))
          (t "Keep the template as it is for a few more weeks and look again."))))

(defun writing-habit-insights-weeks-facts (patterns overall)
  "Return the facts of PATTERNS and OVERALL for the model."
  (string-join
   (cons (format "Overall share of planned minutes kept: %d%%" (writing-habit-insights--pct overall))
         (mapcar (lambda (p)
                   (format "Pattern: %s %s, kept %d%% of planned minutes over %d weeks"
                           (plist-get p :kind) (plist-get p :group)
                           (writing-habit-insights--pct (plist-get p :adherence)) (plist-get p :weeks)))
                 patterns))
   "\n"))

(defun writing-habit-insights-parse-weeks (reply patterns overall)
  "Return (:patterns :change) from REPLY, keeping sentences true to PATTERNS.
OVERALL is the share of the plan kept across all weeks.
A sentence must state its pattern's percentage and hold no number that
the facts do not; otherwise the plain sentence takes its place."
  (unless (hash-table-p reply) (error "The reply held no patterns"))
  (let* ((allowed (cons 8 (writing-habit-insights--numbers
                           (writing-habit-insights-weeks-facts patterns overall))))
         (sentences (mapcar #'writing-habit-coach-clean (gethash "patterns" reply)))
         (used '()) (out '()))
    (dolist (p patterns)
      (let ((match (cl-position-if
                    (lambda (s) (and (string-search (number-to-string
                                                     (writing-habit-insights--pct (plist-get p :adherence)))
                                                    s)
                                     (writing-habit-insights--subset-p s allowed)))
                    sentences)))
        (while (and match (memq match used))
          (setq match (cl-position-if
                       (lambda (s) (and (string-search (number-to-string
                                                        (writing-habit-insights--pct (plist-get p :adherence)))
                                                       s)
                                        (writing-habit-insights--subset-p s allowed)))
                       sentences :start (1+ match))))
        (if match
            (progn (push match used) (push (nth match sentences) out))
          (push (writing-habit-insights-pattern-sentence p overall) out))))
    (let ((change (writing-habit-coach-clean (gethash "change" reply))))
      (when (or (string-empty-p change) (not (writing-habit-insights--subset-p change allowed)))
        (setq change (writing-habit-insights-plain-change patterns overall)))
      (list :patterns (nreverse out) :change change))))

(defun writing-habit-insights-read-weeks-answer (db &optional end-week log)
  "Return the reading of the weeks in DB up to END-WEEK as a plist.
The plist holds :data (:patterns :change), :text, :source, :rows, and
:overall.  The call is logged when LOG is non-nil and the model is allowed."
  (let ((overall (writing-habit-insights-overall db end-week)))
    (cond
     ((null overall)
      (list :data (list :patterns nil :change "") :rows nil
            :text "There are no planned weeks to read yet." :source "no data"))
     (t
      (let ((patterns (writing-habit-insights-strongest
                       (writing-habit-insights-group-rows db end-week) overall)))
        (if (null patterns)
            (list :data (list :patterns nil :change "") :rows nil
                  :text (format "No group has %d weeks behind it yet, so there is no pattern to report. Keep planning and tracking, and look again."
                                writing-habit-insights-min-weeks)
                  :source "no pattern yet")
          (let ((answer (writing-habit-insights--ask
                         "weeks" (writing-habit-insights-weeks-facts patterns overall)
                         (lambda () (list :patterns (mapcar (lambda (p) (writing-habit-insights-pattern-sentence p overall))
                                                            patterns)
                                          :change (writing-habit-insights-plain-change patterns overall)))
                         (lambda (reply) (writing-habit-insights-parse-weeks reply patterns overall))
                         (and log db) end-week)))
            (let ((data (plist-get answer :data)))
              (append (list :text (string-join (append (plist-get data :patterns)
                                                       (list (plist-get data :change)))
                                               "\n")
                            :rows patterns :overall overall)
                      answer)))))))))

;;;###autoload
(defun writing-habit-insights-read-weeks ()
  "Show the two strongest patterns in the record and one change to try."
  (interactive)
  (let* ((table (ignore-errors (writing-habit-table-current)))
         (end (and table (writing-habit-table-path table)
                   (ignore-errors (writing-habit-checks-week-monday table)))))
    (writing-habit-checks--with-db db
      (let* ((answer (writing-habit-insights-read-weeks-answer db end t))
             (rows (plist-get answer :rows)))
        (writing-habit-insights--show
         "Patterns in your weeks"
         (concat
          (if rows
              (concat "The numbers behind the reading:\n"
                      (mapconcat (lambda (r) (format "%s %s: %d%% kept over %d weeks"
                                                     (plist-get r :kind) (plist-get r :group)
                                                     (writing-habit-insights--pct (plist-get r :adherence))
                                                     (plist-get r :weeks)))
                                 rows "\n")
                      (format "\nall weeks: %d%% kept\n\n"
                              (writing-habit-insights--pct (plist-get answer :overall))))
            "")
          (plist-get answer :text)
          (format "\n\nThe reading is a %s." (plist-get answer :source))))
        (when (plist-get answer :log-id)
          (writing-habit-insights--settle
           db answer (funcall writing-habit-insights-confirm "Keep the suggestion? ")))
        answer))))

;;;; Sizing the week (Rule 7)

(defun writing-habit-insights-size (db week-start &optional schedule-code tags)
  "Suggest how many minutes to plan for the week starting WEEK-START in DB.
The suggestion is the median of the minutes actually written in up to
eight recent planned weeks before it, rounded to 15 minutes.  When at
least two earlier weeks share an event tag of TAGS, or at least three
share SCHEDULE-CODE, those weeks are used instead.  Return a plist, or
nil when there is no earlier planned week."
  (let ((weeks (mapcar (lambda (r) (list :week (nth 0 r) :planned (nth 1 r) :actual (nth 2 r) :code (nth 3 r)))
                       (sqlite-select
                        db (concat "SELECT o.week_start, o.planned_min, o.actual_min, w.schedule_code"
                                   " FROM v_week_overall o LEFT JOIN plan_week w USING(week_start)"
                                   " WHERE o.week_start < ? AND o.planned_min > 0"
                                   " ORDER BY o.week_start DESC")
                        (list week-start)))))
    (when weeks
      (let* ((window writing-habit-insights-size-window)
             (chosen (seq-take weeks window))
             (basis (format "your last %d planned weeks" (min (length weeks) window)))
             (by-tag nil))
        (catch 'found
          (dolist (tag tags)
            (let* ((tagged-weeks (mapcar #'car (sqlite-select db "SELECT week_start FROM week_context WHERE tag = ?"
                                                              (list tag))))
                   (tagged (seq-filter (lambda (w) (member (plist-get w :week) tagged-weeks)) weeks)))
              (when (>= (length tagged) 2)
                (setq chosen (seq-take tagged window)
                      basis (format "your %d earlier %s weeks" (length chosen) tag)
                      by-tag t)
                (throw 'found nil)))))
        (when (and (not by-tag) schedule-code)
          (let ((same (seq-filter (lambda (w) (equal (plist-get w :code) schedule-code)) weeks)))
            (when (>= (length same) 3)
              (setq chosen (seq-take same window)
                    basis (format "your %d earlier weeks with the %s plan" (length chosen) schedule-code)))))
        (let* ((actual (mapcar (lambda (w) (plist-get w :actual)) chosen))
               (planned (mapcar (lambda (w) (plist-get w :planned)) chosen))
               (current (caar (sqlite-select db "SELECT planned_min FROM v_week_overall WHERE week_start = ?"
                                             (list week-start)))))
          (list :minutes (* 15 (writing-habit-insights--round
                                (/ (float (writing-habit-insights--median actual)) 15.0)))
                :basis basis :weeks (length chosen)
                :median-planned (truncate (writing-habit-insights--median planned))
                :kept (when (> (apply #'+ planned) 0)
                        (writing-habit-insights--round (/ (float (apply #'+ actual)) (apply #'+ planned)) 2))
                :current current))))))

(defun writing-habit-insights-plain-size (size)
  "Write the suggestion SIZE plainly."
  (let ((text (format "Plan about %s of writing next week. That is what you actually wrote in a typical week of %s, where you planned %s."
                      (writing-habit-insights--hours (plist-get size :minutes)) (plist-get size :basis)
                      (writing-habit-insights--hours (plist-get size :median-planned)))))
    (if (and (plist-get size :current) (> (plist-get size :current) (plist-get size :minutes)))
        (format "%s The week as planned holds %s, so trimming it toward the suggestion makes the plan easier to keep."
                text (writing-habit-insights--hours (plist-get size :current)))
      text)))

(defun writing-habit-insights-size-facts (size)
  "Return the facts of SIZE for the model."
  (concat (format "Suggested minutes for next week: %d\nBased on: %s\nTypical minutes planned in those weeks: %d\nShare of planned minutes kept in those weeks: %s%%"
                  (plist-get size :minutes) (plist-get size :basis) (plist-get size :median-planned)
                  (if (plist-get size :kept) (writing-habit-insights--pct (plist-get size :kept)) "unknown"))
          (if (plist-get size :current)
              (format "\nMinutes in next week's plan as it stands: %d" (plist-get size :current))
            "")))

(defun writing-habit-insights-explain-size (size &optional db week)
  "Explain SIZE, logging under WEEK in DB.  The number itself stays as it is."
  (writing-habit-insights--ask
   "size" (writing-habit-insights-size-facts size)
   (lambda () (writing-habit-insights-plain-size size))
   (lambda (reply)
     (let ((text (writing-habit-coach-clean reply)))
       (if (or (string-search (number-to-string (plist-get size :minutes)) text)
               (string-search (writing-habit-insights--hours (plist-get size :minutes)) text))
           text
         (concat (writing-habit-insights-plain-size size) " " text))))
   db week))

;;;###autoload
(defun writing-habit-insights-size-week ()
  "Suggest how many minutes to plan for the open week."
  (interactive)
  (let* ((table (writing-habit-table-current))
         (week (writing-habit-checks-week-monday table))
         (code (car (writing-habit-table-code-or-problem table)))
         (current (apply #'+ (mapcar #'writing-habit-table-block-minutes (writing-habit-table-blocks table)))))
    (writing-habit-checks--with-db db
      (let* ((tags (mapcar #'car (sqlite-select db "SELECT tag FROM week_context WHERE week_start = ?"
                                                (list week))))
             (size (writing-habit-insights-size db week code tags)))
        (if (null size)
            (progn (message "There are no earlier planned weeks to size this week from yet") nil)
          (setq size (plist-put size :current current))
          (let ((answer (writing-habit-insights-explain-size size db week)))
            (writing-habit-insights--show
             "A plan size for this week"
             (format "%s\n\nThe note is a %s." (plist-get answer :data) (plist-get answer :source)))
            (when (plist-get answer :log-id)
              (writing-habit-insights--settle
               db answer (funcall writing-habit-insights-confirm "Keep this size? ")))
            answer))))))

;;;; Weeks with the coach and without it (Rule 10)

(defalias 'writing-habit-insights-comparison #'writing-habit-records-comparison
  "Compare the plan kept in weeks with and without the coach.
See `writing-habit-records-comparison'.")

(defalias 'writing-habit-insights-comparison-text #'writing-habit-records-comparison-text
  "Write a comparison as the sentence of the Python dashboard.
See `writing-habit-records-comparison-text'.")

;;;###autoload
(defun writing-habit-insights-compare-coach ()
  "Show the share of the plan kept in weeks with the coach and without it."
  (interactive)
  (writing-habit-checks--with-db db
    (let ((compared (writing-habit-insights-comparison db)))
      (message "%s"
               (if compared
                   (writing-habit-insights-comparison-text compared)
                 "Each side needs two planned weeks before the weeks with the coach and without it can be compared."))
      compared)))

(provide 'writing-habit-insights)
;;; writing-habit-insights.el ends here
