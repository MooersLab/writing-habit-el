;;; writing-habit-records.el --- What the week's records hold -*- lexical-binding: t; -*-

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

;; The parts of the record that both the dashboard and the coach read: each
;; planned block with what happened to it, the objectives met, the personal
;; bests, and the share of the plan kept in weeks with the coach and without
;; it.  They need only the database, so the HTML dashboard draws them
;; without writing-schedule.el.  This is a port of the matching functions
;; of review.py and coach.py.

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defun writing-habit-records--abs (iso)
  "Return the absolute day number of the ISO date string ISO."
  (calendar-absolute-from-gregorian
   (list (string-to-number (substring iso 5 7))
         (string-to-number (substring iso 8 10))
         (string-to-number (substring iso 0 4)))))

(defun writing-habit-records--iso (day)
  "Return the absolute day number DAY as an ISO date string."
  (let ((date (calendar-gregorian-from-absolute day)))
    (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date))))

(defun writing-habit-records--today ()
  "Return today as an ISO date string."
  (format-time-string "%Y-%m-%d"))

(defun writing-habit-records--minute (clock)
  "Return the minute of the day of the HH:MM string CLOCK, or nil."
  (when (and clock (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)" clock))
    (+ (* 60 (string-to-number (match-string 1 clock)))
       (string-to-number (match-string 2 clock)))))

(defun writing-habit-records--tables (db)
  "Return the names of the tables and views of DB."
  (mapcar #'car (sqlite-select db "SELECT name FROM sqlite_master
 WHERE type IN ('table','view')")))

(defun writing-habit-records--key< (a b)
  "Compare the key lists A and B element by element.
Numbers compare by value, strings by `string<', and booleans as nil
before t."
  (catch 'done
    (while (or a b)
      (let ((x (car a)) (y (car b)))
        (cond ((and (numberp x) (numberp y))
               (unless (= x y) (throw 'done (< x y))))
              ((and (stringp x) (stringp y))
               (unless (string= x y) (throw 'done (string< x y))))
              (t (let ((x (if x 1 0)) (y (if y 1 0)))
                   (unless (= x y) (throw 'done (< x y)))))))
      (setq a (cdr a) b (cdr b)))
    nil))

(defun writing-habit-records--first-max (key items)
  "Return the first of ITEMS with the largest KEY, as Python's max does."
  (let ((best nil) (best-key nil))
    (dolist (item items)
      (let ((k (funcall key item)))
        (when (or (null best-key) (writing-habit-records--key< best-key k))
          (setq best item best-key k))))
    best))

;;;; Block outcomes and the done list

(defun writing-habit-records-block-outcomes (db week-start &optional today)
  "Return plists of each planned block of WEEK-START in DB and its status.
The status is kept at half the planned minutes or more, partial below
that, missed at none, and ahead for a block after TODAY.  The plist's
:review holds the block's focus and objectives when it was reviewed.  A
port of review.block_outcomes."
  (let* ((today (or today (writing-habit-records--today)))
         (blocks (sqlite-select db "SELECT b.day, b.start_time, b.end_time, b.planned_min, p.code, c.name
 FROM plan_block b JOIN project p USING(project_id) JOIN category c USING(category_id)
 WHERE b.week_start = ? ORDER BY b.day, b.start_time, p.code" (list week-start)))
         (sessions (sqlite-select db "SELECT s.day, s.start_time, s.end_time, s.actual_min, p.code
 FROM session s JOIN project p USING(project_id) WHERE s.week_start = ?" (list week-start)))
         (reviews (when (member "session_review" (writing-habit-records--tables db))
                    (sqlite-select db "SELECT day, start_time, code, focus, objectives_committed,
 objectives_met FROM session_review WHERE week_start = ?" (list week-start))))
         (used '()) (out '()))
    (dolist (b blocks)
      (let ((first (writing-habit-records--minute (nth 1 b)))
            (last (writing-habit-records--minute (nth 2 b)))
            (planned (nth 3 b)) (actual 0) (index -1))
        (dolist (s sessions)
          (setq index (1+ index))
          (when (and (equal (nth 0 s) (nth 0 b)) (equal (nth 4 s) (nth 4 b)))
            (let ((s-first (writing-habit-records--minute (nth 1 s)))
                  (s-last (writing-habit-records--minute (nth 2 s))))
              (if (or (null s-first) (null s-last))
                  (let ((key (list (nth 0 b) (nth 4 b) index)))
                    (unless (member key used)
                      (push key used)
                      (cl-incf actual (min (nth 3 s) planned))))
                (when (<= s-last s-first) (cl-incf s-last (* 24 60)))
                (cl-incf actual (max 0 (- (min last s-last) (max first s-first))))))))
        (setq actual (min actual planned))
        (push (list :day (nth 0 b) :start (nth 1 b) :end (nth 2 b) :planned planned
                    :code (nth 4 b) :category (nth 5 b) :actual actual
                    :review (let ((r (seq-find (lambda (r) (and (equal (nth 0 r) (nth 0 b))
                                                               (equal (nth 1 r) (nth 1 b))
                                                               (equal (nth 2 r) (nth 4 b))))
                                               reviews)))
                              (when r (list :focus (nth 3 r) :committed (nth 4 r) :met (nth 5 r))))
                    :status (cond ((string< today (nth 0 b)) "ahead")
                                  ((= actual 0) "missed")
                                  ((>= actual (* 0.5 planned)) "kept")
                                  (t "partial")))
              out)))
    (nreverse out)))

(defun writing-habit-records-has-records (db week-start)
  "Return non-nil when an objective or a review is in DB for WEEK-START."
  (let ((tables (writing-habit-records--tables db)))
    (and (member "block_objective" tables) (member "session_review" tables)
         (or (sqlite-select db "SELECT 1 FROM block_objective WHERE week_start = ? LIMIT 1"
                            (list week-start))
             (sqlite-select db "SELECT 1 FROM session_review WHERE week_start = ? LIMIT 1"
                            (list week-start)))
         t)))

(defun writing-habit-records-done-list (db week-start)
  "Return plists (:day :start :code :text) of the objectives met.
They are the objectives of the week of WEEK-START in DB, in time order."
  (mapcar (lambda (r) (list :day (nth 0 r) :start (nth 1 r) :code (nth 2 r) :text (nth 3 r)))
          (sqlite-select db "SELECT day, start_time, code, text FROM block_objective
 WHERE week_start = ? AND done = 1 ORDER BY day, start_time, position, objective_id"
                         (list week-start))))

(defun writing-habit-records-planned-weeks (db week-start)
  "Return how many weeks up to WEEK-START in DB carry a plan."
  (caar (sqlite-select db "SELECT COUNT(*) FROM v_week_overall WHERE planned_min > 0 AND week_start <= ?"
                       (list week-start))))

;;;; Personal bests

(defun writing-habit-records-personal-bests (db week-start)
  "Return (LABEL VALUE WEEK THIS-WEEK) for each personal best in DB.
Only weeks up to WEEK-START count."
  (let* ((weeks (sqlite-select db "SELECT week_start, planned_min, actual_min, adherence
 FROM v_week_overall WHERE week_start <= ? ORDER BY week_start" (list week-start)))
         (planned (seq-filter (lambda (w) (and (nth 1 w) (> (nth 1 w) 0) (nth 3 w))) weeks))
         (out '()))
    (when planned
      (let ((best (writing-habit-records--first-max
                   (lambda (w) (list (min (nth 3 w) 1.0) (nth 2 w))) planned)))
        (push (list "Most of the plan kept" (format "%d%%" (round (* (min (nth 3 best) 1.0) 100)))
                    (nth 0 best) (equal (nth 0 best) week-start))
              out)))
    (when weeks
      (let ((best (writing-habit-records--first-max (lambda (w) (list (nth 2 w))) weeks)))
        (when (> (nth 2 best) 0)
          (push (list "Most minutes written" (format "%d h %02d min" (/ (nth 2 best) 60) (% (nth 2 best) 60))
                      (nth 0 best) (equal (nth 0 best) week-start))
                out))))
    (let ((run 0) (longest 0) (longest-end nil) (previous nil))
      (dolist (row (sqlite-select db "SELECT day FROM v_day_actual WHERE worked = 1 AND week_start <= ?
 ORDER BY day" (list week-start)))
        (let ((day (writing-habit-records--abs (car row))))
          (setq run (if (and previous (= (- day previous) 1)) (1+ run) 1))
          (when (> run longest) (setq longest run longest-end day))
          (setq previous day)))
      (when (> longest 0)
        (let* ((weekday (mod (1- (calendar-day-of-week
                                  (calendar-gregorian-from-absolute longest-end)))
                             7))
               (end-week (writing-habit-records--iso (- longest-end weekday))))
          (push (list "Longest run of writing days"
                      (format "%d day%s" longest (if (= longest 1) "" "s"))
                      end-week (equal end-week week-start))
                out))))
    (when (member "v_week_review" (writing-habit-records--tables db))
      (let* ((rows (sqlite-select db "SELECT week_start, met, mean_focus, reviews FROM v_week_review
 WHERE week_start <= ?" (list week-start)))
             (met (seq-filter (lambda (r) (and (nth 1 r) (> (nth 1 r) 0))) rows))
             (focused (seq-filter (lambda (r) (and (nth 2 r) (>= (nth 3 r) 3))) rows)))
        (when met
          (let ((best (writing-habit-records--first-max (lambda (r) (list (nth 1 r))) met)))
            (push (list "Most objectives met" (number-to-string (nth 1 best))
                        (nth 0 best) (equal (nth 0 best) week-start))
                  out)))
        (when focused
          (let ((best (writing-habit-records--first-max (lambda (r) (list (nth 2 r))) focused)))
            (push (list "Most focused week" (format "%.1f of 5" (nth 2 best))
                        (nth 0 best) (equal (nth 0 best) week-start))
                  out)))))
    (nreverse out)))

;;;; Weeks with the coach and without it (Rule 10)

(defun writing-habit-records--round2 (value)
  "Round VALUE to two places, ties to even, as Python's round does."
  (/ (fround (* value 100.0)) 100.0))

(defun writing-habit-records-comparison (db &optional end-week)
  "Compare the plan kept in DB in weeks with and without the coach.
Only weeks up to END-WEEK count.  Return a plist, or nil until each side
has two planned weeks."
  (when (member "ai_log" (writing-habit-records--tables db))
    (let* ((with-ai (mapcar #'car (sqlite-select db "SELECT DISTINCT week_start FROM ai_log WHERE week_start IS NOT NULL")))
           (rows (sqlite-select db (concat "SELECT week_start, planned_min, actual_min FROM v_week_overall"
                                           " WHERE planned_min > 0"
                                           (if end-week " AND week_start <= ?" ""))
                                (if end-week (list end-week) nil)))
           (on (seq-filter (lambda (r) (member (car r) with-ai)) rows))
           (off (seq-remove (lambda (r) (member (car r) with-ai)) rows))
           (kept (lambda (group) (writing-habit-records--round2
                                  (/ (float (apply #'+ (mapcar #'caddr group)))
                                     (apply #'+ (mapcar #'cadr group)))))))
      (when (and (>= (length on) 2) (>= (length off) 2))
        (list :with-ai (length on) :with-ai-kept (funcall kept on)
              :without-ai (length off) :without-ai-kept (funcall kept off))))))

(defun writing-habit-records-comparison-text (compared)
  "Write COMPARED as the sentence of the Python dashboard."
  (format "Weeks with the coach: %d, %d%% of the plan kept. Weeks without it: %d, %d%% kept. A few weeks on each side describe your record. They do not prove that the coach helped."
          (plist-get compared :with-ai)
          (truncate (fround (* 100 (plist-get compared :with-ai-kept))))
          (plist-get compared :without-ai)
          (truncate (fround (* 100 (plist-get compared :without-ai-kept))))))

(provide 'writing-habit-records)
;;; writing-habit-records.el ends here
