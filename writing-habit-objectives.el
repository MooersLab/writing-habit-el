;;; writing-habit-objectives.el --- A time block's objectives, with suggestions from the coach -*- lexical-binding: t; -*-

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

;; The twin of the Objectives part of the Python review panel.  A time
;; block is for one to three objectives, each a result that could be
;; shown at the end of the block.  They live in the block_objective table
;; of the habit database, keyed by the block's day, start, and project
;; code, so both ports read and write the same rows.
;;
;; Commands:
;;   `writing-habit-objectives-show'     list the objectives of the block at
;;                                       point, in the week buffer or the
;;                                       hour view
;;   `writing-habit-objectives-suggest'  have the local model draft one to
;;                                       three objectives and an if-then
;;                                       plan, then keep the ones you accept
;;
;; In the objectives buffer, a adds an objective, x or RET checks it off
;; or back on, s asks the coach, g reads the database again, and q quits.
;; A check is written at once, as in the Python panel, because the
;; objectives are the plan for the block.
;;
;; The coach sees only the facts the Python port sends: the task typed at
;; the prompt, the project's name and deadline, the block's length, the
;; grasscatcher tasks waiting for the project, the last next-step note,
;; and, when the project keeps missing the same objective, the sentence
;; that says so.  The call is logged in ai_log like every other coach call.

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-coach)
(require 'writing-habit-db)
(require 'writing-habit-lists)

(declare-function writing-habit-table-current "writing-habit-table-mode")
(declare-function writing-habit-table--row-at-point "writing-habit-table-mode")
(declare-function writing-habit-table--cell-at-point "writing-habit-table-mode")
(declare-function writing-habit-table-blocks "writing-habit-table")
(declare-function writing-habit-table-block-row "writing-habit-table")
(declare-function writing-habit-table-block-column "writing-habit-table")
(declare-function writing-habit-table-block-offset "writing-habit-table")
(declare-function writing-habit-table-block-start "writing-habit-table")
(declare-function writing-habit-table-block-end "writing-habit-table")
(declare-function writing-habit-table-block-letter "writing-habit-table")
(declare-function writing-habit-table-block-minutes "writing-habit-table")
(declare-function writing-habit-table-project-info "writing-habit-table")
(declare-function writing-habit-table-after "writing-habit-table")
(declare-function writing-habit-checks-week-monday "writing-habit-checks")
(declare-function writing-habit-helpers-stuck-check "writing-habit-helpers")
(declare-function writing-habit-helpers-stuck-task "writing-habit-helpers")
(declare-function writing-habit-helpers-stuck-sentence "writing-habit-helpers")
(declare-function writing-habit-helpers-stuck-normalize "writing-habit-helpers")
(defvar writing-habit-table-mode)
(defvar writing-habit-hours--week-buffer)
(defvar writing-habit-dispatch--last-db)
(defvar writing-habit-default-db)

(defconst writing-habit-objectives-limit 3
  "More objectives than this make a block hard to win.")

(defconst writing-habit-objectives-plain-advice
  "Write one to three objectives, each a result you could show someone at the end of the block."
  "The advice given when the coach has nothing to draft.")

;;;; The block

(cl-defstruct (writing-habit-objectives-block
               (:constructor writing-habit-objectives-block-create)
               (:copier nil))
  "One time block of the week.  DAY is its ISO date and CODE its legend code."
  day start end code name due minutes week-start waiting)

(defun writing-habit-objectives-add-days (iso days)
  "Return the ISO date DAYS after ISO, a YYYY-MM-DD string."
  (let* ((parts (mapcar #'string-to-number (split-string iso "-")))
         (abs (+ days (calendar-absolute-from-gregorian
                       (list (nth 1 parts) (nth 2 parts) (nth 0 parts)))))
         (date (calendar-gregorian-from-absolute abs)))
    (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date))))

(defun writing-habit-objectives-block-text (block)
  "Return a line such as B 05:00-07:00 1006AIrxOpt for BLOCK."
  (string-trim-right
   (format "%s %s-%s %s" (writing-habit-objectives-block-code block)
           (writing-habit-objectives-block-start block)
           (writing-habit-objectives-block-end block)
           (or (writing-habit-objectives-block-name block) ""))))

(defun writing-habit-objectives--from-table (model block)
  "Return the objectives block for BLOCK, a block of the table MODEL."
  (require 'writing-habit-checks)
  (let* ((monday (writing-habit-checks-week-monday model))
         (code (writing-habit-table-block-letter block))
         (info (writing-habit-table-project-info model code)))
    (writing-habit-objectives-block-create
     :day (writing-habit-objectives-add-days monday (writing-habit-table-block-offset block))
     :start (writing-habit-table-block-start block)
     :end (writing-habit-table-block-end block)
     :code code
     :name (plist-get info :name)
     :due (plist-get info :due)
     :minutes (writing-habit-table-block-minutes block)
     :week-start monday
     :waiting (mapcar #'writing-habit-lists-entry-description
                      (seq-filter (lambda (e) (equal (writing-habit-lists-entry-code e) code))
                                  (writing-habit-lists-entries
                                   (writing-habit-table-after model) "grasscatcher"))))))

(defun writing-habit-objectives--choose (blocks)
  "Ask which of BLOCKS, blocks of one row, is meant."
  (let* ((choices (mapcar (lambda (b)
                            (cons (format "%s on day %d" (writing-habit-table-block-letter b)
                                          (1+ (writing-habit-table-block-offset b)))
                                  b))
                          blocks)))
    (cdr (assoc (completing-read "Block: " choices nil t) choices))))

(defun writing-habit-objectives-block-at-point ()
  "Return the objectives block at point.
Point may rest on a block in the hour view, or on a project code in a
time-block row of the weekly table.  On the Time cell of a row with
several blocks, ask which one is meant."
  (cond
   ((get-text-property (point) 'writing-habit-hours-block)
    (let ((block (get-text-property (point) 'writing-habit-hours-block))
          (week writing-habit-hours--week-buffer))
      (unless (buffer-live-p week)
        (user-error "The week buffer is gone; open it and run the command again"))
      (with-current-buffer week
        (writing-habit-objectives--from-table (writing-habit-table-current) block))))
   ((and (boundp 'writing-habit-table-mode) writing-habit-table-mode)
    (let* ((model (writing-habit-table-current))
           (row (writing-habit-table--row-at-point model))
           (cell (writing-habit-table--cell-at-point))
           (blocks (seq-filter (lambda (b) (eql (writing-habit-table-block-row b) row))
                               (writing-habit-table-blocks model)))
           (block (or (seq-find (lambda (b) (eql (writing-habit-table-block-column b) cell))
                                blocks)
                      (and (= (length blocks) 1) (car blocks))
                      (and blocks (writing-habit-objectives--choose blocks)))))
      (unless block
        (user-error "Put point on a project code in a time block"))
      (writing-habit-objectives--from-table model block)))
   (t (user-error "Open the week's table, or its hour view, and put point on a block"))))

;;;; The database

(defun writing-habit-objectives--db-path ()
  "Return the habit database to use, asking when none is known."
  (or (and (boundp 'writing-habit-dispatch--last-db) writing-habit-dispatch--last-db)
      (and (boundp 'writing-habit-default-db) writing-habit-default-db)
      (read-file-name "Habit database: " nil nil t)))

(defun writing-habit-objectives--table-p (db table)
  "Return non-nil when DB has TABLE."
  (sqlite-select db "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?"
                 (list table)))

(defun writing-habit-objectives-rows (db day start code)
  "Return (ID TEXT DONE ORIGIN) for each objective of a block in DB, in order.
The block is keyed by DAY, START, and CODE, as in the Python port."
  (when (writing-habit-objectives--table-p db "block_objective")
    (sqlite-select db "SELECT objective_id, text, done, origin FROM block_objective
 WHERE day = ? AND start_time = ? AND code = ? ORDER BY position, objective_id"
                   (list day start code))))

(defun writing-habit-objectives-add (db day start code text &optional origin)
  "Add TEXT at the end of the objectives of a block in DB and return its id.
The block is keyed by DAY, START, and CODE.  ORIGIN is typed, the
default, grasscatcher, or ai."
  (let ((text (string-trim (or text ""))))
    (when (string-empty-p text) (user-error "An objective needs some text"))
    (let ((position (caar (sqlite-select db "SELECT COALESCE(MAX(position), -1) + 1
 FROM block_objective WHERE day = ? AND start_time = ? AND code = ?"
                                         (list day start code)))))
      (sqlite-execute db "INSERT INTO block_objective(day, start_time, code, position, text, origin)
 VALUES (?, ?, ?, ?, ?, ?)" (list day start code position text (or origin "typed")))
      (caar (sqlite-select db "SELECT last_insert_rowid()")))))

(defun writing-habit-objectives-mark (db id done)
  "Mark objective ID of DB as DONE or not done."
  (sqlite-execute db "UPDATE block_objective SET done = ?,
 done_at = CASE WHEN ? THEN datetime('now') END WHERE objective_id = ?"
                  (list (if done 1 0) (if done 1 0) id)))

(defun writing-habit-objectives-last-next-step (db code day start)
  "Return the latest next-step note in DB for CODE before DAY and START."
  (when (writing-habit-objectives--table-p db "session_review")
    (caar (sqlite-select db "SELECT next_step FROM session_review WHERE code = ?
 AND next_step IS NOT NULL AND next_step != ''
 AND (day < ? OR (day = ? AND start_time < ?))
 ORDER BY day DESC, start_time DESC LIMIT 1" (list code day day start)))))

;;;; What the coach sees

(defun writing-habit-objectives--clean (text)
  "Collapse the white space of TEXT, as the Python port does."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (format "%s" (or text "")))))

(defun writing-habit-objectives-facts (task project minutes &optional deadline waiting
                                            last-step stuck)
  "Return the facts the objectives task may see, as the Python port writes them.
TASK is the task typed, PROJECT the project's name, MINUTES the block's
length, DEADLINE the project's deadline, WAITING the grasscatcher tasks
of the project, LAST-STEP the last next-step note, and STUCK the sentence
that says the project keeps missing its objectives.  Nothing else is sent."
  (let ((lines (list (format "Task: %s" (writing-habit-objectives--clean task))
                     (format "Project: %s" (writing-habit-objectives--clean project))
                     (format "Block length: %d minutes" minutes))))
    (when stuck
      (setq lines (append lines (list (format "Stuck: %s Break the task into smaller steps."
                                              (writing-habit-objectives--clean stuck))))))
    (when (and deadline (not (string-empty-p deadline)))
      (setq lines (append lines (list (format "Project deadline: %s" deadline)))))
    (when (and last-step (not (string-empty-p last-step)))
      (setq lines (append lines (list (format "Last next-step note: %s"
                                              (writing-habit-objectives--clean last-step))))))
    (when waiting
      (setq lines (append lines (list "Waiting tasks for this project:")
                          (mapcar (lambda (w) (format "  - %s" (writing-habit-objectives--clean w)))
                                  waiting))))
    (string-join lines "\n")))

(defun writing-habit-objectives-parse (reply)
  "Return (OBJECTIVES . IF-THEN) from REPLY, the parsed objectives reply.
At most three objectives are kept, and empty ones are dropped.  Signal an
error when the reply holds none."
  (unless (hash-table-p reply) (error "The reply held no objectives"))
  (let* ((items (seq-take (seq-remove #'string-empty-p
                                      (mapcar #'writing-habit-objectives--clean
                                              (gethash "objectives" reply)))
                          writing-habit-objectives-limit))
         (if-then (writing-habit-objectives--clean (gethash "if_then" reply))))
    (unless items (error "The reply held no objectives"))
    (cons items if-then)))

;;;; The objectives buffer

(defvar-local writing-habit-objectives--block nil
  "The block the objectives buffer shows.")

(defvar-local writing-habit-objectives--db nil
  "The habit database file the objectives buffer reads and writes.")

(defvar-local writing-habit-objectives--if-then nil
  "The if-then plan kept from the last suggestion for this block.")

(defmacro writing-habit-objectives--with-db (var path &rest body)
  "Open the database at PATH as VAR around BODY."
  (declare (indent 2))
  `(let ((,var (writing-habit-db-connect ,path)))
     (unwind-protect (progn ,@body) (writing-habit-db-close ,var))))

(defun writing-habit-objectives-string (block rows &optional if-then)
  "Return the text of the objectives buffer for BLOCK and its ROWS.
ROWS are (ID TEXT DONE ORIGIN) lists, and IF-THEN is a plan to show."
  (let* ((count (length rows))
         (met (seq-count (lambda (r) (eql (nth 2 r) 1)) rows))
         (head (concat "Objectives for " (writing-habit-objectives-block-text block) "\n"
                       (format "%s, %d minutes%s\n\n"
                               (writing-habit-objectives-block-day block)
                               (or (writing-habit-objectives-block-minutes block) 0)
                               (if (writing-habit-objectives-block-due block)
                                   (format ", deadline %s" (writing-habit-objectives-block-due block))
                                 ""))))
         (body (if rows
                   (mapconcat
                    (lambda (r)
                      (propertize (format "- [%s] %s%s" (if (eql (nth 2 r) 1) "X" " ") (nth 1 r)
                                          (if (member (nth 3 r) '("ai" "grasscatcher"))
                                              (format "   (%s)" (nth 3 r)) ""))
                                  'writing-habit-objective (list (nth 0 r) (nth 2 r))))
                    rows "\n")
                 "No objectives yet.  Press a to type one, or s to ask the coach."))
         (tally (if rows (format "\n\nMet %d of %d." met count) ""))
         (limit (if (> count writing-habit-objectives-limit)
                    (format "\n%d objectives.  Three or fewer keep the block winnable; park the rest on the grasscatcher."
                            count)
                  ""))
         (plan (if (and if-then (not (string-empty-p if-then)))
                   (format "\n\nIf-then plan: %s" if-then) "")))
    (concat head body tally limit plan "\n\n"
            (propertize "a add   x or RET check   s suggest with AI   g refresh   q quit\n"
                        'face 'shadow))))

(defun writing-habit-objectives-refresh ()
  "Draw the objectives buffer again from the database."
  (interactive)
  (let ((block writing-habit-objectives--block)
        (line (line-number-at-pos))
        (rows (writing-habit-objectives--with-db db writing-habit-objectives--db
                (writing-habit-objectives-rows
                 db (writing-habit-objectives-block-day writing-habit-objectives--block)
                 (writing-habit-objectives-block-start writing-habit-objectives--block)
                 (writing-habit-objectives-block-code writing-habit-objectives--block)))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (writing-habit-objectives-string block rows writing-habit-objectives--if-then))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun writing-habit-objectives--buffer (block db-path)
  "Return the objectives buffer for BLOCK, reading DB-PATH, drawn afresh."
  (with-current-buffer (get-buffer-create "*writing-habit objectives*")
    (unless (derived-mode-p 'writing-habit-objectives-mode)
      (writing-habit-objectives-mode))
    (unless (and writing-habit-objectives--block
                 (equal (writing-habit-objectives-block-text writing-habit-objectives--block)
                        (writing-habit-objectives-block-text block))
                 (equal (writing-habit-objectives-block-day writing-habit-objectives--block)
                        (writing-habit-objectives-block-day block)))
      (setq writing-habit-objectives--if-then nil))
    (setq writing-habit-objectives--block block
          writing-habit-objectives--db db-path)
    (writing-habit-objectives-refresh)
    (current-buffer)))

(defun writing-habit-objectives-toggle ()
  "Check off the objective at point, or check it back on."
  (interactive)
  (let ((spot (get-text-property (point) 'writing-habit-objective)))
    (unless spot (user-error "No objective on this line"))
    (writing-habit-objectives--with-db db writing-habit-objectives--db
      (writing-habit-objectives-mark db (nth 0 spot) (not (eql (nth 1 spot) 1))))
    (writing-habit-objectives-refresh)))

(defun writing-habit-objectives-add-typed (text)
  "Add TEXT, a typed objective, to the block of the objectives buffer."
  (interactive (list (read-string "Objective (a result you could show): ")))
  (let ((b writing-habit-objectives--block))
    (writing-habit-objectives--with-db db writing-habit-objectives--db
      (writing-habit-objectives-add db (writing-habit-objectives-block-day b)
                                    (writing-habit-objectives-block-start b)
                                    (writing-habit-objectives-block-code b) text "typed"))
    (writing-habit-objectives-refresh)))

(defun writing-habit-objectives-suggest-here ()
  "Ask the coach for objectives for the block of the objectives buffer."
  (interactive)
  (writing-habit-objectives-suggest writing-habit-objectives--block
                                    writing-habit-objectives--db))

(defvar writing-habit-objectives-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'writing-habit-objectives-add-typed)
    (define-key map (kbd "x") #'writing-habit-objectives-toggle)
    (define-key map (kbd "RET") #'writing-habit-objectives-toggle)
    (define-key map (kbd "s") #'writing-habit-objectives-suggest-here)
    (define-key map (kbd "g") #'writing-habit-objectives-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap of `writing-habit-objectives-mode'.")

(define-derived-mode writing-habit-objectives-mode special-mode "WH-Objectives"
  "The objectives of one time block.
\\{writing-habit-objectives-mode-map}")

;;;; Commands

;;;###autoload
(defun writing-habit-objectives-show (&optional block db-path)
  "List the objectives of BLOCK, or of the block at point.
DB-PATH is the habit database; without it the last one used is read.
The list shows each objective checked or not, how many were met, and
where an objective came from when the coach or the grasscatcher wrote it."
  (interactive)
  (let* ((block (or block (writing-habit-objectives-block-at-point)))
         (db-path (or db-path (writing-habit-objectives--db-path))))
    (pop-to-buffer (writing-habit-objectives--buffer block db-path))))

(defun writing-habit-objectives--stuck (db block)
  "Return the stuck sign of BLOCK's project in DB, or nil."
  (when (require 'writing-habit-helpers nil t)
    (ignore-errors
      (writing-habit-helpers-stuck-check
       db (writing-habit-objectives-block-code block)
       (cons (writing-habit-objectives-block-day block)
             (writing-habit-objectives-block-start block))))))

;;;###autoload
(defun writing-habit-objectives-suggest (&optional block db-path task)
  "Have the local model draft one to three objectives for BLOCK.
BLOCK defaults to the block at point and DB-PATH to the last habit
database used.  TASK is the vague task, such as work on the R01; it is
asked for when not given, with the stuck task of the project offered
first.  Each objective is offered in turn, and those accepted are saved
with the origin ai.  An accepted if-then plan is shown with the
objectives and copied to the kill ring for the block's review note.
Nothing is asked in an AI-off week."
  (interactive)
  (when writing-habit-coach-ai-off
    (user-error "This is an AI-off week, so the coach stays quiet"))
  (let* ((block (or block (writing-habit-objectives-block-at-point)))
         (db-path (or db-path (writing-habit-objectives--db-path))))
    (writing-habit-objectives--with-db db db-path
      (let* ((stuck (writing-habit-objectives--stuck db block))
             (task (or task
                       (read-string "Task for this block (such as work on the R01): "
                                    (and stuck (writing-habit-helpers-stuck-task stuck)))))
             (_ (when (string-empty-p (string-trim task))
                  (user-error "Type the task first, such as work on the R01")))
             (hint (and stuck
                        (string= (writing-habit-helpers-stuck-normalize task)
                                 (writing-habit-helpers-stuck-normalize
                                  (writing-habit-helpers-stuck-task stuck)))
                        (writing-habit-helpers-stuck-sentence stuck)))
             (facts (writing-habit-objectives-facts
                     task (or (writing-habit-objectives-block-name block)
                              (writing-habit-objectives-block-code block))
                     (or (writing-habit-objectives-block-minutes block) 0)
                     (writing-habit-objectives-block-due block)
                     (writing-habit-objectives-block-waiting block)
                     (writing-habit-objectives-last-next-step
                      db (writing-habit-objectives-block-code block)
                      (writing-habit-objectives-block-day block)
                      (writing-habit-objectives-block-start block))
                     hint))
             (source (format "%s at %s" writing-habit-coach-model writing-habit-coach-url))
             (parsed (condition-case err
                         (writing-habit-objectives-parse
                          (writing-habit-coach-ask "objectives" facts))
                       (error (setq source (concat "plain summary written without a model, because "
                                                   (error-message-string err)))
                              nil)))
             (items (car parsed))
             (if-then (cdr parsed))
             (lines (append items (when (and items if-then (not (string-empty-p if-then)))
                                    (list (concat "Note: " if-then)))))
             (log-id (when (writing-habit-objectives--table-p db "ai_log")
                       (writing-habit-coach-log
                        db "objectives" source facts
                        (if items (string-join lines "\n") writing-habit-objectives-plain-advice)
                        (writing-habit-objectives-block-week-start block))))
             (added 0)
             (kept-plan nil))
        (if (null items)
            (message "%s (%s)" writing-habit-objectives-plain-advice source)
          (dolist (index (funcall writing-habit-coach-chooser "Suggested objectives"
                                  (concat "Accept the objectives to keep. You can reword them later.\n"
                                          "Source: " source)
                                  lines))
            (if (< index (length items))
                (progn (writing-habit-objectives-add
                        db (writing-habit-objectives-block-day block)
                        (writing-habit-objectives-block-start block)
                        (writing-habit-objectives-block-code block)
                        (nth index items) "ai")
                       (setq added (1+ added)))
              (setq kept-plan if-then)))
          (writing-habit-coach-mark-accepted db log-id (> added 0))
          (when kept-plan (kill-new kept-plan))
          (message "Added %d objective%s%s" added (if (= added 1) "" "s")
                   (if kept-plan "; the if-then plan is on the kill ring" "")))
        (let ((buffer (writing-habit-objectives--buffer block db-path)))
          (when kept-plan
            (with-current-buffer buffer
              (setq writing-habit-objectives--if-then kept-plan)
              (writing-habit-objectives-refresh)))
          (pop-to-buffer buffer)
          added)))))

(provide 'writing-habit-objectives)
;;; writing-habit-objectives.el ends here
