;;; writing-habit-pomodoro.el --- A Pomodoro timer that counts each day's Pomodoros -*- lexical-binding: t; -*-

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

;; The twin of the Pomodoro timer of the Python window, and of its daily
;; count in pom_count.py.
;;
;; The timer splits a writing block into Pomodoros of focused work with
;; short breaks between them, and a long break at the end of the cycle:
;;
;;   Pomodoro 1, short break, Pomodoro 2, short break, ..., Pomodoro n, long break
;;
;; By default the cycle length n comes from the block: the largest number
;; of Pomodoros that fit in it with their short breaks, so a two-hour block
;; holds four, 90 minutes three, and an hour two.  The block is the one
;; under way in the weekly table of the current buffer, or the one at
;; point, or the one at point in the hour view.  Without a block the cycle
;; holds four.
;;
;; The timer shows in the mode line, such as [Pomodoro 2/4  18:07  Today: 5],
;; red during a Pomodoro and green during a break.  It can tick once a
;; second and sounds a buzzer when a phase ends.  Each finished Pomodoro
;; adds one to the day's count, which survives a restart of Emacs.  At
;; 11:59 PM the day's total is written to a table of a SQLite database, by
;; default zzPomCount in ~/6003TimeTracking/cb/mytime.db, one row per day.
;; A day left unwritten, because Emacs was closed at 11:59 PM, is written
;; the next time the timer runs.  The table is created when it is missing.
;; The writer never changes another table: it writes only to a table with
;; the columns date, pom_count, and recorded_at, or those three after an ID
;; integer primary key, refuses the time tracker's zTimeSpent table by name,
;; and never alters, renames, or drops a table or a column.
;;
;; Commands:
;;   `writing-habit-pomodoro-start'         start, pause, or resume
;;   `writing-habit-pomodoro-stop'          stop the cycle
;;   `writing-habit-pomodoro-record-today'  write today's count now
;;   `writing-habit-pomodoro-mode'          the mode line display and the
;;                                          nightly record, with the hotkeys
;;                                          C-s-p and C-s-s (Control Command
;;                                          P and S on a Mac)
;;
;; The counting lives in a clock that takes the time as an argument, so the
;; tests drive it without waiting.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function writing-habit-table-current "writing-habit-table-mode")
(declare-function writing-habit-table-blocks "writing-habit-table")
(declare-function writing-habit-table-block-offset "writing-habit-table")
(declare-function writing-habit-table-block-start "writing-habit-table")
(declare-function writing-habit-table-block-end "writing-habit-table")
(declare-function writing-habit-table-block-row "writing-habit-table")
(declare-function writing-habit-table--row-at-point "writing-habit-table-mode" (model))

(defgroup writing-habit-pomodoro nil
  "A Pomodoro timer that counts each day's Pomodoros."
  :group 'writing-habit
  :prefix "writing-habit-pomodoro-")

;;;; Settings

(defcustom writing-habit-pomodoro-minutes 25
  "Minutes of focused work in a Pomodoro."
  :type 'integer)

(defcustom writing-habit-pomodoro-short-break 5
  "Minutes of a short break."
  :type 'integer)

(defcustom writing-habit-pomodoro-long-break 15
  "Minutes of the long break at the end of a cycle."
  :type 'integer)

(defcustom writing-habit-pomodoro-per-cycle 4
  "Pomodoros per cycle when `writing-habit-pomodoro-auto-cycle' is nil.
It is also the cycle length when no block decides."
  :type 'integer)

(defcustom writing-habit-pomodoro-auto-cycle t
  "Non-nil takes the cycle length from the time block."
  :type 'boolean)

(defcustom writing-habit-pomodoro-auto-next t
  "Non-nil starts the next phase without a key press."
  :type 'boolean)

(defcustom writing-habit-pomodoro-tick t
  "Non-nil ticks once a second during a Pomodoro."
  :type 'boolean)

(defcustom writing-habit-pomodoro-tick-in-breaks nil
  "Non-nil ticks during breaks too."
  :type 'boolean)

(defcustom writing-habit-pomodoro-tick-volume 30
  "The volume of the tick, in percent."
  :type 'integer)

(defcustom writing-habit-pomodoro-buzzer t
  "Non-nil sounds a buzzer when a Pomodoro or a break ends."
  :type 'boolean)

(defcustom writing-habit-pomodoro-buzzer-volume 80
  "The volume of the buzzer, in percent."
  :type 'integer)

(defcustom writing-habit-pomodoro-tick-file ""
  "A sound file for the tick, or empty for the built-in tick."
  :type 'file)

(defcustom writing-habit-pomodoro-buzzer-file ""
  "A sound file for the alarm, or empty for the built-in buzzer."
  :type 'file)

(defcustom writing-habit-pomodoro-record-db t
  "Non-nil writes each day's total to a database table at 11:59 PM."
  :type 'boolean)

(defcustom writing-habit-pomodoro-db-dir "~/6003TimeTracking/cb"
  "The folder of the database that holds the daily counts."
  :type 'directory)

(defcustom writing-habit-pomodoro-db-file "mytime.db"
  "The database file that holds the daily counts."
  :type 'string)

(defcustom writing-habit-pomodoro-db-table "zzPomCount"
  "The table that gets one row per day."
  :type 'string)

(defcustom writing-habit-pomodoro-state-file
  (locate-user-emacs-file "writing-habit-pomodoro.eld")
  "The file that keeps today's count and the last day written to the database."
  :type 'file)

(defvar writing-habit-pomodoro-block-function #'writing-habit-pomodoro-current-block
  "Function that returns the block deciding the cycle, as (START . END), or nil.
START and END are HH:MM.  Tests replace it.")

(defvar writing-habit-pomodoro-now-function #'float-time
  "Function that returns the time in seconds.  Tests replace it.")

(defvar writing-habit-pomodoro-today-function (lambda () (format-time-string "%Y-%m-%d"))
  "Function that returns today's date as YYYY-MM-DD.  Tests replace it.")

(defvar writing-habit-pomodoro-wall-function (lambda () (format-time-string "%Y-%m-%dT%H:%M:%S"))
  "Function that returns the time of a record, as YYYY-MM-DDTHH:MM:SS.
Tests replace it.")

(defvar writing-habit-pomodoro-play-function #'writing-habit-pomodoro--play
  "Function that plays a sound by name, tick or buzzer, and volume.
Tests replace it.")

(defvar writing-habit-pomodoro-log-function (lambda (text) (message "%s" text))
  "Function that reports a line, such as when a phase ends.  Tests replace it.")

;;;; The cycle

(defun writing-habit-pomodoro-fit-in-block (block-minutes pomodoro short-break)
  "Return how many Pomodoros of POMODORO minutes fit in BLOCK-MINUTES.
The Pomodoros are separated by SHORT-BREAK minutes, and the long break is
not counted, because it falls in the break after the block.  A block too
short for one Pomodoro still gets one.  Nil gives the default of four."
  (if (or (null block-minutes) (<= block-minutes 0))
      4
    (max 1 (/ (+ block-minutes (max 0 short-break))
              (+ (max 1 pomodoro) (max 0 short-break))))))

(defun writing-habit-pomodoro--minute (clock)
  "Return the minute of the day of HH:MM CLOCK, or nil."
  (when (and (stringp clock) (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)" clock))
    (+ (* 60 (string-to-number (match-string 1 clock))) (string-to-number (match-string 2 clock)))))

(defun writing-habit-pomodoro-block-minutes (block)
  "Return the length in minutes of BLOCK, (START . END), or nil."
  (let ((first (and block (writing-habit-pomodoro--minute (car block))))
        (last (and block (writing-habit-pomodoro--minute (cdr block)))))
    (when (and first last)
      (when (<= last first) (setq last (+ last (* 24 60))))
      (- last first))))

(defun writing-habit-pomodoro-cycle-length (&optional block)
  "Return the Pomodoros per cycle the settings give for BLOCK."
  (if writing-habit-pomodoro-auto-cycle
      (writing-habit-pomodoro-fit-in-block (writing-habit-pomodoro-block-minutes block)
                                           writing-habit-pomodoro-minutes
                                           writing-habit-pomodoro-short-break)
    (max 1 writing-habit-pomodoro-per-cycle)))

(cl-defstruct (writing-habit-pomodoro-phase
               (:constructor writing-habit-pomodoro-phase-create)
               (:copier nil))
  "One phase of a cycle: KIND is pomodoro, short, or long."
  kind number minutes)

(defun writing-habit-pomodoro-phase-label (phase of)
  "Return the label of PHASE in a cycle of OF Pomodoros."
  (pcase (writing-habit-pomodoro-phase-kind phase)
    ('pomodoro (format "Pomodoro %d/%d" (writing-habit-pomodoro-phase-number phase) of))
    ('short "Short break")
    (_ "Long break")))

(defun writing-habit-pomodoro-build-cycle (count)
  "Return the phases of one cycle of COUNT Pomodoros."
  (let ((phases '()))
    (dotimes (i count)
      (push (writing-habit-pomodoro-phase-create :kind 'pomodoro :number (1+ i)
                                                 :minutes writing-habit-pomodoro-minutes)
            phases)
      (when (< (1+ i) count)
        (push (writing-habit-pomodoro-phase-create :kind 'short :number (1+ i)
                                                   :minutes writing-habit-pomodoro-short-break)
              phases)))
    (push (writing-habit-pomodoro-phase-create :kind 'long :number count
                                               :minutes writing-habit-pomodoro-long-break)
          phases)
    (nreverse phases)))

;;;; The clock

(cl-defstruct (writing-habit-pomodoro-clock
               (:constructor writing-habit-pomodoro-clock-create)
               (:copier nil))
  "The timer's state.  STATE is idle, running, paused, waiting, or done.
Every function of the clock takes the time in seconds."
  (state 'idle) (phases nil) (index 0) (ends-at 0.0) (paused-left 0.0)
  (count 0) (completed 0))

(defun writing-habit-pomodoro-clock-phase (clock)
  "Return the phase under way in CLOCK, or nil."
  (nth (writing-habit-pomodoro-clock-index clock) (writing-habit-pomodoro-clock-phases clock)))

(defun writing-habit-pomodoro-clock--begin (clock now)
  "Begin the phase of CLOCK that is due at NOW."
  (setf (writing-habit-pomodoro-clock-ends-at clock)
        (+ now (* 60.0 (writing-habit-pomodoro-phase-minutes
                        (writing-habit-pomodoro-clock-phase clock))))
        (writing-habit-pomodoro-clock-state clock) 'running))

(defun writing-habit-pomodoro-clock-start (clock now &optional block)
  "Start a cycle of CLOCK at NOW, resume a pause, or begin a waiting phase.
BLOCK, (START . END), decides the cycle length of a new cycle."
  (pcase (writing-habit-pomodoro-clock-state clock)
    ('paused
     (setf (writing-habit-pomodoro-clock-ends-at clock)
           (+ now (writing-habit-pomodoro-clock-paused-left clock))
           (writing-habit-pomodoro-clock-state clock) 'running))
    ('waiting (writing-habit-pomodoro-clock--begin clock now))
    ((or 'idle 'done)
     (let ((count (writing-habit-pomodoro-cycle-length block)))
       (setf (writing-habit-pomodoro-clock-count clock) count
             (writing-habit-pomodoro-clock-phases clock) (writing-habit-pomodoro-build-cycle count)
             (writing-habit-pomodoro-clock-index clock) 0
             (writing-habit-pomodoro-clock-completed clock) 0)
       (writing-habit-pomodoro-clock--begin clock now)))))

(defun writing-habit-pomodoro-clock-pause (clock now)
  "Pause CLOCK at NOW."
  (when (eq (writing-habit-pomodoro-clock-state clock) 'running)
    (setf (writing-habit-pomodoro-clock-paused-left clock)
          (max 0.0 (- (writing-habit-pomodoro-clock-ends-at clock) now))
          (writing-habit-pomodoro-clock-state clock) 'paused)))

(defun writing-habit-pomodoro-clock-stop (clock)
  "Stop CLOCK."
  (setf (writing-habit-pomodoro-clock-state clock) 'idle
        (writing-habit-pomodoro-clock-phases clock) nil
        (writing-habit-pomodoro-clock-index clock) 0))

(defun writing-habit-pomodoro-clock-remaining (clock now)
  "Return the seconds left at NOW in the phase of CLOCK."
  (pcase (writing-habit-pomodoro-clock-state clock)
    ('running (max 0.0 (- (writing-habit-pomodoro-clock-ends-at clock) now)))
    ('paused (writing-habit-pomodoro-clock-paused-left clock))
    ('waiting (let ((phase (writing-habit-pomodoro-clock-phase clock)))
                (if phase (* 60.0 (writing-habit-pomodoro-phase-minutes phase)) 0.0)))
    (_ 0.0)))

(defun writing-habit-pomodoro-clock-poll (clock now)
  "Advance CLOCK when its phase has ended by NOW.  Return the phase that ended."
  (when (and (eq (writing-habit-pomodoro-clock-state clock) 'running)
             (>= now (writing-habit-pomodoro-clock-ends-at clock)))
    (let ((ended (writing-habit-pomodoro-clock-phase clock)))
      (when (and ended (eq (writing-habit-pomodoro-phase-kind ended) 'pomodoro))
        (cl-incf (writing-habit-pomodoro-clock-completed clock)))
      (cl-incf (writing-habit-pomodoro-clock-index clock))
      (cond ((>= (writing-habit-pomodoro-clock-index clock)
                 (length (writing-habit-pomodoro-clock-phases clock)))
             (setf (writing-habit-pomodoro-clock-state clock) 'done))
            (writing-habit-pomodoro-auto-next
             ;; Count the next phase from when the last one should have
             ;; ended, so a late read of the clock does not stretch the cycle.
             (writing-habit-pomodoro-clock--begin clock (writing-habit-pomodoro-clock-ends-at clock)))
            (t (setf (writing-habit-pomodoro-clock-state clock) 'waiting)))
      ended)))

(defun writing-habit-pomodoro-clock-text (clock now)
  "Return the text of CLOCK at NOW, such as Pomodoro 2/4  18:07."
  (let ((state (writing-habit-pomodoro-clock-state clock))
        (phase (writing-habit-pomodoro-clock-phase clock)))
    (pcase state
      ('idle "Pomodoro")
      ('done (format "Cycle done, %d Pomodoros" (writing-habit-pomodoro-clock-completed clock)))
      (_
       (let* ((left (ceiling (writing-habit-pomodoro-clock-remaining clock now)))
              (clock-text (format "%02d:%02d" (/ left 60) (% left 60)))
              (label (writing-habit-pomodoro-phase-label phase (writing-habit-pomodoro-clock-count clock))))
         (pcase state
           ('waiting (format "%s ready. Press Start" label))
           ('paused (format "%s  %s paused" label clock-text))
           (_ (format "%s  %s" label clock-text))))))))

;;;; Today's count, kept across restarts

(defun writing-habit-pomodoro--read-state ()
  "Return the stored state plist, or nil when it is missing or unreadable."
  (when (file-readable-p writing-habit-pomodoro-state-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents writing-habit-pomodoro-state-file)
          (let ((value (read (current-buffer))))
            (and (listp value) value)))
      (error nil))))

(defun writing-habit-pomodoro--write-state (state)
  "Store the plist STATE."
  (with-temp-file writing-habit-pomodoro-state-file
    (let ((print-length nil) (print-level nil))
      (prin1 state (current-buffer)))))

(defun writing-habit-pomodoro-today-count (day)
  "Return how many Pomodoros were finished on DAY, an ISO date.
A stored count from another day counts as zero, which is how the count
starts over each morning."
  (let* ((state (writing-habit-pomodoro--read-state))
         (count (plist-get state :count)))
    (if (and (equal (plist-get state :date) day) (integerp count) (> count 0)) count 0)))

(defun writing-habit-pomodoro-stored-day ()
  "Return (DAY . COUNT) of the stored tally, or nil."
  (let ((day (plist-get (writing-habit-pomodoro--read-state) :date)))
    (when (and (stringp day) (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" day))
      (cons day (writing-habit-pomodoro-today-count day)))))

(defun writing-habit-pomodoro-last-recorded ()
  "Return (DAY . COUNT) last written to the database, or (\"\" . -1)."
  (let ((state (writing-habit-pomodoro--read-state)))
    (cons (or (plist-get state :recorded-date) "") (or (plist-get state :recorded-count) -1))))

(defun writing-habit-pomodoro-add-to-today (day)
  "Count one more finished Pomodoro on DAY and return the new total."
  (let ((count (1+ (writing-habit-pomodoro-today-count day)))
        (state (writing-habit-pomodoro--read-state)))
    (writing-habit-pomodoro--write-state (plist-put (plist-put state :date day) :count count))
    count))

(defun writing-habit-pomodoro--note-recorded (day count)
  "Remember that COUNT was written for DAY."
  (let ((state (writing-habit-pomodoro--read-state)))
    (writing-habit-pomodoro--write-state
     (plist-put (plist-put state :recorded-date day) :recorded-count count))))

;;;; The daily record in a database

(defconst writing-habit-pomodoro--shapes
  '(("date" "pom_count" "recorded_at") ("id" "date" "pom_count" "recorded_at"))
  "The column lists of a Pomodoro count table, the only shapes written to.")

(defconst writing-habit-pomodoro--protected '("ztimespent")
  "Tables of the time tracker that a Pomodoro count is never written to.")

(defun writing-habit-pomodoro-database-path (&optional folder file)
  "Return the database path from FOLDER and FILE, with ~ expanded."
  (expand-file-name (if (or (null file) (string-empty-p file)) "mytime.db" file)
                    (expand-file-name (if (or (null folder) (string-empty-p folder))
                                          "~/6003TimeTracking/cb"
                                        folder))))

(defun writing-habit-pomodoro-check-table (table)
  "Return TABLE when it is a plain SQL name of a table of our own.
Signal an error for any other name, and for the time tracker's tables."
  (unless (and (stringp table) (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" table))
    (error "Not a plain table name: %S" table))
  (when (member (downcase table) writing-habit-pomodoro--protected)
    ;; The text goes through %s, so `error' does not curl its apostrophe.
    (error "%s" (format "%s is the time tracker's own table. Choose a table of its own for the Pomodoro counts, such as zzPomCount"
                        table)))
  table)

(defun writing-habit-pomodoro--check-shape (db table)
  "Signal an error when TABLE exists in DB with columns other than ours."
  (let ((columns (mapcar (lambda (r) (nth 1 r))
                         (sqlite-select db (format "PRAGMA table_xinfo(\"%s\")" table)))))
    (when (and columns
               (not (member (mapcar #'downcase columns) writing-habit-pomodoro--shapes)))
      (error "%s already exists with the columns %s, so it is not a Pomodoro count table. Choose another name, such as zzPomCount"
             table (string-join columns ", ")))))

(defun writing-habit-pomodoro-record (path table day count &optional stamp)
  "Write COUNT Pomodoros for DAY to TABLE of the database at PATH.
STAMP is the time of the record.  Return the symbol `added' or `updated'.
The folder must exist, so a mistyped path fails instead of making a new
database somewhere unexpected.  The table is created when it is missing."
  (setq table (writing-habit-pomodoro-check-table table))
  (let ((path (expand-file-name path)))
    (unless (file-directory-p (file-name-directory path))
      (error "No folder %s" (file-name-directory path)))
    (let ((db (sqlite-open path))
          (stamp (or stamp (funcall writing-habit-pomodoro-wall-function)))
          found)
      (unwind-protect
          (progn
            (writing-habit-pomodoro--check-shape db table)
            (sqlite-execute db (format "CREATE TABLE IF NOT EXISTS \"%s\" (date TEXT PRIMARY KEY, pom_count INTEGER NOT NULL, recorded_at TEXT NOT NULL)" table))
            (setq found (sqlite-select db (format "SELECT 1 FROM \"%s\" WHERE date = ?" table) (list day)))
            (if found
                (sqlite-execute db (format "UPDATE \"%s\" SET pom_count = ?, recorded_at = ? WHERE date = ?" table)
                                (list count stamp day))
              (sqlite-execute db (format "INSERT INTO \"%s\" (date, pom_count, recorded_at) VALUES (?, ?, ?)" table)
                              (list day count stamp))))
        (sqlite-close db))
      (if found 'updated 'added))))

(defun writing-habit-pomodoro-rows (path table)
  "Return (DATE . COUNT) for every day in TABLE of the database at PATH.
The days come oldest first."
  (setq table (writing-habit-pomodoro-check-table table))
  (let ((db (sqlite-open (expand-file-name path))))
    (unwind-protect
        (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r)))
                (sqlite-select db (format "SELECT date, pom_count FROM \"%s\" ORDER BY date" table)))
      (sqlite-close db))))

(defun writing-habit-pomodoro-record-day (day count)
  "Write COUNT for DAY to the database table.  Return what happened, or nil."
  (when writing-habit-pomodoro-record-db
    (let ((path (writing-habit-pomodoro-database-path writing-habit-pomodoro-db-dir
                                                      writing-habit-pomodoro-db-file)))
      (condition-case err
          (let ((done (writing-habit-pomodoro-record path writing-habit-pomodoro-db-table day count)))
            (writing-habit-pomodoro--note-recorded day count)
            (funcall writing-habit-pomodoro-log-function
                     (format "Recorded %d Pomodoros for %s in %s of %s"
                             count day writing-habit-pomodoro-db-table path))
            done)
        (error
         (funcall writing-habit-pomodoro-log-function
                  (format "Could not record the Pomodoro count for %s: %s" day (error-message-string err)))
         nil)))))

(defun writing-habit-pomodoro-catch-up ()
  "Write the stored tally of an earlier day when the database lacks it."
  (let ((stored (writing-habit-pomodoro-stored-day)))
    (when (and stored
               (string< (car stored) (funcall writing-habit-pomodoro-today-function))
               (not (equal (writing-habit-pomodoro-last-recorded) stored)))
      (writing-habit-pomodoro-record-day (car stored) (cdr stored)))))

(defun writing-habit-pomodoro-next-record-time (now)
  "Return the seconds from NOW, a decoded time, to the next 11:59 PM."
  (let* ((second (decoded-time-second now)) (minute (decoded-time-minute now))
         (hour (decoded-time-hour now))
         (into (+ (* 3600 hour) (* 60 minute) second))
         (target (+ (* 3600 23) (* 60 59))))
    (if (< into target) (- target into) (- (+ target 86400) into))))

;;;; Sounds

(defun writing-habit-pomodoro--wave (path samples)
  "Write SAMPLES, numbers from -1 to 1, to PATH as a 16-bit mono WAV file."
  (let* ((rate 22050) (n (length samples)) (data (* 2 n)))
    (cl-flet ((u32 (v) (unibyte-string (logand v 255) (logand (ash v -8) 255)
                                       (logand (ash v -16) 255) (logand (ash v -24) 255)))
              (u16 (v) (unibyte-string (logand v 255) (logand (ash v -8) 255))))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert "RIFF" (u32 (+ 36 data)) "WAVEfmt " (u32 16) (u16 1) (u16 1)
                (u32 rate) (u32 (* 2 rate)) (u16 2) (u16 16) "data" (u32 data))
        (dolist (s samples)
          (insert (u16 (logand (truncate (* 32000 (max -1.0 (min 1.0 s)))) 65535))))
        (let ((coding-system-for-write 'no-conversion))
          (write-region nil nil path nil 'silent))))))

(defun writing-habit-pomodoro--tick-samples ()
  "Return a short wooden click, like a kitchen timer."
  (let ((rate 22050.0) (out '()))
    (dotimes (i (truncate (* rate 0.035)))
      (push (* (exp (/ (- i) (* rate 0.004)))
               (+ (* 0.6 (sin (/ (* 2 float-pi 2600 i) rate)))
                  (* 0.4 (- (/ (random 2000) 1000.0) 1.0))))
            out))
    (nreverse out)))

(defun writing-habit-pomodoro--buzzer-samples ()
  "Return three rasping buzzes, a little under a second and a half in all."
  (let ((rate 22050.0) (out '()))
    (dotimes (_ 3)
      (let ((n (truncate (* rate 0.32))))
        (dotimes (i n)
          (let* ((tm (/ i rate))
                 (square (if (>= (sin (* 2 float-pi 220 tm)) 0) 1.0 -1.0))
                 (fifth (if (>= (sin (* 2 float-pi 330 tm)) 0) 1.0 -1.0))
                 (fade (min 1.0 (/ i 200.0) (/ (- n i) 200.0))))
            (push (* 0.35 (+ square (* 0.6 fifth)) fade) out))))
      (dotimes (_ (truncate (* rate 0.12))) (push 0.0 out)))
    (nreverse out)))

(defun writing-habit-pomodoro-sound-files (&optional folder)
  "Write the two built-in sounds once in FOLDER and return their paths.
The result is ((tick . PATH) (buzzer . PATH)).  FOLDER defaults to
writing-habit-sounds in the variable `temporary-file-directory'."
  (let* ((folder (or folder (expand-file-name "writing-habit-sounds" temporary-file-directory)))
         (paths (list (cons 'tick (expand-file-name "tick.wav" folder))
                      (cons 'buzzer (expand-file-name "buzzer.wav" folder)))))
    (make-directory folder t)
    (dolist (p paths)
      (unless (and (file-exists-p (cdr p))
                   (> (file-attribute-size (file-attributes (cdr p))) 100))
        (writing-habit-pomodoro--wave (cdr p) (if (eq (car p) 'tick)
                                                  (writing-habit-pomodoro--tick-samples)
                                                (writing-habit-pomodoro--buzzer-samples)))))
    paths))

(defun writing-habit-pomodoro-choose-sound (name)
  "Return (FILE . NOTE) for the sound NAME, tick or buzzer.
A custom file that is missing falls back to the built-in sound, and NOTE
says so, so a moved file never silences the timer."
  (let ((custom (if (eq name 'tick) writing-habit-pomodoro-tick-file writing-habit-pomodoro-buzzer-file))
        (built-in (cdr (assq name (writing-habit-pomodoro-sound-files)))))
    (cond ((or (null custom) (string-empty-p custom)) (cons built-in nil))
          ((file-regular-p (expand-file-name custom)) (cons (expand-file-name custom) nil))
          (t (cons built-in (format "%s not found, so the built-in %s plays" custom name))))))

(defvar writing-habit-pomodoro--noted nil
  "The last note about each sound, so it is reported once.")

(defun writing-habit-pomodoro--play (name volume)
  "Play the sound NAME at VOLUME percent by the best means this computer has.
On a Mac, afplay plays it without stopping Emacs.  Elsewhere a WAV file
plays through `play-sound-file' when Emacs can, and otherwise the buzzer
becomes the bell and the tick is silent."
  (when (> volume 0)
    (let* ((choice (writing-habit-pomodoro-choose-sound name))
           (note (cdr choice))
           (afplay (and (eq system-type 'darwin) (executable-find "afplay"))))
      (when (and note (not (equal note (alist-get name writing-habit-pomodoro--noted))))
        (funcall writing-habit-pomodoro-log-function note))
      (setf (alist-get name writing-habit-pomodoro--noted) note)
      (cond
       (afplay (start-process "writing-habit-pomodoro-sound" nil afplay
                              "-v" (format "%.2f" (/ (min volume 100) 100.0)) (car choice)))
       ((and (fboundp 'play-sound-internal) (string-suffix-p ".wav" (downcase (car choice))))
        (ignore-errors (play-sound-file (car choice) (/ (min volume 100) 100.0))))
       ((eq name 'buzzer) (ding t))))))

;;;; The block that decides the cycle

(defun writing-habit-pomodoro-current-block ()
  "Return (START . END) of the block that decides the cycle, or nil.
It is the block at point in the hour view, the block under way in the
weekly table of the current buffer, or the first block of the row at
point there."
  (or (let ((b (get-text-property (point) 'writing-habit-hours-block)))
        (when b (cons (writing-habit-table-block-start b) (writing-habit-table-block-end b))))
      (when (and (boundp 'writing-habit-table-mode) writing-habit-table-mode)
        (ignore-errors
          (let* ((model (writing-habit-table-current))
                 (blocks (writing-habit-table-blocks model))
                 (offset (mod (1- (string-to-number (format-time-string "%u"))) 7))
                 (minute (writing-habit-pomodoro--minute (format-time-string "%H:%M")))
                 (live (seq-find (lambda (b)
                                   (and (= (writing-habit-table-block-offset b) offset)
                                        (<= (writing-habit-pomodoro--minute (writing-habit-table-block-start b)) minute)
                                        (< minute (writing-habit-pomodoro--minute (writing-habit-table-block-end b)))))
                                 blocks))
                 (row (writing-habit-table--row-at-point model))
                 (here (seq-find (lambda (b) (eql (writing-habit-table-block-row b) row)) blocks))
                 (b (or live here)))
            (when b (cons (writing-habit-table-block-start b) (writing-habit-table-block-end b))))))))

;;;; The timer

(defvar writing-habit-pomodoro-mode)

(defvar writing-habit-pomodoro-clock (writing-habit-pomodoro-clock-create)
  "The clock of the running timer.")

(defvar writing-habit-pomodoro--timer nil
  "The timer that reads the clock once a second.")

(defvar writing-habit-pomodoro--record-timer nil
  "The timer that writes the day's total at 11:59 PM.")

(defvar writing-habit-pomodoro--record-target nil
  "The day the next 11:59 PM record belongs to.")

(defface writing-habit-pomodoro-work '((t :background "#fde4e1" :foreground "black"))
  "Face of the mode line during a Pomodoro.")
(defface writing-habit-pomodoro-break '((t :background "#e2f3e4" :foreground "black"))
  "Face of the mode line during a break.")
(defface writing-habit-pomodoro-wait '((t :background "#fff0d9" :foreground "black"))
  "Face of the mode line while a phase waits to start.")

(defun writing-habit-pomodoro-mode-line ()
  "Return the timer's text for the mode line."
  (let* ((clock writing-habit-pomodoro-clock)
         (now (funcall writing-habit-pomodoro-now-function))
         (state (writing-habit-pomodoro-clock-state clock))
         (phase (writing-habit-pomodoro-clock-phase clock))
         (text (format "[%s  Today: %d]" (writing-habit-pomodoro-clock-text clock now)
                       (writing-habit-pomodoro-today-count (funcall writing-habit-pomodoro-today-function)))))
    (propertize text 'face
                (cond ((eq state 'waiting) 'writing-habit-pomodoro-wait)
                      ((and (memq state '(running paused)) phase)
                       (if (eq (writing-habit-pomodoro-phase-kind phase) 'pomodoro)
                           'writing-habit-pomodoro-work
                         'writing-habit-pomodoro-break))))))

(defun writing-habit-pomodoro--phase-ended (ended)
  "Count, sound, and report the phase ENDED."
  (let ((clock writing-habit-pomodoro-clock))
    (when (eq (writing-habit-pomodoro-phase-kind ended) 'pomodoro)
      (writing-habit-pomodoro-catch-up)   ; before a new day's tally replaces the old one
      (writing-habit-pomodoro-add-to-today (funcall writing-habit-pomodoro-today-function)))
    (when writing-habit-pomodoro-buzzer
      (funcall writing-habit-pomodoro-play-function 'buzzer writing-habit-pomodoro-buzzer-volume))
    (let ((following (writing-habit-pomodoro-clock-phase clock))
          (count (writing-habit-pomodoro-clock-count clock)))
      (funcall
       writing-habit-pomodoro-log-function
       (cond
        ((eq (writing-habit-pomodoro-clock-state clock) 'done)
         (let ((line (format "Pomodoro cycle done: %d Pomodoros"
                             (writing-habit-pomodoro-clock-completed clock)))
               (block (funcall writing-habit-pomodoro-block-function)))
           ;; The long break ends at the next block.  With automatic starts
           ;; and a block under way, the next cycle begins by itself.
           (if (and writing-habit-pomodoro-auto-next block)
               (progn (writing-habit-pomodoro-clock-start
                       clock (funcall writing-habit-pomodoro-now-function) block)
                      (format "%s. Next cycle of %d started" line
                              (writing-habit-pomodoro-clock-count clock)))
             line)))
        (following
         (format "%s ended. %s %s" (writing-habit-pomodoro-phase-label ended count)
                 (writing-habit-pomodoro-phase-label following count)
                 (if (eq (writing-habit-pomodoro-clock-state clock) 'running) "started" "is ready")))
        (t (format "%s ended" (writing-habit-pomodoro-phase-label ended count))))))))

(defun writing-habit-pomodoro-update ()
  "Read the clock: tick, buzz at a phase's end, and redraw the mode line.
Return the text the timer shows."
  (let* ((clock writing-habit-pomodoro-clock)
         (now (funcall writing-habit-pomodoro-now-function))
         (ended (writing-habit-pomodoro-clock-poll clock now))
         (phase nil))
    (when ended (writing-habit-pomodoro--phase-ended ended))
    (setq phase (writing-habit-pomodoro-clock-phase clock))
    (when (and (eq (writing-habit-pomodoro-clock-state clock) 'running) phase
               writing-habit-pomodoro-tick (null ended)
               (or (eq (writing-habit-pomodoro-phase-kind phase) 'pomodoro)
                   writing-habit-pomodoro-tick-in-breaks))
      (funcall writing-habit-pomodoro-play-function 'tick writing-habit-pomodoro-tick-volume))
    (unless (memq (writing-habit-pomodoro-clock-state clock) '(running paused waiting))
      (writing-habit-pomodoro--cancel-tick))
    (force-mode-line-update t)
    (writing-habit-pomodoro-clock-text clock now)))

(defun writing-habit-pomodoro--cancel-tick ()
  "Stop reading the clock."
  (when (timerp writing-habit-pomodoro--timer)
    (cancel-timer writing-habit-pomodoro--timer))
  (setq writing-habit-pomodoro--timer nil))

(defun writing-habit-pomodoro--ensure-tick ()
  "Read the clock once a second."
  (unless (timerp writing-habit-pomodoro--timer)
    (setq writing-habit-pomodoro--timer (run-at-time 1 1 #'writing-habit-pomodoro-update))))

;;;###autoload
(defun writing-habit-pomodoro-start ()
  "Start a Pomodoro cycle, pause the one running, or resume a pause.
The cycle length comes from the block under way or at point."
  (interactive)
  (unless writing-habit-pomodoro-mode (writing-habit-pomodoro-mode 1))
  (let* ((clock writing-habit-pomodoro-clock)
         (state (writing-habit-pomodoro-clock-state clock))
         (now (funcall writing-habit-pomodoro-now-function)))
    (if (eq state 'running)
        (writing-habit-pomodoro-clock-pause clock now)
      (writing-habit-pomodoro-clock-start clock now (funcall writing-habit-pomodoro-block-function))
      (when (memq state '(idle done))
        (funcall writing-habit-pomodoro-log-function
                 (format "Pomodoro cycle of %d started" (writing-habit-pomodoro-clock-count clock))))
      (writing-habit-pomodoro--ensure-tick))
    (writing-habit-pomodoro-update)))

;;;###autoload
(defun writing-habit-pomodoro-stop ()
  "Stop the Pomodoro cycle."
  (interactive)
  (let ((clock writing-habit-pomodoro-clock))
    (unless (eq (writing-habit-pomodoro-clock-state clock) 'idle)
      (funcall writing-habit-pomodoro-log-function
               (format "Pomodoro stopped after %d Pomodoros" (writing-habit-pomodoro-clock-completed clock))))
    (writing-habit-pomodoro-clock-stop clock)
    (writing-habit-pomodoro--cancel-tick)
    (writing-habit-pomodoro-update)))

;;;###autoload
(defun writing-habit-pomodoro-record-today ()
  "Write today's count to the database table now, to test the settings."
  (interactive)
  (let ((day (funcall writing-habit-pomodoro-today-function)))
    (writing-habit-pomodoro-record-day day (writing-habit-pomodoro-today-count day))))

(defun writing-habit-pomodoro-record-tonight ()
  "Write the day's total at 11:59 PM, or catch up after a late timer."
  (let ((target writing-habit-pomodoro--record-target))
    (if (and target (equal (funcall writing-habit-pomodoro-today-function) target))
        (writing-habit-pomodoro-record-day target (writing-habit-pomodoro-today-count target))
      ;; The timer fired late, after a sleep, so the day may already have
      ;; changed.  The stored tally still says which day it belongs to.
      (writing-habit-pomodoro-catch-up)))
  (writing-habit-pomodoro-schedule-record))

(defun writing-habit-pomodoro-schedule-record ()
  "Set the timer for the next 11:59 PM."
  (when (timerp writing-habit-pomodoro--record-timer)
    (cancel-timer writing-habit-pomodoro--record-timer))
  (let ((seconds (max 1 (writing-habit-pomodoro-next-record-time (decode-time)))))
    (setq writing-habit-pomodoro--record-target
          (format-time-string "%Y-%m-%d" (time-add nil seconds))
          writing-habit-pomodoro--record-timer
          (run-at-time seconds nil #'writing-habit-pomodoro-record-tonight))))

(defvar writing-habit-pomodoro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s-p") #'writing-habit-pomodoro-start)
    (define-key map (kbd "C-s-s") #'writing-habit-pomodoro-stop)
    map)
  "Keymap of `writing-habit-pomodoro-mode': Control Command P and S on a Mac.")

(defconst writing-habit-pomodoro--mode-line-entry
  '(writing-habit-pomodoro-mode (" " (:eval (writing-habit-pomodoro-mode-line))))
  "The entry of the timer in `global-mode-string'.")

;;;###autoload
(define-minor-mode writing-habit-pomodoro-mode
  "Show the Pomodoro timer in the mode line and record each day's count.
The day's total is written at 11:59 PM, and a day left unwritten is
written when the mode starts.  \\[writing-habit-pomodoro-start] starts or
pauses the timer and \\[writing-habit-pomodoro-stop] stops it.
\\{writing-habit-pomodoro-mode-map}"
  :global t
  :lighter nil
  (if writing-habit-pomodoro-mode
      (progn
        (unless (member writing-habit-pomodoro--mode-line-entry global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '("")) (list writing-habit-pomodoro--mode-line-entry))))
        (writing-habit-pomodoro-schedule-record)
        (run-at-time 0 nil #'writing-habit-pomodoro-catch-up))
    (setq global-mode-string (delete writing-habit-pomodoro--mode-line-entry global-mode-string))
    (when (timerp writing-habit-pomodoro--record-timer)
      (cancel-timer writing-habit-pomodoro--record-timer))
    (writing-habit-pomodoro--cancel-tick)))

(provide 'writing-habit-pomodoro)
;;; writing-habit-pomodoro.el ends here
