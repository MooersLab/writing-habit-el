;;; writing-habit-progress.el --- Progress of the unfinished projects -*- lexical-binding: t; -*-

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

;; The twin of the Progress tab of the Python window.  A table named
;; progress at the end of the week file lists the unfinished projects:
;;
;;   #+NAME: progress
;;   | Project ID | Project name | Est. hours | Hours spent | Weight | Deadline   | Days to deadline | Next week |
;;   |------------+--------------+------------+-------------+--------+------------+------------------+-----------|
;;   | 1006       | AIrxOpt      |        100 |         2.5 |    0.9 | 2026-12-15 |               49 | yes       |
;;
;; The writer types the estimate, the weight, and the deadline.  The hours
;; spent are summed from the zTimeSpent table of mytime.db, matched by the
;; project's four-digit number, and the days to the deadline are counted
;; from today each time the table is written.  The columns are found by
;; the words of the header, so a table written by either port reads in the
;; other, and a table written before a column existed still reads.
;;
;; `writing-habit-progress' shows the table as a dashboard of horizontal
;; bars.  The whole bar is the estimate, the filled part the hours spent,
;; and the light part the hours left; the days to the deadline follow the
;; bar.  A checked project whose weight is above 0.85 is highlighted.
;;
;; Commands, run from a buffer that shows a weekly table:
;;   `writing-habit-progress'          the progress dashboard
;;   `writing-habit-progress-update'   fill Hours spent from mytime.db
;;   `writing-habit-progress-seed-command'  add the current list's projects
;;   `writing-habit-progress-plan-next-week'  next week's plan from the weights

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-lists)
(require 'writing-habit-name)
(require 'writing-habit-table)
(require 'writing-habit-track)
(require 'writing-habit-checks)
(require 'writing-habit-hours)

(declare-function writing-habit-table-current "writing-habit-table-mode")
(declare-function writing-habit-table--apply "writing-habit-table-mode" (model))
(declare-function writing-habit-table--replace-contents "writing-habit-table-mode" (source))

(defgroup writing-habit-progress nil
  "The progress table of the unfinished projects."
  :group 'writing-habit
  :prefix "writing-habit-progress-")

(defconst writing-habit-progress-name "progress"
  "The #+NAME of the progress table.")

(defconst writing-habit-progress-header
  '("Project ID" "Project name" "Est. hours" "Hours spent" "Weight"
    "Deadline" "Days to deadline" "Next week")
  "The header of the progress table, as both ports write it.")

(defconst writing-habit-progress--fields
  '(("project id" . number) ("project name" . name) ("est. hours" . estimate)
    ("hours spent" . spent) ("weight" . weight) ("deadline" . deadline)
    ("days to deadline" . days) ("next week" . planned))
  "The field each header word names.")

(defconst writing-habit-progress--old-order
  '(number name estimate spent weight planned)
  "The columns of a table with no header row, as the first tables were written.")

(defconst writing-habit-progress--no '("no" "n" "-" "[ ]" "0" "false" "off")
  "The words that leave a project unchecked for next week.")

(defcustom writing-habit-progress-high-weight 0.85
  "A checked project whose weight is above this is highlighted."
  :type 'number)

(defcustom writing-habit-progress-soon-days 14
  "The days to a deadline are drawn as soon when it is this close."
  :type 'integer)

(defvar writing-habit-progress-today-function
  (lambda () (format-time-string "%Y-%m-%d"))
  "Return today's date as YYYY-MM-DD.  Tests bind it to a fixed day.")

(cl-defstruct (writing-habit-progress-row
               (:constructor writing-habit-progress-row-create)
               (:copier nil))
  "One project of the progress table.
ESTIMATE, SPENT, and WEIGHT are numbers or nil, DEADLINE an ISO date or
nil, and PLANNED is non-nil when the project is checked for next week."
  number name estimate (spent 0) weight (planned t) deadline)

;;;; Reading and writing

(defun writing-habit-progress--cells (line)
  "Return the trimmed cells of table LINE."
  (let ((inner (string-trim line)))
    (when (string-prefix-p "|" inner) (setq inner (substring inner 1)))
    (when (string-suffix-p "|" inner) (setq inner (substring inner 0 -1)))
    (mapcar #'string-trim (split-string inner "|"))))

(defun writing-habit-progress--number (text)
  "Return TEXT as a number, or nil when it is empty or not a number."
  (let ((text (string-trim (replace-regexp-in-string "," "" (or text "")))))
    (when (string-match-p "\\`-?[0-9]*\\.?[0-9]+\\'" text)
      (string-to-number text))))

(defun writing-habit-progress--date (text)
  "Return the ISO date in TEXT when it is a real date, else nil."
  (when (and text (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" text))
    (let ((y (string-to-number (match-string 1 text)))
          (m (string-to-number (match-string 2 text)))
          (d (string-to-number (match-string 3 text)))
          (iso (match-string 0 text)))
      (when (and (<= 1 m 12) (<= 1 d (calendar-last-day-of-month m y)))
        iso))))

(defun writing-habit-progress--fmt (value)
  "Format number VALUE the way the Python port does, or the empty string."
  (cond ((null value) "")
        ((= value (ftruncate (float value))) (format "%d" (truncate value)))
        (t (let ((s (format "%.2f" value)))
             (string-trim-right (string-trim-right s "0") "\\.")))))

(defun writing-habit-progress--day-number (iso)
  "Return the absolute day number of ISO date ISO."
  (calendar-absolute-from-gregorian
   (list (string-to-number (substring iso 5 7))
         (string-to-number (substring iso 8 10))
         (string-to-number (substring iso 0 4)))))

(defun writing-habit-progress-days-left (row &optional today)
  "Return the days from TODAY to ROW's deadline, negative once it has passed."
  (let ((due (writing-habit-progress-row-deadline row)))
    (when due
      (- (writing-habit-progress--day-number due)
         (writing-habit-progress--day-number
          (or today (funcall writing-habit-progress-today-function)))))))

(defun writing-habit-progress-days-text (days)
  "Say how far off a deadline DAYS away is, for the end of a bar."
  (cond ((null days) "")
        ((= days 0) "due today")
        ((< days 0) (format "%d d late" (- days)))
        (t (format "%d d" days))))

(defun writing-habit-progress-remaining (row)
  "Return the hours ROW has left, or nil without an estimate."
  (when (writing-habit-progress-row-estimate row)
    (max 0 (- (writing-habit-progress-row-estimate row)
              (writing-habit-progress-row-spent row)))))

(defun writing-habit-progress-high-p (row)
  "Return non-nil when the weight of ROW is above the high mark."
  (let ((w (writing-habit-progress-row-weight row)))
    (and w (> w writing-habit-progress-high-weight))))

(defun writing-habit-progress-label (row)
  "Return ROW's number and name."
  (string-trim (format "%s %s" (writing-habit-progress-row-number row)
                       (writing-habit-progress-row-name row))))

(defun writing-habit-progress-span (lines)
  "Return (NAME-LINE FIRST END) of the progress table in LINES, or nil."
  (let ((i 0) found)
    (while (and (not found) (< i (length lines)))
      (let ((line (nth i lines)))
        (when (and (string-match "\\`[ \t]*#\\+NAME:[ \t]*\\(.*?\\)[ \t]*\\'" line)
                   (string= (downcase (match-string 1 line)) writing-habit-progress-name))
          (let ((first (1+ i)))
            (while (and (< first (length lines))
                        (string-prefix-p "#+" (string-trim-left (nth first lines))))
              (setq first (1+ first)))
            (let ((end first))
              (while (and (< end (length lines))
                          (string-prefix-p "|" (string-trim-left (nth end lines))))
                (setq end (1+ end)))
              (setq found (list i first end))))))
      (setq i (1+ i)))
    found))

(defun writing-habit-progress-read (lines)
  "Return the rows of the progress table in LINES, or nil when there is none.
A table with no project rows returns the symbol `empty'."
  (let ((span (writing-habit-progress-span lines)))
    (when span
      (let ((order writing-habit-progress--old-order) (rows '()))
        (dolist (line (seq-subseq lines (nth 1 span) (nth 2 span)))
          (let ((cells (writing-habit-progress--cells line)))
            (cond
             ((string-match-p "\\`[ \t]*|[-+]" line))
             ((string-prefix-p "project" (downcase (or (car cells) "")))
              (setq order (mapcar (lambda (c) (cdr (assoc (downcase c) writing-habit-progress--fields)))
                                  cells)))
             (t
              (let ((value (cl-loop for f in order for c in cells when f collect (cons f c))))
                (cl-flet ((get (f) (or (cdr (assq f value)) "")))
                  (let ((number (cond ((string-match "[0-9]\\{4\\}" (get 'number)) (match-string 0 (get 'number)))
                                      ((string-match "\\`[0-9]\\{4\\}" (get 'name)) (match-string 0 (get 'name)))))
                        (name (get 'name)))
                    (when number
                      (when (string-prefix-p number name) (setq name (substring name 4)))
                      (push (writing-habit-progress-row-create
                             :number number :name (string-trim name)
                             :estimate (writing-habit-progress--number (get 'estimate))
                             :spent (or (writing-habit-progress--number (get 'spent)) 0)
                             :weight (writing-habit-progress--number (get 'weight))
                             :planned (not (member (downcase (string-trim (get 'planned)))
                                                   writing-habit-progress--no))
                             :deadline (writing-habit-progress--date (get 'deadline)))
                            rows)))))))))
        (or (nreverse rows) 'empty)))))

(defun writing-habit-progress-render (rows)
  "Return the lines of an aligned progress table holding ROWS."
  (let* ((today (funcall writing-habit-progress-today-function))
         (body (mapcar (lambda (r)
                         (let ((days (writing-habit-progress-days-left r today)))
                           (list (writing-habit-progress-row-number r)
                                 (writing-habit-progress-row-name r)
                                 (writing-habit-progress--fmt (writing-habit-progress-row-estimate r))
                                 (writing-habit-progress--fmt (writing-habit-progress-row-spent r))
                                 (writing-habit-progress--fmt (writing-habit-progress-row-weight r))
                                 (or (writing-habit-progress-row-deadline r) "")
                                 (if days (number-to-string days) "")
                                 (if (writing-habit-progress-row-planned r) "yes" "no"))))
                       rows))
         (table (cons writing-habit-progress-header body))
         (widths (cl-loop for i below (length writing-habit-progress-header)
                          collect (apply #'max (mapcar (lambda (row) (string-width (nth i row))) table))))
         (right '(2 3 4 6)))
    (cl-flet ((line (row header)
                (concat "| "
                        (mapconcat #'identity
                                   (cl-loop for c in row for w in widths for i from 0
                                            collect (if (and (memq i right) (not header))
                                                        (string-pad c w nil t)
                                                      (string-pad c w)))
                                   " | ")
                        " |")))
      (append (list (line writing-habit-progress-header t)
                    (concat "|" (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "+") "|"))
              (mapcar (lambda (row) (line row nil)) body)))))

(defun writing-habit-progress-write (lines rows)
  "Return LINES with the progress table holding ROWS, added at the end if missing."
  (let ((table (writing-habit-progress-render rows))
        (span (writing-habit-progress-span lines)))
    (if span
        (append (seq-take lines (nth 1 span)) table (nthcdr (nth 2 span) lines))
      (let ((lines (reverse lines)))
        (while (and lines (string-empty-p (string-trim (car lines))))
          (setq lines (cdr lines)))
        (append (nreverse lines)
                (list "" (concat "#+NAME: " writing-habit-progress-name))
                table (list ""))))))

(defun writing-habit-progress-seed (entries rows)
  "Return ROWS plus a row for each numbered project of list ENTRIES.
A project already in ROWS is kept as it is.  The result is sorted by number."
  (let ((rows (copy-sequence rows))
        (have (mapcar #'writing-habit-progress-row-number rows)))
    (dolist (entry entries)
      (let ((project (or (writing-habit-lists-entry-project entry) "")))
        (when (string-match "\\`[ \t]*\\([0-9]\\{4\\}\\)\\(.*\\)" project)
          (let ((number (match-string 1 project)) (name (string-trim (match-string 2 project))))
            (unless (member number have)
              (push number have)
              (setq rows (append rows (list (writing-habit-progress-row-create
                                             :number number :name name
                                             :deadline (writing-habit-progress--date
                                                        (writing-habit-lists-entry-deadline entry)))))))))))
    (sort rows (lambda (a b) (string< (writing-habit-progress-row-number a)
                                      (writing-habit-progress-row-number b))))))

(defun writing-habit-progress-fill-deadlines (rows entries)
  "Give each of ROWS without a deadline the one its project has in ENTRIES.
Return how many rows gained a deadline.  A deadline already set is kept."
  (let ((found '()) (added 0))
    (dolist (entry entries)
      (let ((project (or (writing-habit-lists-entry-project entry) ""))
            (due (writing-habit-progress--date (writing-habit-lists-entry-deadline entry))))
        (when (and due (string-match "\\`[ \t]*\\([0-9]\\{4\\}\\)" project)
                   (not (assoc (match-string 1 project) found)))
          (push (cons (match-string 1 project) due) found))))
    (dolist (row rows)
      (let ((due (cdr (assoc (writing-habit-progress-row-number row) found))))
        (when (and due (not (writing-habit-progress-row-deadline row)))
          (setf (writing-habit-progress-row-deadline row) due)
          (cl-incf added))))
    added))

(defun writing-habit-progress-hours-spent (path numbers)
  "Return (NUMBER . HOURS) for each of NUMBERS from zTimeSpent in PATH.
A session counts for a project when its ProjectID or the start of its
ProjectDirectory is the project's number.  The minutes come from the
Start and End times, so the tracker's computed columns are not used.
Nothing is written to PATH."
  (let ((path (expand-file-name path))
        (totals (mapcar (lambda (n) (cons n 0)) numbers)))
    (unless (file-exists-p path)
      (user-error "No time-tracking database at %s" path))
    (let ((src (sqlite-open path)))
      (unwind-protect
          (dolist (row (sqlite-select src "SELECT Start, \"End\", ProjectID, ProjectDirectory FROM zTimeSpent"))
            (let ((start (writing-habit-track--clock (nth 0 row)))
                  (end (writing-habit-track--clock (nth 1 row)))
                  (number (writing-habit-track--project-number (nth 2 row) (nth 3 row))))
              (when (and start end number (assoc number totals))
                (let ((minutes (writing-habit-track--span start end)))
                  (when (> minutes 0)
                    (cl-incf (cdr (assoc number totals)) minutes))))))
        (sqlite-close src)))
    (mapcar (lambda (c) (cons (car c) (/ (fround (* 10 (/ (cdr c) 60.0))) 10)))
            totals)))

;;;; The week buffer

(defun writing-habit-progress--buffer-lines ()
  "Return the lines of the current buffer."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(defun writing-habit-progress--set-buffer-lines (lines)
  "Replace the current buffer's text with LINES, changing only what differs."
  (let ((text (mapconcat #'identity lines "\n")))
    (unless (string= text (buffer-substring-no-properties (point-min) (point-max)))
      (let ((source (generate-new-buffer " *writing-habit progress*")))
        (unwind-protect
            (progn
              (with-current-buffer source (insert text))
              (save-excursion (writing-habit-table--replace-contents source)))
          (kill-buffer source))))))

(defun writing-habit-progress--entries ()
  "Return the legend and every project list of the week in the current buffer."
  (let* ((table (writing-habit-table-current))
         (after (writing-habit-table-after table))
         (legend (mapcar (lambda (l) (writing-habit-lists-entry-create
                                      :code (nth 0 l) :description (nth 1 l)))
                         (writing-habit-table-legend table))))
    (append legend
            (apply #'append (mapcar (lambda (name) (writing-habit-lists-entries after name))
                                    (mapcar #'car (writing-habit-lists-names-all)))))))

(defun writing-habit-progress-rows ()
  "Return the progress rows of the week in the current buffer, or nil."
  (let ((rows (writing-habit-progress-read (writing-habit-progress--buffer-lines))))
    (if (eq rows 'empty) '() rows)))

(defun writing-habit-progress--store (rows)
  "Write ROWS into the progress table of the current buffer."
  (writing-habit-progress--set-buffer-lines
   (writing-habit-progress-write (writing-habit-progress--buffer-lines) rows)))

(defvar writing-habit-progress--week-buffer nil
  "The week buffer the progress dashboard shows.")

(defun writing-habit-progress--week ()
  "Return the week buffer to act on.
It is the current buffer, or in the dashboard the week it was drawn from."
  (cond ((derived-mode-p 'writing-habit-progress-mode)
         (or (and (buffer-live-p writing-habit-progress--week-buffer)
                  writing-habit-progress--week-buffer)
             (user-error "The week buffer is gone; open it and run the command again")))
        (t (current-buffer))))

;;;###autoload
(defun writing-habit-progress-seed-command ()
  "Add a row for each numbered project of the current semester list.
The progress table is made at the end of the file when there is none."
  (interactive)
  (with-current-buffer (writing-habit-progress--week)
    (let* ((table (writing-habit-table-current))
           (current (car (writing-habit-lists-names)))
           (before (writing-habit-progress-rows))
           (rows (writing-habit-progress-seed
                  (writing-habit-lists-entries (writing-habit-table-after table) current)
                  before)))
      (writing-habit-progress-fill-deadlines rows (writing-habit-progress--entries))
      (writing-habit-progress--store rows)
      (message "Added %d project%s to the progress table"
               (- (length rows) (length before)) (if (= 1 (- (length rows) (length before))) "" "s"))))
  (writing-habit-progress--refresh-dashboard))

;;;###autoload
(defun writing-habit-progress-update (&optional path)
  "Fill Hours spent from the zTimeSpent table of mytime.db at PATH.
PATH defaults to `writing-habit-track-mytime-db'.  Empty deadlines are
filled from the legend and the project lists.  The buffer is changed,
not saved."
  (interactive)
  (prog1 (with-current-buffer (writing-habit-progress--week)
    (let ((rows (writing-habit-progress-rows)))
      (unless rows (user-error "The progress table has no projects to update"))
      (let* ((hours (writing-habit-progress-hours-spent
                     (or path writing-habit-track-mytime-db)
                     (mapcar #'writing-habit-progress-row-number rows)))
             (changed 0))
        (dolist (row rows)
          (let ((new (cdr (assoc (writing-habit-progress-row-number row) hours))))
            (when (and new (/= new (writing-habit-progress-row-spent row)))
              (setf (writing-habit-progress-row-spent row) new)
              (cl-incf changed))))
        (writing-habit-progress-fill-deadlines rows (writing-habit-progress--entries))
        (writing-habit-progress--store rows)
        (message "Updated the hours spent: %d project%s changed, %s hours in all"
                 changed (if (= changed 1) "" "s")
                 (writing-habit-progress--fmt (apply #'+ (mapcar #'cdr hours))))
        changed)))
    (writing-habit-progress--refresh-dashboard)))

;;;; The dashboard

(defface writing-habit-progress-spent '((t :foreground "#4a9e5c"))
  "Face of the hours spent in a bar.")
(defface writing-habit-progress-left '((t :foreground "#b8b6ae"))
  "Face of the hours left in a bar.")
(defface writing-habit-progress-over '((t :foreground "#2c6b3a"))
  "Face of the hours past the estimate.")
(defface writing-habit-progress-late '((t :foreground "#b3261e" :weight bold))
  "Face of the days to a deadline that has passed.")
(defface writing-habit-progress-soon '((t :foreground "#a15c00"))
  "Face of the days to a deadline that is near.")
(defface writing-habit-progress-muted '((t :inherit shadow))
  "Face of quiet text on the dashboard.")
(defface writing-habit-progress-high '((t :background "#fff1b8" :foreground "black"))
  "Face of the label of a checked project whose weight is high.")

(defun writing-habit-progress--days-face (days)
  "Return the face for a deadline DAYS away."
  (cond ((null days) 'writing-habit-progress-muted)
        ((< days 0) 'writing-habit-progress-late)
        ((<= days writing-habit-progress-soon-days) 'writing-habit-progress-soon)
        (t 'writing-habit-progress-muted)))

(defun writing-habit-progress--scale (rows)
  "Return the hours the full width of the bars of ROWS stands for."
  (max 1 (apply #'max 0 (mapcar (lambda (r) (max (or (writing-habit-progress-row-estimate r) 0)
                                                (writing-habit-progress-row-spent r)))
                               rows))))

(defun writing-habit-progress-bar (row scale width)
  "Return ROW's bar, WIDTH characters for SCALE hours, as a propertized string."
  (let* ((cells (lambda (hours) (round (* width (/ (float hours) scale)))))
         (estimate (writing-habit-progress-row-estimate row))
         (spent (writing-habit-progress-row-spent row)))
    (if (not (and estimate (> estimate 0)))
        (concat (propertize (make-string (max (funcall cells spent) (if (> spent 0) 1 0)) ?█)
                            'face 'writing-habit-progress-spent)
                (propertize " no estimate" 'face 'writing-habit-progress-muted))
      (let* ((whole (max 1 (funcall cells estimate)))
             (done (min whole (funcall cells (min spent estimate))))
             (over (max 0 (- (funcall cells spent) whole))))
        (concat (propertize (make-string done ?█) 'face 'writing-habit-progress-spent)
                (propertize (make-string (- whole done) ?░) 'face 'writing-habit-progress-left)
                (if (> spent estimate)
                    (concat (propertize "|" 'face 'writing-habit-progress-muted)
                            (propertize (make-string over ?▓) 'face 'writing-habit-progress-over))
                  ""))))))

(defun writing-habit-progress-dashboard-string (rows &optional width)
  "Return the progress dashboard of ROWS as a propertized string WIDTH wide."
  (let* ((width (or width 100))
         (rows (sort (copy-sequence rows)
                     (lambda (a b) (string< (writing-habit-progress-row-number a)
                                            (writing-habit-progress-row-number b)))))
         (labels (mapcar #'writing-habit-progress-label rows))
         (label-width (min 32 (apply #'max 4 (mapcar #'string-width labels))))
         (bar-width (max 10 (- width label-width 16)))
         (scale (writing-habit-progress--scale rows))
         (today (funcall writing-habit-progress-today-function)))
    (with-temp-buffer
      (insert (propertize "Progress of the unfinished projects\n" 'face 'bold)
              (propertize (format "Bars: █ hours spent, ░ hours left of the estimate, ▓ past it.  Full width is %s hours.  %s\n\n"
                                  (writing-habit-progress--fmt scale) today)
                          'face 'writing-habit-progress-muted))
      (if (null rows)
          (insert "No progress table in this week file yet.  Press a to add the current list's projects.\n")
        (cl-loop for row in rows for label in labels do
                 (let* ((days (writing-habit-progress-days-left row today))
                        (text (truncate-string-to-width label label-width nil ?\s "…")))
                   (insert (if (and (writing-habit-progress-row-planned row)
                                    (writing-habit-progress-high-p row))
                               (propertize text 'face 'writing-habit-progress-high)
                             text)
                           " "
                           (writing-habit-progress-bar row scale bar-width))
                   (let ((d (writing-habit-progress-days-text days)))
                     (unless (string-empty-p d)
                       (insert " " (propertize d 'face (writing-habit-progress--days-face days)))))
                   (insert "\n")))
        (insert "\n"
                (propertize
                 (format "%-6s %-24s %7s %7s %6s %10s %5s %4s\n" "ID" "Name" "Est. h" "Spent" "Weight"
                         "Deadline" "Days" "Next")
                 'face 'bold))
        (dolist (row rows)
          (let ((days (writing-habit-progress-days-left row today)))
            (insert (format "%-6s %-24s %7s %7s %6s %10s %5s %4s\n"
                            (writing-habit-progress-row-number row)
                            (truncate-string-to-width (writing-habit-progress-row-name row) 24 nil nil "…")
                            (writing-habit-progress--fmt (writing-habit-progress-row-estimate row))
                            (writing-habit-progress--fmt (writing-habit-progress-row-spent row))
                            (writing-habit-progress--fmt (writing-habit-progress-row-weight row))
                            (or (writing-habit-progress-row-deadline row) "")
                            (if days (number-to-string days) "")
                            (if (writing-habit-progress-row-planned row) "yes" "no"))))))
      (insert "\n" (propertize "g refresh   u update hours spent   a add the current list's projects   e edit the table   n plan next week   q quit\n"
                               'face 'writing-habit-progress-muted))
      (buffer-string))))

(defvar writing-habit-progress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'writing-habit-progress-refresh)
    (define-key map (kbd "u") #'writing-habit-progress-update)
    (define-key map (kbd "a") #'writing-habit-progress-seed-command)
    (define-key map (kbd "e") #'writing-habit-progress-edit)
    (define-key map (kbd "n") #'writing-habit-progress-plan-next-week)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap of `writing-habit-progress-mode'.")

(define-derived-mode writing-habit-progress-mode special-mode "WH-Progress"
  "The progress dashboard of a week's unfinished projects.
\\{writing-habit-progress-mode-map}"
  (setq truncate-lines t))

(defun writing-habit-progress--dashboard-buffer ()
  "Return the dashboard buffer."
  (get-buffer-create "*writing-habit progress*"))

(defun writing-habit-progress-refresh ()
  "Draw the dashboard again from the week buffer."
  (interactive)
  (let* ((week (writing-habit-progress--week))
         (rows (with-current-buffer week (writing-habit-progress-rows)))
         (buffer (writing-habit-progress--dashboard-buffer))
         (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (width (if window (window-body-width window) 100)))
        (unless (derived-mode-p 'writing-habit-progress-mode) (writing-habit-progress-mode))
        (setq-local writing-habit-progress--week-buffer week)
        (erase-buffer)
        (insert (writing-habit-progress-dashboard-string rows width))
        (goto-char (point-min))))
    buffer))

(defun writing-habit-progress--refresh-dashboard ()
  "Redraw the dashboard when it is shown."
  (let ((buffer (get-buffer "*writing-habit progress*")))
    (when (and buffer (get-buffer-window buffer))
      (with-current-buffer buffer (writing-habit-progress-refresh)))))

(defun writing-habit-progress-edit ()
  "Jump to the progress table in the week buffer, to type estimates and weights."
  (interactive)
  (let ((week (writing-habit-progress--week)))
    (pop-to-buffer week)
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*#\\+NAME:[ \t]*progress[ \t]*$" nil t)
        (forward-line 3)
      (message "This week file has no progress table yet"))))

;;;###autoload
(defun writing-habit-progress ()
  "Show the progress dashboard of the week in the current buffer."
  (interactive)
  (let ((week (writing-habit-progress--week)))
    (with-current-buffer (writing-habit-progress--dashboard-buffer)
      (unless (derived-mode-p 'writing-habit-progress-mode) (writing-habit-progress-mode))
      (setq-local writing-habit-progress--week-buffer week))
    (pop-to-buffer (writing-habit-progress--dashboard-buffer))
    (writing-habit-progress-refresh)))

;;;; Next week's plan from the progress table

;; The twin of progress.share, progress.place, and the Plan next week from
;; progress item of the Python Coach menu.  The week's hours are shared
;; among the checked projects by weight, never more than a project has
;; left, and each project's blocks go in the hours the energy profile
;; rates best for new writing.  The notes match the Python port word for
;; word.

(defconst writing-habit-progress-next-week "next week"
  "The list that accepted projects are added to.")

(cl-defstruct (writing-habit-progress-block
               (:constructor writing-habit-progress-block-create)
               (:copier nil))
  "One block of next week's plan: day OFFSET, START and END, its ROW, and LEVEL."
  offset start end row level)

(cl-defstruct (writing-habit-progress-plan
               (:constructor writing-habit-progress-plan-create)
               (:copier nil))
  "Next week's plan.
CAPACITY is the minutes shared, SHARES a list of (ROW . MINUTES), BLOCKS
the placed blocks, UNPLACED a list of (ROW . MINUTES) that found no slot,
LEFT-OUT the rows without an estimate or a weight, and ROWS every row."
  capacity shares blocks unplaced left-out rows)

(defun writing-habit-progress-eligible-p (row)
  "Return non-nil when ROW can take part in a plan: a weight and time left."
  (let ((w (writing-habit-progress-row-weight row))
        (left (writing-habit-progress-remaining row)))
    (and w (> w 0) left (> left 0))))

(defun writing-habit-progress--by-weight (rows)
  "Return ROWS by weight, highest first, then by number."
  (sort (copy-sequence rows)
        (lambda (a b)
          (let ((wa (writing-habit-progress-row-weight a)) (wb (writing-habit-progress-row-weight b)))
            (or (> wa wb)
                (and (= wa wb) (string< (writing-habit-progress-row-number a)
                                        (writing-habit-progress-row-number b))))))))

(defun writing-habit-progress-plan-eligible (plan)
  "Return the rows of PLAN that could take part, checked or not, by weight."
  (writing-habit-progress--by-weight
   (seq-filter #'writing-habit-progress-eligible-p (writing-habit-progress-plan-rows plan))))

(defun writing-habit-progress-share (rows capacity &optional unit)
  "Share CAPACITY minutes among ROWS by weight, UNIT minutes at a time.
UNIT defaults to 30.  Only rows checked for next week, with a weight
above zero and time remaining, take part.  A row never gets more than it
has left, and minutes it cannot use go to the others in proportion to
their weights.  Return (ROW . MINUTES) by weight, highest first."
  (let* ((unit (or unit 30))
         (pool (seq-filter (lambda (r) (and (writing-habit-progress-eligible-p r)
                                            (writing-habit-progress-row-planned r)))
                           rows))
         (left (mapcar (lambda (r) (cons r (truncate (* 60 (writing-habit-progress-remaining r)))))
                       pool))
         (got (mapcar (lambda (r) (cons r 0)) pool)))
    (catch 'full
      (dotimes (_ (/ capacity unit))
        (let ((open (seq-filter (lambda (r) (>= (- (cdr (assq r left)) (cdr (assq r got))) unit))
                                pool)))
          (unless open (throw 'full nil))
          (let* ((total (apply #'+ (mapcar #'writing-habit-progress-row-weight open)))
                 (given (+ unit (apply #'+ (mapcar (lambda (r) (cdr (assq r got))) open))))
                 (pick nil) (best nil))
            ;; Give the next unit to the open row furthest below its fair
            ;; share of what the open rows hold.
            (dolist (r open)
              (let ((key (list (- (* (/ (float (writing-habit-progress-row-weight r)) total) given)
                                  (cdr (assq r got)))
                               (writing-habit-progress-row-weight r)
                               (- (string-to-number (writing-habit-progress-row-number r))))))
                (when (or (null best) (writing-habit-progress--key> key best))
                  (setq best key pick r))))
            (cl-incf (cdr (assq pick got)) unit)))))
    (delq nil (mapcar (lambda (r) (let ((m (cdr (assq r got)))) (when (> m 0) (cons r m))))
                      (writing-habit-progress--by-weight pool)))))

(defun writing-habit-progress--key> (a b)
  "Return non-nil when the number list A comes after B."
  (cond ((null a) nil)
        ((> (car a) (car b)) t)
        ((< (car a) (car b)) nil)
        (t (writing-habit-progress--key> (cdr a) (cdr b)))))

(defun writing-habit-progress--key< (a b)
  "Return non-nil when the number list A comes before B."
  (writing-habit-progress--key> b a))

(defun writing-habit-progress-place (shares profile &optional days longest first-hour last-hour)
  "Place each share of SHARES in blocks, by PROFILE.  Return (BLOCKS . UNPLACED).
DAYS are the day offsets, Monday to Friday by default.  Blocks are at
most LONGEST minutes, 90 by default, start on the quarter hour from
FIRST-HOUR, 5, and end by LAST-HOUR, 21.  The heaviest projects go first,
into the hours PROFILE rates best for generative work.  Blocks never
overlap, keep every run within two hours, and a day holds at most four."
  (let* ((days (or days '(0 1 2 3 4)))
         (longest (or longest 90))
         (first-hour (or first-hour 5))
         (last-hour (or last-hour 21))
         (taken (mapcar (lambda (d) (cons d nil)) days))
         (blocks '()) (unplaced '()))
    (dolist (share shares)
      (let ((minutes (cdr share)) (pieces '()) (missing 0))
        (while (> minutes 0)
          (push (min longest minutes) pieces)
          (setq minutes (- minutes (min longest minutes))))
        (dolist (piece (nreverse pieces))
          (let ((best nil))
            (dolist (day days)
              (let ((busy (cdr (assq day taken))))
                (when (< (length busy) 4)
                  (cl-loop for first from (* first-hour 60) to (- (* last-hour 60) piece) by 15 do
                           (let ((last (+ first piece)))
                             (unless (or (seq-some (lambda (iv) (and (< first (cdr iv)) (< (car iv) last))) busy)
                                         (not (writing-habit-checks--fits-runs (cons (cons first last) busy))))
                               (let* ((level (writing-habit-checks-fit
                                              profile "generative"
                                              (writing-habit-hours--clock first)
                                              (writing-habit-hours--clock last)))
                                      (key (list (- level) (length busy) day first)))
                                 (when (or (null best) (writing-habit-progress--key< key (car best)))
                                   (setq best (list key day first last level))))))))))
            (if (null best)
                (setq missing (+ missing piece))
              (let ((day (nth 1 best)) (first (nth 2 best)) (last (nth 3 best)))
                (push (cons first last) (cdr (assq day taken)))
                (push (writing-habit-progress-block-create
                       :offset day :start (writing-habit-hours--clock first)
                       :end (writing-habit-hours--clock last)
                       :row (car share) :level (nth 4 best))
                      blocks)))))
        (when (> missing 0) (push (cons (car share) missing) unplaced))))
    (cons (sort blocks (lambda (a b)
                         (or (< (writing-habit-progress-block-offset a) (writing-habit-progress-block-offset b))
                             (and (= (writing-habit-progress-block-offset a) (writing-habit-progress-block-offset b))
                                  (string< (writing-habit-progress-block-start a)
                                           (writing-habit-progress-block-start b))))))
          (nreverse unplaced))))

(defun writing-habit-progress-plan (rows capacity profile)
  "Suggest next week's plan from progress ROWS, CAPACITY minutes, and PROFILE."
  (let* ((shares (writing-habit-progress-share rows capacity))
         (placed (writing-habit-progress-place shares profile)))
    (writing-habit-progress-plan-create
     :capacity capacity :rows rows :shares shares
     :blocks (car placed) :unplaced (cdr placed)
     :left-out (seq-filter (lambda (r) (or (null (writing-habit-progress-row-estimate r))
                                           (not (writing-habit-progress-row-weight r))
                                           (zerop (writing-habit-progress-row-weight r))))
                           rows))))

(defun writing-habit-progress-block-line (block)
  "Return BLOCK as Monday 05:00-06:30  1006 AIrxOpt."
  (format "%s %s-%s  %s" (aref writing-habit-checks-day-names (writing-habit-progress-block-offset block))
          (writing-habit-progress-block-start block) (writing-habit-progress-block-end block)
          (writing-habit-progress-label (writing-habit-progress-block-row block))))

(defun writing-habit-progress--hours (minutes)
  "Return MINUTES as 7 h 30 min, or 45 min."
  (let ((h (/ minutes 60)) (m (% minutes 60)))
    (if (> h 0) (format "%d h %02d min" h m) (format "%d min" m))))

(defun writing-habit-progress--minutes-of (plan row)
  "Return the minutes PLAN gives ROW."
  (or (cdr (seq-find (lambda (s) (equal (writing-habit-progress-row-number (car s))
                                        (writing-habit-progress-row-number row)))
                     (writing-habit-progress-plan-shares plan)))
      0))

(defun writing-habit-progress-item-line (plan row)
  "Name ROW's hours and blocks in PLAN, for a checkbox."
  (let ((minutes (writing-habit-progress--minutes-of plan row)))
    (if (zerop minutes)
        (format "%s: not in this plan" (writing-habit-progress-label row))
      (let ((when (mapcar (lambda (b)
                            (format "%s %s-%s"
                                    (aref writing-habit-checks-day-names (writing-habit-progress-block-offset b))
                                    (writing-habit-progress-block-start b) (writing-habit-progress-block-end b)))
                          (seq-filter (lambda (b) (equal (writing-habit-progress-row-number
                                                          (writing-habit-progress-block-row b))
                                                         (writing-habit-progress-row-number row)))
                                      (writing-habit-progress-plan-blocks plan)))))
        (concat (format "%s: %d h %02d min" (writing-habit-progress-label row) (/ minutes 60) (% minutes 60))
                (if when (format " (%s)" (string-join when "; ")) ""))))))

(defun writing-habit-progress-plan-facts (plan)
  "Write PLAN as the facts the model receives."
  (let ((lines (list (format "Minutes to plan next week: %d (%s)"
                             (writing-habit-progress-plan-capacity plan)
                             (writing-habit-progress--hours (writing-habit-progress-plan-capacity plan))))))
    (dolist (s (writing-habit-progress-plan-shares plan))
      (let ((row (car s)))
        (push (format "Project %s: weight %s, %s hours left of %s, planned %d minutes (%s)"
                      (writing-habit-progress-label row)
                      (writing-habit-progress--fmt (writing-habit-progress-row-weight row))
                      (writing-habit-progress--fmt (writing-habit-progress-remaining row))
                      (writing-habit-progress--fmt (writing-habit-progress-row-estimate row))
                      (cdr s) (writing-habit-progress--hours (cdr s)))
              lines)))
    (dolist (b (writing-habit-progress-plan-blocks plan))
      (push (format "Block: %s, in %s hours for new writing" (writing-habit-progress-block-line b)
                    (writing-habit-checks-word (writing-habit-progress-block-level b)))
            lines))
    (dolist (u (writing-habit-progress-plan-unplaced plan))
      (push (format "Not placed: %s, %d minutes, for want of a free slot"
                    (writing-habit-progress-label (car u)) (cdr u))
            lines))
    (dolist (row (writing-habit-progress-plan-left-out plan))
      (push (format "Left out: %s, because it has no %s" (writing-habit-progress-label row)
                    (if (writing-habit-progress-row-estimate row) "weight" "estimate"))
            lines))
    (string-join (nreverse lines) "\n")))

(defun writing-habit-progress--series (items)
  "Join ITEMS with commas and a final and."
  (cond ((null (cdr items)) (car items))
        ((null (cddr items)) (format "%s and %s" (car items) (cadr items)))
        (t (concat (string-join (butlast items) ", ") ", and " (car (last items))))))

(defun writing-habit-progress-plan-plain (plan)
  "Write the note on PLAN without a model, as the Python port does."
  (let ((shares (writing-habit-progress-plan-shares plan))
        (left-out (writing-habit-progress-plan-left-out plan)))
    (cond
     ((and (null shares) (writing-habit-progress-plan-eligible plan))
      "No project is checked for next week, so there is nothing to share out. Check the projects to work on and press Update the plan.")
     ((null shares)
      "No project in the progress table has both a weight and time left, so there is nothing to share out. Fill in the Est. hours and Weight columns on the Progress tab.")
     (t
      (concat
       (if (null (cdr shares))
           (format "All of next week's %s go to %s." (writing-habit-progress--hours (cdar shares))
                   (writing-habit-progress-label (caar shares)))
         (format "Next week's %s go by weight to %s."
                 (writing-habit-progress--hours (writing-habit-progress-plan-capacity plan))
                 (writing-habit-progress--series
                  (mapcar (lambda (s) (format "%s (%s)" (writing-habit-progress-label (car s))
                                              (writing-habit-progress--hours (cdr s))))
                          shares))))
       " The heaviest projects take the hours your energy profile rates best for new writing, in blocks of at most 90 minutes with a break after every two hours."
       (if (writing-habit-progress-plan-unplaced plan)
           (concat " " (mapconcat (lambda (u) (format "%s of %s found no free slot"
                                                      (writing-habit-progress--hours (cdr u))
                                                      (writing-habit-progress-label (car u))))
                                  (writing-habit-progress-plan-unplaced plan) "; ")
                   ".")
         "")
       (if left-out
           (format " %d project%s without an estimate or a weight %s left out."
                   (length left-out) (if (= 1 (length left-out)) "" "s")
                   (if (= 1 (length left-out)) "was" "were"))
         ""))))))

;;;; Applying an accepted plan to the week buffer

(defun writing-habit-progress-add-to-next-week (lines rows)
  "Return (LINES . ADDED) with each of ROWS put on the Next week list of LINES.
LINES are the lines after the weekly table.  A project already on the
list is skipped.  An entry of the current list is copied without its code,
so its deadline, risk, and activity come along."
  (let* ((target writing-habit-progress-next-week)
         (key (lambda (e) (substring (concat (or (writing-habit-lists-entry-project e) "") "    ") 0 4)))
         (have (mapcar key (and (writing-habit-lists--span lines target)
                                (writing-habit-lists-entries lines target))))
         (current (writing-habit-lists-entries lines writing-habit-lists-current))
         (added 0))
    (unless (writing-habit-lists--span lines target)
      ;; The week lists sit before the period line and the semester lists.
      (let ((at (or (cl-position-if (lambda (l) (string-match-p "\\`[ \t]*#\\+PROJECT_PERIOD:" l)) lines)
                    (car (sort (delq nil (mapcar (lambda (n) (writing-habit-lists--name-line lines n))
                                                 (writing-habit-lists-names)))
                               #'<))
                    (length lines))))
        (setq lines (append (seq-take lines at)
                            (list (format "#+NAME: %s" target) "")
                            (nthcdr at lines)))))
    (dolist (row rows)
      (let ((number (writing-habit-progress-row-number row)))
        (unless (member number have)
          (let* ((from (seq-find (lambda (e) (equal (funcall key e) number)) current))
                 (entry (if from
                            (writing-habit-lists-entry-create
                             :code nil :description (writing-habit-lists-entry-description from)
                             :risk (writing-habit-lists-entry-risk from)
                             :activity (writing-habit-lists-entry-activity from))
                          (writing-habit-lists-entry-create
                           :code nil :description (concat number (writing-habit-progress-row-name row))))))
            (setq lines (car (writing-habit-lists-insert lines target nil entry)))
            (push number have)
            (cl-incf added)))))
    (cons lines added)))

(defun writing-habit-progress-capacity (table)
  "Return the minutes to share next week: the planned minutes of TABLE.
A week with no blocks gives 600, ten hours."
  (let ((m (apply #'+ (mapcar #'writing-habit-table-block-minutes (writing-habit-table-blocks table)))))
    (if (> m 0) m 600)))

(defun writing-habit-progress-apply-plan (week plan checked)
  "Apply the accepted PLAN to buffer WEEK, with CHECKED the checked rows.
The Next week column keeps each listed row's box, the planned projects go
on the Next week list, and the blocks are put on the kill ring.  Return
the number of projects added to the list."
  (with-current-buffer week
    (let* ((numbers (mapcar #'writing-habit-progress-row-number checked))
           (eligible (writing-habit-progress-plan-eligible plan))
           (added 0))
      ;; The projects go on the Next week list first, through the model.
      (let* ((table (writing-habit-table-current))
             (result (writing-habit-progress-add-to-next-week
                      (writing-habit-table-after table)
                      (mapcar #'car (writing-habit-progress-plan-shares plan)))))
        (setq added (cdr result))
        (when (> added 0)
          (writing-habit-table--set-after table (car result))
          (writing-habit-table--apply table)))
      ;; Then the boxes are kept in the Next week column.
      (let ((rows (writing-habit-progress-rows)))
        (dolist (row rows)
          (when (seq-find (lambda (e) (equal (writing-habit-progress-row-number e)
                                             (writing-habit-progress-row-number row)))
                          eligible)
            (setf (writing-habit-progress-row-planned row)
                  (and (member (writing-habit-progress-row-number row) numbers) t))))
        (writing-habit-progress--store rows))
      (kill-new (mapconcat #'writing-habit-progress-block-line
                           (writing-habit-progress-plan-blocks plan) "\n"))
      added)))

;;;; The plan buffer

(defvar-local writing-habit-progress--plan nil "The plan shown.")
(defvar-local writing-habit-progress--plan-rows nil "The rows listed, by weight.")
(defvar-local writing-habit-progress--plan-checked nil "The numbers of the checked rows.")
(defvar-local writing-habit-progress--plan-note nil "The note above the rows.")
(defvar-local writing-habit-progress--plan-source nil "Where the note came from.")
(defvar-local writing-habit-progress--plan-profile nil "The energy profile used.")
(defvar-local writing-habit-progress--plan-week nil "The week buffer.")

(defun writing-habit-progress--plan-draw ()
  "Draw the plan buffer from its state."
  (let ((inhibit-read-only t) (line (line-number-at-pos)))
    (erase-buffer)
    (insert (propertize "Next week from progress\n\n" 'face 'bold)
            writing-habit-progress--plan-note "\n\n")
    (dolist (row writing-habit-progress--plan-rows)
      (let* ((on (member (writing-habit-progress-row-number row) writing-habit-progress--plan-checked))
             (text (concat (if on "[X] " "[ ] ")
                           (writing-habit-progress-item-line writing-habit-progress--plan row))))
        (insert (propertize (if (and on (writing-habit-progress-high-p row))
                                (propertize text 'face 'writing-habit-progress-high)
                              text)
                            'writing-habit-progress-row row)
                "\n")))
    (insert "\n" (propertize (format "The note is a %s.\n" writing-habit-progress--plan-source)
                             'face 'writing-habit-progress-muted)
            (propertize "SPC toggle   u update the plan   C-c C-c add the checked to Next week   f facts   q discard\n"
                        'face 'writing-habit-progress-muted))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun writing-habit-progress-plan-toggle ()
  "Check or uncheck the project on this line."
  (interactive)
  (let ((row (get-text-property (line-beginning-position) 'writing-habit-progress-row)))
    (unless row (user-error "No project on this line"))
    (let ((n (writing-habit-progress-row-number row)))
      (setq writing-habit-progress--plan-checked
            (if (member n writing-habit-progress--plan-checked)
                (delete n writing-habit-progress--plan-checked)
              (cons n writing-habit-progress--plan-checked))))
    (writing-habit-progress--plan-draw)
    (forward-line 1)))

(defun writing-habit-progress--plan-stale-p ()
  "Return non-nil when the boxes changed after the plan was last made."
  (not (equal (sort (copy-sequence writing-habit-progress--plan-checked) #'string<)
              (sort (mapcar (lambda (r) (writing-habit-progress-row-number r))
                            (seq-filter #'writing-habit-progress-row-planned
                                        writing-habit-progress--plan-rows))
                    #'string<))))

(defun writing-habit-progress-plan-update ()
  "Share the week's hours again among the checked projects only."
  (interactive)
  (dolist (row writing-habit-progress--plan-rows)
    (setf (writing-habit-progress-row-planned row)
          (and (member (writing-habit-progress-row-number row) writing-habit-progress--plan-checked) t)))
  (setq writing-habit-progress--plan
        (writing-habit-progress-plan (writing-habit-progress-plan-rows writing-habit-progress--plan)
                                     (writing-habit-progress-plan-capacity writing-habit-progress--plan)
                                     writing-habit-progress--plan-profile)
        writing-habit-progress--plan-note (writing-habit-progress-plan-plain writing-habit-progress--plan)
        writing-habit-progress--plan-source "plain summary of the updated plan, written without a model")
  (writing-habit-progress--plan-draw))

(defun writing-habit-progress-plan-accept ()
  "Add the checked projects to Next week, keep the boxes, and copy the blocks."
  (interactive)
  (when (writing-habit-progress--plan-stale-p)
    (writing-habit-progress-plan-update))     ; boxes changed after the last update
  (let* ((plan writing-habit-progress--plan)
         (checked (seq-filter (lambda (r) (member (writing-habit-progress-row-number r)
                                                  writing-habit-progress--plan-checked))
                              writing-habit-progress--plan-rows))
         (added (writing-habit-progress-apply-plan writing-habit-progress--plan-week plan checked)))
    (quit-window)
    (message "Added %d project%s to the Next week list and copied next week's suggested blocks"
             added (if (= added 1) "" "s"))
    added))

(defun writing-habit-progress-plan-facts-show ()
  "Show the facts that were sent, or would be sent, to the model."
  (interactive)
  (let ((facts (writing-habit-progress-plan-facts writing-habit-progress--plan)))
    (with-help-window "*writing-habit plan facts*" (princ facts))))

(defvar writing-habit-progress-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'writing-habit-progress-plan-toggle)
    (define-key map (kbd "x") #'writing-habit-progress-plan-toggle)
    (define-key map (kbd "u") #'writing-habit-progress-plan-update)
    (define-key map (kbd "C-c C-c") #'writing-habit-progress-plan-accept)
    (define-key map (kbd "f") #'writing-habit-progress-plan-facts-show)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap of `writing-habit-progress-plan-mode'.")

(define-derived-mode writing-habit-progress-plan-mode special-mode "WH-Plan"
  "Next week's plan from the progress table, one line per project.
\\{writing-habit-progress-plan-mode-map}"
  (setq truncate-lines t))

;;;###autoload
(defun writing-habit-progress-plan-next-week ()
  "Suggest next week's plan from the progress weights and the energy profile.
The week's planned minutes are shared among the checked projects by
weight and placed in the best hours.  In the plan buffer, uncheck a
project and press \\<writing-habit-progress-plan-mode-map>\\[writing-habit-progress-plan-update] to share the hours again, and
\\[writing-habit-progress-plan-accept] to add the checked projects to the Next week list."
  (interactive)
  (let* ((week (writing-habit-progress--week))
         (table (with-current-buffer week (writing-habit-table-current)))
         (rows (with-current-buffer week
                 (or (writing-habit-progress-read (writing-habit-progress--buffer-lines))
                     (user-error "The week file has no progress table; make one with writing-habit-progress-seed-command first"))))
         (rows (if (eq rows 'empty) '() rows))
         (profile (writing-habit-checks-profile))
         (plan (writing-habit-progress-plan rows (writing-habit-progress-capacity table) profile))
         (plain (writing-habit-progress-plan-plain plan)))
    (if (null (writing-habit-progress-plan-eligible plan))
        (progn (message "%s" plain) nil)
      (let ((note (if (writing-habit-progress-plan-shares plan)
                      (with-current-buffer week
                        (writing-habit-checks--note "progress_plan" (writing-habit-progress-plan-facts plan)
                                                    plain (and (writing-habit-coach-allowed-p)
                                                               (writing-habit-checks-week-monday table))))
                    (list :text plain :source "plain summary written without a model")))
            (buffer (get-buffer-create "*writing-habit next week*")))
        (with-current-buffer buffer
          (writing-habit-progress-plan-mode)
          (setq writing-habit-progress--plan plan
                writing-habit-progress--plan-rows (writing-habit-progress-plan-eligible plan)
                writing-habit-progress--plan-checked
                (mapcar #'writing-habit-progress-row-number
                        (seq-filter #'writing-habit-progress-row-planned
                                    (writing-habit-progress-plan-eligible plan)))
                writing-habit-progress--plan-note (plist-get note :text)
                writing-habit-progress--plan-source (plist-get note :source)
                writing-habit-progress--plan-profile profile
                writing-habit-progress--plan-week week)
          (writing-habit-progress--plan-draw))
        (pop-to-buffer buffer)
        buffer))))

(provide 'writing-habit-progress)
;;; writing-habit-progress.el ends here
