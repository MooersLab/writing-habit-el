;;; writing-habit-hours.el --- The week's blocks by the hour -*- lexical-binding: t; -*-

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

;; The twin of the hour view of the Python Schedule tab.  The weekly table
;; lists blocks by their times; this dashboard turns it on its side, with
;; one row for each hour of the day and one column for each day, so the
;; shape of the week, its free hours, and its breaks can be seen at once.
;;
;; A block is named in the hour it starts, with its activity letter, its
;; project code, and its times, such as gA 05:00-06:30, and each further
;; hour it fills shows only a down arrow, as in the Python hour view, so the
;; grid stays quiet; the colour says whose block it is.  Each project keeps one colour, and
;; an hour with no block shows a dot.  The last row gives the planned
;; minutes of each day.  RET on a block jumps to its cell in the table.
;;
;; Commands, run from a buffer that shows a weekly table:
;;   `writing-habit-hours'   the hour-by-day dashboard of the week

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-table)

(declare-function writing-habit-table-current "writing-habit-table-mode")
(declare-function writing-habit-table--goto "writing-habit-table-mode" (model row &optional cell))

(defgroup writing-habit-hours nil
  "The week's blocks by the hour."
  :group 'writing-habit
  :prefix "writing-habit-hours-")

(defcustom writing-habit-hours-cell-width 16
  "The width of a day's column on the hour dashboard."
  :type 'integer)

(defcustom writing-habit-hours-colors
  '("#dbe9f6" "#dcefdc" "#f6dfe9" "#fbe8cc" "#e6e0f3" "#d9f0ee" "#f3f0c9" "#efdcd3")
  "The background colours given to the projects, in the order of the legend."
  :type '(repeat color))

(defun writing-habit-hours--clock (minutes)
  "Return MINUTES after midnight as HH:MM."
  (format "%02d:%02d" (/ minutes 60) (% minutes 60)))

(defun writing-habit-hours--minutes (clock)
  "Return the minutes after midnight of CLOCK, HH:MM."
  (+ (* 60 (string-to-number (substring clock 0 2))) (string-to-number (substring clock 3 5))))

(defun writing-habit-hours-cells (blocks)
  "Return the hour grid of BLOCKS as (FIRST LAST CELLS).
FIRST and LAST are the first and last hours shown.  CELLS is a hash table
keyed by (HOUR . OFFSET) whose values are lists of (BLOCK . STARTS), where
STARTS is non-nil in the hour the block starts."
  (let ((cells (make-hash-table :test #'equal)) (first 24) (last 0))
    (dolist (b blocks)
      (let* ((start (writing-habit-hours--minutes (writing-habit-table-block-start b)))
             (end (writing-habit-hours--minutes (writing-habit-table-block-end b)))
             (end (if (<= end start) (+ end 1440) end))
             (hour (/ start 60)))
        (setq first (min first hour) last (max last (/ (1- end) 60)))
        (while (< (* hour 60) end)
          (when (< hour 24)
            (push (cons b (= hour (/ start 60)))
                  (gethash (cons hour (writing-habit-table-block-offset b)) cells)))
          (setq hour (1+ hour)))))
    (maphash (lambda (k v) (puthash k (nreverse v) cells)) cells)
    (list (if (> first last) 5 first) (if (> first last) 21 (min last 23)) cells)))

(defun writing-habit-hours--label (block starts)
  "Return the text for BLOCK in an hour.
It is the code and times when STARTS is non-nil, else an arrow."
  (let ((letter (or (cdr (assoc (writing-habit-table-block-category block)
                                writing-habit-table-category-letter))
                    "")))
    (if starts
        (format "%s%s %s-%s" letter (writing-habit-table-block-letter block)
                (writing-habit-table-block-start block) (writing-habit-table-block-end block))
      "↓")))

(defun writing-habit-hours--color (code codes)
  "Return the background colour of project CODE among CODES."
  (let ((i (or (cl-position code codes :test #'equal) 0)))
    (nth (% i (length writing-habit-hours-colors)) writing-habit-hours-colors)))

(defun writing-habit-hours-string (table)
  "Return the hour dashboard of TABLE as a propertized string."
  (let* ((blocks (writing-habit-table-blocks table))
         (columns (writing-habit-table-columns table))
         (codes (delete-dups (append (mapcar #'car (writing-habit-table-legend table))
                                     (mapcar #'writing-habit-table-block-letter blocks))))
         (grid (writing-habit-hours-cells blocks))
         (width writing-habit-hours-cell-width)
         (pad (lambda (s) (truncate-string-to-width s width nil ?\s "…"))))
    (with-temp-buffer
      (insert (propertize (format "The week by the hour%s\n"
                                  (if (writing-habit-table-path table)
                                      (format ", %s" (file-name-nondirectory (writing-habit-table-path table)))
                                    ""))
                          'face 'bold)
              (propertize "A block is named in the hour it starts and shows ↓ in the hours it fills.  · is a free hour.\n\n"
                          'face 'shadow))
      (insert (propertize (concat "      " (mapconcat (lambda (c) (funcall pad (nth 2 c))) columns " ") "\n")
                          'face 'bold))
      (cl-loop for hour from (nth 0 grid) to (nth 1 grid) do
               (insert (format "%02d:00 " hour))
               (dolist (col columns)
                 (let* ((here (gethash (cons hour (nth 1 col)) (nth 2 grid)))
                        (text (if here
                                  (mapconcat (lambda (h) (writing-habit-hours--label (car h) (cdr h))) here " ")
                                "·"))
                        (cell (funcall pad text)))
                   (if here
                       (let ((b (car (car here))))
                         (insert (propertize cell
                                             'face (list :background (writing-habit-hours--color
                                                                      (writing-habit-table-block-letter b) codes)
                                                         :foreground "black")
                                             'writing-habit-hours-block b
                                             'help-echo (format "%s %s-%s, %s"
                                                                (writing-habit-table-block-letter b)
                                                                (writing-habit-table-block-start b)
                                                                (writing-habit-table-block-end b)
                                                                (writing-habit-table-block-category b)))))
                     (insert (propertize cell 'face 'shadow))))
                 (insert " "))
               (delete-char -1)
               (insert "\n"))
      (insert (propertize (concat "Total " (mapconcat
                                           (lambda (c)
                                             (let ((m (apply #'+ (mapcar #'writing-habit-table-block-minutes
                                                                        (seq-filter (lambda (b) (= (writing-habit-table-block-offset b) (nth 1 c)))
                                                                                    blocks)))))
                                               (funcall pad (format "%d min" m))))
                                           columns " ")
                                  "\n")
                          'face 'bold))
      (let ((legend (writing-habit-table-legend table)))
        (when legend
          (insert "\n")
          (dolist (entry legend)
            (insert (propertize (format " %s " (car entry))
                                'face (list :background (writing-habit-hours--color (car entry) codes)
                                            :foreground "black"))
                    " " (or (nth 1 entry) "") "\n"))))
      (insert "\n" (propertize "RET jump to the block   g refresh   q quit\n" 'face 'shadow))
      (buffer-string))))

(defvar-local writing-habit-hours--week-buffer nil
  "The week buffer the hour dashboard shows.")

(defun writing-habit-hours-refresh ()
  "Draw the hour dashboard again from its week buffer."
  (interactive)
  (let* ((week (if (derived-mode-p 'writing-habit-hours-mode)
                   writing-habit-hours--week-buffer
                 (current-buffer)))
         (table (progn (unless (buffer-live-p week)
                         (user-error "The week buffer is gone; open it and run the command again"))
                       (with-current-buffer week (writing-habit-table-current))))
         (buffer (get-buffer-create "*writing-habit hours*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t) (line (line-number-at-pos)))
        (unless (derived-mode-p 'writing-habit-hours-mode) (writing-habit-hours-mode))
        (setq writing-habit-hours--week-buffer week)
        (erase-buffer)
        (insert (writing-habit-hours-string table))
        (goto-char (point-min))
        (forward-line (1- line))))
    buffer))

(defun writing-habit-hours-visit ()
  "Jump to the cell of the block at point in the week buffer."
  (interactive)
  (let ((block (get-text-property (point) 'writing-habit-hours-block))
        (week writing-habit-hours--week-buffer))
    (unless block (user-error "No block here"))
    (pop-to-buffer week)
    (writing-habit-table--goto (writing-habit-table-current)
                               (writing-habit-table-block-row block)
                               (writing-habit-table-block-column block))))

(defvar writing-habit-hours-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'writing-habit-hours-visit)
    (define-key map (kbd "g") #'writing-habit-hours-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap of `writing-habit-hours-mode'.")

(define-derived-mode writing-habit-hours-mode special-mode "WH-Hours"
  "The week's blocks by the hour.
\\{writing-habit-hours-mode-map}"
  (setq truncate-lines t))

;;;###autoload
(defun writing-habit-hours ()
  "Show the week in the current buffer as a grid of hours by day."
  (interactive)
  (let ((week (current-buffer)))
    (with-current-buffer (get-buffer-create "*writing-habit hours*")
      (unless (derived-mode-p 'writing-habit-hours-mode) (writing-habit-hours-mode))
      (setq writing-habit-hours--week-buffer week))
    (with-current-buffer week (writing-habit-hours-refresh))
    (pop-to-buffer "*writing-habit hours*")))

(provide 'writing-habit-hours)
;;; writing-habit-hours.el ends here
