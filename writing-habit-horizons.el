;;; writing-habit-horizons.el --- Go to the lists of the weeks and semesters ahead -*- lexical-binding: t; -*-

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

;; The Schedule tab of the Python editor chooses its project lists with
;; two rows of buttons.  The first row holds the weeks: Next week, the
;; seven weeks after it, and the grasscatcher.  The second row holds the
;; semesters: the current one, the carried-over and completed lists, and
;; the six semesters ahead.  Each button names its list and how many rows
;; it holds.
;;
;; This file gives the table menu the same two rows as two small menus.
;; In the week buffer, the table menu on C-c C-; offers
;;
;;   L  Weeks ahead       1 to 8 for Next week to Eight weeks out,
;;                        g for the grasscatcher
;;   F  Future semesters  c current, o carried over, d completed,
;;                        1 to 6 for the semesters ahead
;;
;; Each key moves point to the first row of that list, the way a button
;; of the Python editor shows its page, and the menu shows the number of
;; rows in each list.  A list that the file does not hold yet can be
;; added as an empty #+NAME: line at the end of the file.  The scheduler
;; reads only the first table, so a list there never reaches the plan.
;;
;; Lists are found by their #+NAME: line, so the one-cell layout and the
;; layout in columns are both found.  The name is compared after
;; `writing-habit-lists-normalize-title', so a list named by its title,
;; such as "Next week", is found too.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'writing-habit-lists)

(defconst writing-habit-horizons-weeks
  '(("next week" . "Next week")
    ("two weeks out" . "Two weeks out")
    ("three weeks out" . "Three weeks out")
    ("four weeks out" . "Four weeks out")
    ("five weeks out" . "Five weeks out")
    ("six weeks out" . "Six weeks out")
    ("seven weeks out" . "Seven weeks out")
    ("eight weeks out" . "Eight weeks out")
    ("grasscatcher" . "Grasscatcher"))
  "The lists of the week row, as (NAME . TITLE), in the Python order.")

(defconst writing-habit-horizons-semesters
  '(("current-semester-projects" . "Current semester projects")
    ("carried-over-projects" . "Carried over")
    ("completed-this-semester" . "Completed this semester")
    ("next-semester-projects" . "Next semester projects")
    ("next-next-semester-projects" . "Next next semester projects")
    ("next-next-next-semester-projects" . "3*Next semester projects")
    ("next-next-next-next-semester-projects" . "4*Next semester projects")
    ("next-next-next-next-next-semester-projects" . "5*Next semester projects")
    ("next-next-next-next-next-next-semester-projects" . "6*Next semester projects"))
  "The lists of the semester row, as (NAME . TITLE), in the Python order.")

(defun writing-habit-horizons-all ()
  "Return (NAME . TITLE) for every list of both rows."
  (append writing-habit-horizons-weeks writing-habit-horizons-semesters))

(defun writing-habit-horizons-title (name)
  "Return the title of list NAME, or NAME itself when it is not known."
  (or (cdr (assoc name (writing-habit-horizons-all))) name))

(defun writing-habit-horizons--normalize (text)
  "Return the list name that TEXT, a #+NAME: value, stands for."
  (let ((name (writing-habit-lists-normalize-title text)))
    (or (car (seq-find (lambda (pair) (string= (downcase (cdr pair)) name))
                       (writing-habit-horizons-all)))
        name)))

(defun writing-habit-horizons--name-of (line)
  "Return the list name of LINE when it is a #+NAME: line, or nil."
  (when (string-match writing-habit-lists--name-re line)
    (writing-habit-horizons--normalize (match-string 1 line))))

(defun writing-habit-horizons--line ()
  "Return the text of the line at point."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun writing-habit-horizons-find (name)
  "Return the start of the #+NAME: line of list NAME in this buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (when (equal (writing-habit-horizons--name-of (writing-habit-horizons--line)) name)
          (setq found (line-beginning-position)))
        (forward-line 1))
      found)))

(defun writing-habit-horizons--rule-p (line)
  "Return non-nil when LINE is a rule line of an org table."
  (string-match-p "\\`[ \t]*|[-+]" line))

(defun writing-habit-horizons-rows (name)
  "Return (POSITION . TEXT) for each project row of list NAME.
Return the symbol `missing' when this buffer holds no such list.  The
#+CAPTION: and other keyword lines after the name are skipped, and so is
a header row, which is the row above the first rule."
  (let ((at (writing-habit-horizons-find name)))
    (if (null at)
        'missing
      (save-excursion
        (goto-char at)
        (forward-line 1)
        (while (and (not (eobp))
                    (string-match-p "\\`[ \t]*#\\+" (writing-habit-horizons--line)))
          (forward-line 1))
        (let ((rows '()) (ruled nil))
          (while (and (not (eobp))
                      (string-match-p "\\`[ \t]*|" (writing-habit-horizons--line)))
            (let ((line (writing-habit-horizons--line)))
              (if (writing-habit-horizons--rule-p line)
                  (unless ruled
                    ;; The rows above the first rule are a header.
                    (setq ruled t rows '()))
                (push (cons (line-beginning-position) line) rows)))
            (forward-line 1))
          (seq-remove (lambda (row)
                        (string-match-p "\\`[ \t]*|[ \t|]*\\'" (cdr row)))
                      (nreverse rows)))))))

(defun writing-habit-horizons-count (name)
  "Return the number of rows of list NAME, or nil when the buffer lacks it."
  (let ((rows (writing-habit-horizons-rows name)))
    (unless (eq rows 'missing) (length rows))))

(defun writing-habit-horizons--source-buffer ()
  "Return the buffer the menu was opened from, or the current buffer."
  (if (and (boundp 'transient--original-buffer)
           (buffer-live-p transient--original-buffer))
      transient--original-buffer
    (current-buffer)))

(defun writing-habit-horizons-label (name)
  "Return the menu label of list NAME, with its count of rows."
  (with-current-buffer (writing-habit-horizons--source-buffer)
    (let ((count (writing-habit-horizons-count name))
          (title (writing-habit-horizons-title name)))
      (if count
          (format "%s (%d)" title count)
        (format "%s (not in this file)" title)))))

(defun writing-habit-horizons-add (name)
  "Add an empty list NAME at the end of this buffer and return its start.
Only the #+NAME: line and a #+CAPTION: line are written.  The first
project typed below them starts the table."
  (save-excursion
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (unless (save-excursion (forward-line -1) (looking-at-p "[ \t]*$"))
      (insert "\n"))
    (let ((start (point)))
      (insert (format "#+NAME: %s\n#+CAPTION: %s\n" name
                      (writing-habit-horizons-title name)))
      start)))

(defun writing-habit-horizons--read-name ()
  "Read the title of a list and return its name."
  (let* ((choices (mapcar (lambda (pair) (cons (cdr pair) (car pair)))
                          (writing-habit-horizons-all)))
         (title (completing-read "List: " choices nil t)))
    (cdr (assoc title choices))))

;;;###autoload
(defun writing-habit-horizons-goto (name)
  "Move point to the first row of list NAME of the week's file.
NAME is a list of the weeks ahead, the grasscatcher, or a semester list.
When the file holds no such list, offer to add an empty one at the end
of the file."
  (interactive (list (writing-habit-horizons--read-name)))
  (let ((title (writing-habit-horizons-title name))
        (at (writing-habit-horizons-find name)))
    (unless at
      (unless (y-or-n-p (format "This file holds no %s list.  Add an empty one at the end? "
                                title))
        (user-error "No %s list in this file" title))
      (setq at (writing-habit-horizons-add name)))
    (push-mark nil t)
    (let ((rows (writing-habit-horizons-rows name)))
      (if (consp rows)
          (progn (goto-char (car (car rows)))
                 (when (search-forward "|" (line-end-position) t)
                   (skip-chars-forward " " (line-end-position))))
        ;; An empty list: put point on the line under its keyword lines.
        (goto-char at)
        (forward-line 1)
        (while (and (not (eobp)) (looking-at-p "[ \t]*#\\+"))
          (forward-line 1)))
      (when (fboundp 'org-fold-show-context) (org-fold-show-context))
      (when (eq (window-buffer) (current-buffer)) (recenter 0))
      (message "%s: %s" title
               (pcase (if (consp rows) (length rows) 0)
                 (0 "no projects yet")
                 (1 "1 row")
                 (n (format "%d rows" n)))))))

(defmacro writing-habit-horizons--define-goto (symbol name)
  "Define SYMBOL as a command to go to list NAME."
  `(defun ,symbol ()
     ,(format "Go to the list %s." name)
     (interactive)
     (writing-habit-horizons-goto ,name)))

(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-1 "next week")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-2 "two weeks out")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-3 "three weeks out")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-4 "four weeks out")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-5 "five weeks out")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-6 "six weeks out")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-7 "seven weeks out")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-week-8 "eight weeks out")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-grasscatcher "grasscatcher")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-current
                                     "current-semester-projects")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-carried
                                     "carried-over-projects")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-completed
                                     "completed-this-semester")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-semester-1
                                     "next-semester-projects")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-semester-2
                                     "next-next-semester-projects")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-semester-3
                                     "next-next-next-semester-projects")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-semester-4
                                     "next-next-next-next-semester-projects")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-semester-5
                                     "next-next-next-next-next-semester-projects")
(writing-habit-horizons--define-goto writing-habit-horizons-goto-semester-6
                                     "next-next-next-next-next-next-semester-projects")

;;;###autoload (autoload 'writing-habit-horizons-weeks-menu "writing-habit-horizons" nil t)
(transient-define-prefix writing-habit-horizons-weeks-menu ()
  "Go to a list of the projects queued for the weeks ahead."
  ["Weeks ahead"
   ["The next four weeks"
    ("1" writing-habit-horizons-goto-week-1
     :description (lambda () (writing-habit-horizons-label "next week")))
    ("2" writing-habit-horizons-goto-week-2
     :description (lambda () (writing-habit-horizons-label "two weeks out")))
    ("3" writing-habit-horizons-goto-week-3
     :description (lambda () (writing-habit-horizons-label "three weeks out")))
    ("4" writing-habit-horizons-goto-week-4
     :description (lambda () (writing-habit-horizons-label "four weeks out")))]
   ["The four after them"
    ("5" writing-habit-horizons-goto-week-5
     :description (lambda () (writing-habit-horizons-label "five weeks out")))
    ("6" writing-habit-horizons-goto-week-6
     :description (lambda () (writing-habit-horizons-label "six weeks out")))
    ("7" writing-habit-horizons-goto-week-7
     :description (lambda () (writing-habit-horizons-label "seven weeks out")))
    ("8" writing-habit-horizons-goto-week-8
     :description (lambda () (writing-habit-horizons-label "eight weeks out")))]
   ["Tasks"
    ("g" writing-habit-horizons-goto-grasscatcher
     :description (lambda () (writing-habit-horizons-label "grasscatcher")))]])

;;;###autoload (autoload 'writing-habit-horizons-semesters-menu "writing-habit-horizons" nil t)
(transient-define-prefix writing-habit-horizons-semesters-menu ()
  "Go to a list of the projects of this semester or of the semesters ahead."
  ["Semesters"
   ["This semester"
    ("c" writing-habit-horizons-goto-current
     :description (lambda () (writing-habit-horizons-label "current-semester-projects")))
    ("o" writing-habit-horizons-goto-carried
     :description (lambda () (writing-habit-horizons-label "carried-over-projects")))
    ("d" writing-habit-horizons-goto-completed
     :description (lambda () (writing-habit-horizons-label "completed-this-semester")))]
   ["The semesters ahead"
    ("1" writing-habit-horizons-goto-semester-1
     :description (lambda () (writing-habit-horizons-label "next-semester-projects")))
    ("2" writing-habit-horizons-goto-semester-2
     :description (lambda () (writing-habit-horizons-label "next-next-semester-projects")))
    ("3" writing-habit-horizons-goto-semester-3
     :description (lambda () (writing-habit-horizons-label
                              "next-next-next-semester-projects")))
    ("4" writing-habit-horizons-goto-semester-4
     :description (lambda () (writing-habit-horizons-label
                              "next-next-next-next-semester-projects")))
    ("5" writing-habit-horizons-goto-semester-5
     :description (lambda () (writing-habit-horizons-label
                              "next-next-next-next-next-semester-projects")))
    ("6" writing-habit-horizons-goto-semester-6
     :description (lambda () (writing-habit-horizons-label
                              "next-next-next-next-next-next-semester-projects")))]])

(provide 'writing-habit-horizons)
;;; writing-habit-horizons.el ends here
