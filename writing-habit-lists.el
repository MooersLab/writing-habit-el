;;; writing-habit-lists.el --- Semester project lists beside the weekly table -*- lexical-binding: t; -*-

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

;; A weekly table file may carry up to six lists of projects after the
;; weekly table itself: the projects finished this semester, and one list
;; per planning horizon, about two years ahead.  Each list is its own small
;; org table, named by an org #+NAME: line:
;;
;;   #+NAME: current-semester-projects
;;   | B: 1006AIrxOpt 2026-12-15 :safe: |
;;   | - 1002RNAeditingR01 2027-02-05 :safe: |
;;
;; A row has the one-cell form of a legend row, and a dash stands for a
;; project that has no code yet.  The scheduler reads only the first table
;; of a file, so the lists never reach plan import, the calendar, or the
;; sheets.  This file is a port of `project_lists.py'.  Every function
;; takes the list of lines after the weekly table and returns new lines,
;; leaving its argument alone.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-name)

(defconst writing-habit-lists
  '(("current-semester-projects" . "Current semester projects")
    ("completed-this-semester" . "Completed this semester")
    ("next-semester-projects" . "Next semester projects")
    ("next-next-semester-projects" . "Next next semester projects")
    ("next-next-next-semester-projects" . "3*Next semester projects")
    ("next-next-next-next-semester-projects" . "4*Next semester projects"))
  "Each list as (NAME . TITLE): the name in the file and the title shown.")

(defconst writing-habit-lists-current "current-semester-projects"
  "The list of the projects of this semester.")

(defconst writing-habit-lists-completed "completed-this-semester"
  "The list of the projects finished this semester.")

(defconst writing-habit-lists--name-re "\\`[ \t]*#\\+NAME:[ \t]*\\([^ \t]+\\)[ \t]*\\'"
  "An org #+NAME: line, with the name in group 1.")

(defconst writing-habit-lists--risk-tag '(("safe" . "safe") ("speculative" . "risky"))
  "The tag written for each risk class.")

(defun writing-habit-lists-names ()
  "Return the list names in horizon order."
  (mapcar #'car writing-habit-lists))

;;;; Entries

(cl-defstruct (writing-habit-lists-entry
               (:constructor writing-habit-lists-entry-create)
               (:copier writing-habit-lists-entry-copy))
  "One project in a list.  CODE is nil while no code is assigned.
RISK is a class name, safe or speculative, and ACTIVITY a default activity."
  code description risk activity)

(defun writing-habit-lists-entry-project (entry)
  "Return the project name of ENTRY, without its deadline."
  (car (writing-habit-name-split-deadline (writing-habit-lists-entry-description entry))))

(defun writing-habit-lists-entry-deadline (entry)
  "Return the deadline of ENTRY as YYYY-MM-DD, or nil."
  (cdr (writing-habit-name-split-deadline (writing-habit-lists-entry-description entry))))

(defun writing-habit-lists-entry-text (entry)
  "Return the first cell of the row for ENTRY, in the legend's own form."
  (let ((tag (cdr (assoc (or (writing-habit-lists-entry-risk entry) "")
                         writing-habit-lists--risk-tag)))
        (activity (writing-habit-lists-entry-activity entry)))
    (concat (string-trim-right
             (format "%s %s"
                     (if (writing-habit-lists-entry-code entry)
                         (concat (writing-habit-lists-entry-code entry) ":")
                       "-")
                     (string-trim (writing-habit-lists-entry-description entry))))
            (if activity (format " @%s" activity) "")
            (if tag (format " :%s:" tag) ""))))

(defun writing-habit-lists-parse-entry (cell)
  "Return the entry the first CELL of a row names, or nil."
  (let ((cell (string-trim (or cell ""))))
    (unless (string-empty-p cell)
      (let* ((dash (and (string-prefix-p "-" cell) (not (string-prefix-p "--" cell))))
             (probe (if dash (concat "X: " (string-trim (substring cell 1))) cell))
             (parsed (writing-habit-name-parse-legend-cell probe)))
        (when parsed
          (writing-habit-lists-entry-create
           :code (unless dash (nth 0 parsed))
           :description (nth 1 parsed)
           :risk (nth 2 parsed)
           :activity (writing-habit-name-legend-activity probe)))))))

(defun writing-habit-lists--cells (line)
  "Return the cells of the table LINE, untrimmed."
  (let ((inner (string-trim line)))
    (when (string-prefix-p "|" inner) (setq inner (substring inner 1)))
    (when (string-suffix-p "|" inner) (setq inner (substring inner 0 -1)))
    (split-string inner "|")))

(defun writing-habit-lists--with-first-cell (line text)
  "Return LINE with its first cell holding TEXT, keeping the cell's width."
  (let* ((lead (progn (string-match "\\`[ \t]*" line) (match-string 0 line)))
         (cells (writing-habit-lists--cells line))
         (width (max (- (length (car cells)) 2) 0)))
    (setcar cells (concat " " (if (<= (length text) width)
                                  (concat text (make-string (- width (length text)) ?\s))
                                text)
                          " "))
    (concat lead "|" (mapconcat #'identity cells "|") "|")))

(defun writing-habit-lists--rule-p (line)
  "Return non-nil when LINE is a table rule such as |---+---|."
  (string-match-p "\\`[ \t]*|[-+]" line))

;;;; Finding

(defun writing-habit-lists--name-line (lines name)
  "Return the index in LINES of the #+NAME: line of list NAME, or nil."
  (cl-position-if (lambda (line)
                    (and (string-match writing-habit-lists--name-re line)
                         (string= (downcase (match-string 1 line)) name)))
                  lines))

(defun writing-habit-lists--span (lines name)
  "Return (FIRST . END) line indexes of list NAME in LINES, or nil.
FIRST is the first table line and END is one past the last."
  (let ((at (writing-habit-lists--name-line lines name)))
    (when at
      (let ((start (1+ at)) (n (length lines)))
        (while (and (< start n) (string-prefix-p "#+" (string-trim-left (nth start lines))))
          (setq start (1+ start)))
        (let ((end start))
          (while (and (< end n) (string-prefix-p "|" (string-trim-left (nth end lines))))
            (setq end (1+ end)))
          (cons start end))))))

(defun writing-habit-lists-present (lines)
  "Return the names of the lists LINES carries, in horizon order."
  (seq-filter (lambda (name) (writing-habit-lists--span lines name))
              (writing-habit-lists-names)))

(defun writing-habit-lists-row-indexes (lines name)
  "Return the line indexes in LINES of the project rows of list NAME."
  (let ((span (writing-habit-lists--span lines name)) (out '()))
    (when span
      (cl-loop for i from (car span) below (cdr span)
               for line = (nth i lines)
               unless (or (writing-habit-lists--rule-p line)
                          (null (writing-habit-lists-parse-entry
                                 (car (writing-habit-lists--cells line)))))
               do (push i out)))
    (nreverse out)))

(defun writing-habit-lists-entries (lines name)
  "Return the entries of list NAME in LINES, in order."
  (mapcar (lambda (i) (writing-habit-lists-parse-entry
                       (car (writing-habit-lists--cells (nth i lines)))))
          (writing-habit-lists-row-indexes lines name)))

(defun writing-habit-lists-find-project (lines name project)
  "Return the position of PROJECT in list NAME of LINES, or nil."
  (cl-position project (writing-habit-lists-entries lines name)
               :key #'writing-habit-lists-entry-project :test #'equal))

;;;; Editing

(defun writing-habit-lists-set-entry (lines name position entry)
  "Return LINES with row POSITION of list NAME rewritten to ENTRY."
  (let* ((lines (copy-sequence lines))
         (index (nth position (writing-habit-lists-row-indexes lines name))))
    (setf (nth index lines)
          (writing-habit-lists--with-first-cell (nth index lines)
                                                (writing-habit-lists-entry-text entry)))
    lines))

(defun writing-habit-lists--create (lines name)
  "Return (LINES . SLOT) with an empty list NAME added in horizon order.
SLOT is the index where its first row goes."
  (let* ((names (writing-habit-lists-names))
         (order (cl-position name names :test #'string=))
         (earlier (delq nil (mapcar (lambda (n) (writing-habit-lists--span lines n))
                                    (seq-take names order))))
         (later (delq nil (mapcar (lambda (n) (writing-habit-lists--name-line lines n))
                                  (nthcdr (1+ order) names))))
         (at (length lines)))
    (while (and (> at 0) (string-empty-p (string-trim (nth (1- at) lines))))
      (setq at (1- at)))
    (cond (earlier (setq at (cdr (car (last earlier)))))
          (later (setq at (apply #'min later))
                 (while (and (> at 0) (string-empty-p (string-trim (nth (1- at) lines))))
                   (setq at (1- at)))))
    (let ((block (list "" (format "#+NAME: %s" name))))
      (cons (append (seq-take lines at) block (nthcdr at lines))
            (+ at (length block))))))

(defun writing-habit-lists-insert (lines name position entry)
  "Return (LINES . POSITION) with ENTRY inserted before POSITION in list NAME.
A nil POSITION, or one past the end, appends ENTRY."
  (let* ((rows (writing-habit-lists-row-indexes lines name))
         (span (writing-habit-lists--span lines name))
         at shape)
    (if (null span)
        (let ((made (writing-habit-lists--create lines name)))
          (setq lines (car made) at (cdr made)))
      (setq at (cond ((and position (< position (length rows))) (nth position rows))
                     (rows (1+ (car (last rows))))
                     (t (cdr span)))
            shape (and rows (nth (car (last rows)) lines))))
    (let ((line (if shape
                    (let* ((filled (writing-habit-lists--with-first-cell
                                    shape (writing-habit-lists-entry-text entry)))
                           (lead (progn (string-match "\\`[ \t]*" filled) (match-string 0 filled)))
                           (cells (writing-habit-lists--cells filled)))
                      ;; A fresh row copies the sibling's cells but none of their values.
                      (concat lead "|"
                              (mapconcat #'identity
                                         (cons (car cells)
                                               (mapcar (lambda (c) (make-string (length c) ?\s))
                                                       (cdr cells)))
                                         "|")
                              "|"))
                  (format "| %s |" (writing-habit-lists-entry-text entry)))))
      (setq lines (append (seq-take lines at) (list line) (nthcdr at lines)))
      (cons lines (cl-position at (writing-habit-lists-row-indexes lines name))))))

(defun writing-habit-lists-remove (lines name position)
  "Return (LINES . ENTRY) with row POSITION of list NAME removed."
  (let* ((index (nth position (writing-habit-lists-row-indexes lines name)))
         (entry (writing-habit-lists-parse-entry
                 (car (writing-habit-lists--cells (nth index lines))))))
    (cons (append (seq-take lines index) (nthcdr (1+ index) lines)) entry)))

(defun writing-habit-lists-move (lines name position up)
  "Return (LINES . POSITION) with row POSITION of list NAME moved UP or down."
  (let* ((rows (writing-habit-lists-row-indexes lines name))
         (other (if up (1- position) (1+ position))))
    (if (not (and (>= other 0) (< other (length rows))))
        (cons lines position)
      (let ((lines (copy-sequence lines))
            (a (nth position rows)) (b (nth other rows)))
        (cl-rotatef (nth a lines) (nth b lines))
        (cons lines other)))))

(defun writing-habit-lists-sort (lines name)
  "Return (LINES . CHANGED) with list NAME ordered by project number.
The sort is stable, so unnumbered projects, such as email, keep their
order after the numbered ones."
  (let* ((rows (writing-habit-lists-row-indexes lines name))
         (current (mapcar (lambda (i) (nth i lines)) rows))
         (ordered (seq-sort-by
                   (lambda (line)
                     (writing-habit-name-number-key
                      (writing-habit-lists-entry-project
                       (writing-habit-lists-parse-entry (car (writing-habit-lists--cells line))))))
                   #'writing-habit-name-key< current)))
    (if (equal ordered current)
        (cons lines nil)
      (let ((lines (copy-sequence lines)))
        (cl-mapc (lambda (i line) (setf (nth i lines) line)) rows ordered)
        (cons lines t)))))

(defun writing-habit-lists-transfer (lines source position target)
  "Return (LINES . ENTRY) with row POSITION moved from list SOURCE to TARGET.
The entry goes to the end of TARGET, unless TARGET already names the
project, in which case it only leaves SOURCE."
  (let* ((removed (writing-habit-lists-remove lines source position))
         (lines (car removed))
         (entry (cdr removed)))
    (unless (writing-habit-lists-find-project lines target
                                              (writing-habit-lists-entry-project entry))
      (setq lines (car (writing-habit-lists-insert lines target nil entry))))
    (cons lines entry)))

(defun writing-habit-lists-semester-numbers (lines)
  "Return the project numbers on this semester's lists in LINES.
A project counts while it is on the current list and after it moves to
the completed list."
  (let ((found '()))
    (dolist (name (list writing-habit-lists-current writing-habit-lists-completed))
      (dolist (entry (writing-habit-lists-entries lines name))
        (dolist (token (writing-habit-name--project-tokens
                        (concat " " (writing-habit-lists-entry-description entry))))
          (cl-pushnew (substring token 0 4) found :test #'string=))))
    (nreverse found)))

(defun writing-habit-lists-after-first-table (text)
  "Return the lines of TEXT after its first org table."
  (let ((lines (split-string text "\n")) (seen nil) (out nil) (i 0))
    (catch 'done
      (dolist (line lines)
        (if (string-prefix-p "|" (string-trim-left line))
            (setq seen t)
          (when seen (setq out (nthcdr i lines)) (throw 'done nil)))
        (setq i (1+ i))))
    out))

(provide 'writing-habit-lists)
;;; writing-habit-lists.el ends here
