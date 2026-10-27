;;; writing-habit-columns.el --- Project tables kept in columns -*- lexical-binding: t; -*-

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

;; A weekly table file may keep its legend and its project lists as
;; tables with one column per field:
;;
;;   #+NAME: This week
;;   | Code | Project Number | Project Name | Deadline   | Risk tag | Default activity |
;;   |------+----------------+--------------+------------+----------+------------------|
;;   | B:   | 1006           | AIrxOpt      | 2026-12-15 | :safe:   |                  |
;;   | -    | 1452           | PHFSboXt     | 2027-03-01 | :safe:   | editing          |
;;
;; The grasscatcher has Code, Project Number, Project Name, Priority, and
;; Task.  The rest of the package works on the one-cell form of a row,
;; such as "B: 1006AIrxOpt 2026-12-15 :safe:".  This file turns a row of
;; columns into that cell and back, so the editor and the readers keep one
;; model whichever layout the file uses.  It is a port of `columns.py'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-name)
(require 'writing-habit-lists)

(defconst writing-habit-columns-project
  '("Code" "Project Number" "Project Name" "Deadline" "Risk tag" "Default activity")
  "The columns of a project table.")

(defconst writing-habit-columns-task
  '("Code" "Project Number" "Project Name" "Priority" "Task")
  "The columns of the grasscatcher.")

(defconst writing-habit-columns-legend-name "This week"
  "The name of the table that holds the week's legend in columns.")

(defconst writing-habit-columns-legend-slot (string 0 ?t ?h ?i ?s ?- ?w ?e ?e ?k)
  "A line that stands, among the lines after the week, for the This week table.")

(defconst writing-habit-columns-extra-lists
  '(("carried-over-projects" . "Carried over") ("grasscatcher" . "Grasscatcher")
    ("next week" . "Next week") ("two weeks out" . "Two weeks out")
    ("three weeks out" . "Three weeks out") ("four weeks out" . "Four weeks out")
    ("five weeks out" . "Five weeks out") ("six weeks out" . "Six weeks out")
    ("seven weeks out" . "Seven weeks out") ("eight weeks out" . "Eight weeks out"))
  "Lists the Python editor keeps that are written in columns here too.
The eight week lists hold the projects waiting for each of the next
eight weeks.")

(defconst writing-habit-columns--risk
  '(("safe" . "safe") ("risky" . "risky") ("speculative" . "risky"))
  "Risk words a Risk tag column may hold, and the tag each is written as.")

(defconst writing-habit-columns--activity
  '(("generative" . "generative") ("editing" . "editing") ("support" . "support")
    ("g" . "generative") ("e" . "editing") ("s" . "support"))
  "Words a Default activity column may hold, and the activity each names.")

(defun writing-habit-columns-cells (line)
  "Return the trimmed cells of the org table LINE."
  (mapcar #'string-trim (writing-habit-lists--cells line)))

(defun writing-habit-columns--rule-p (line)
  "Return non-nil when LINE is a table rule."
  (string-match-p "\\`[ \t]*|[-+]" line))

(defun writing-habit-columns-header-p (row)
  "Return non-nil when ROW, a list of cells, is a Code header row."
  (and row (>= (length row) 3)
       (string-prefix-p "code" (downcase (string-trim (car row))))))

(defun writing-habit-columns-task-header-p (row)
  "Return non-nil when ROW is the header of a grasscatcher in columns."
  (and (writing-habit-columns-header-p row)
       (seq-some (lambda (c) (string= (downcase (string-trim c)) "task")) row)))

(defun writing-habit-columns-split-project (project)
  "Split PROJECT such as 1006AIrxOpt into (\"1006\" . \"AIrxOpt\").
A project without a leading four-digit number gives (\"\" . PROJECT)."
  (let ((project (string-trim (or project ""))))
    (if (string-match "\\`\\([0-9]\\{4\\}\\)\\(.*\\)\\'" project)
        (cons (match-string 1 project) (string-trim (match-string 2 project)))
      (cons "" project))))

(defun writing-habit-columns--pad (row n)
  "Return ROW padded with empty strings, or cut, to N cells."
  (seq-take (append row (make-list n "")) n))

(defun writing-habit-columns--head (code)
  "Return CODE as the start of a one-cell row: \"B:\", or \"-\" when empty."
  (let ((code (string-trim (or code ""))))
    (cond ((member code '("" "-")) "-")
          ((let ((case-fold-search nil))
             (string-match "\\`\\([A-Za-z0-9]\\{1,2\\}\\)[ \t]*:?\\'" code))
           (concat (upcase (match-string 1 code)) ":")))))

;;;; Columns to the one-cell form

(defun writing-habit-columns-project-cell (row)
  "Return the one-cell form of the project ROW, or nil when it is empty."
  (pcase-let ((`(,code ,number ,name ,deadline ,risk ,activity)
               (mapcar #'string-trim (writing-habit-columns--pad row 6))))
    (let ((head (writing-habit-columns--head code)))
      (when (and head (not (and (string-empty-p number) (string-empty-p name))))
        (let ((text (string-trim-right (concat head " " number name)))
              (word (cdr (assoc (downcase (string-trim (string-remove-prefix "@" activity)))
                                writing-habit-columns--activity)))
              (tag (cdr (assoc (downcase (replace-regexp-in-string "[:() ]" "" risk))
                               writing-habit-columns--risk))))
          (setq deadline (replace-regexp-in-string "[ \t]+" "" deadline))
          (unless (string-empty-p deadline) (setq text (concat text " " deadline)))
          (when word (setq text (concat text " @" word)))
          (when tag (setq text (concat text " :" tag ":")))
          text)))))

(defun writing-habit-columns-task-cell (row)
  "Return the one-cell form of the grasscatcher ROW, or nil when it is empty.
The Emacs model keeps the task without its project number, and
`writing-habit-columns-task-columns' writes the number back from the code,
or from a number the Python editor wrote after the code."
  (pcase-let ((`(,code ,_number ,_name ,priority ,task)
               (mapcar #'string-trim (writing-habit-columns--pad row 5))))
    (let ((head (writing-habit-columns--head code)))
      (when (and head (not (string-empty-p task)))
        (unless (or (string-empty-p priority)
                    (string-match-p "\\`\\[#\\([A-Za-z][0-9]*\\|[0-9]+\\)\\]\\'" priority))
          (setq priority (format "[#%s]" (string-trim priority "[[#]+" "[]]+"))))
        (string-trim (concat head " " (if (string-empty-p priority) "" (concat priority " "))
                             task))))))

;;;; The one-cell form to columns

(defun writing-habit-columns-project-columns (cell)
  "Return the six columns of the project row given in the one-cell CELL."
  (let ((entry (writing-habit-lists-parse-entry cell)))
    (if (null entry)
        (list (string-trim cell) "" "" "" "" "")
      (pcase-let* ((`(,project . ,deadline)
                    (writing-habit-name-split-deadline
                     (writing-habit-lists-entry-description entry)))
                   (`(,number . ,name) (writing-habit-columns-split-project project)))
        (list (if (writing-habit-lists-entry-code entry)
                  (concat (writing-habit-lists-entry-code entry) ":")
                "-")
              number name (or deadline "")
              (pcase (writing-habit-lists-entry-risk entry)
                ("safe" ":safe:") ("speculative" ":risky:") (_ ""))
              (or (writing-habit-lists-entry-activity entry) ""))))))

(defun writing-habit-columns-task-columns (cell projects)
  "Return the five columns of the grasscatcher row given in the one-cell CELL.
PROJECTS is an alist of (CODE . (NUMBER . NAME)) naming each code's project."
  (let ((entry (writing-habit-lists-parse-entry cell)))
    (if (null entry)
        (list "-" "" "" "" (string-trim cell))
      (let* ((text (writing-habit-lists-entry-description entry))
             (named (and (string-match "\\`\\([0-9]\\{4\\}\\)[ \t]+" text)
                         (seq-find (lambda (p) (equal (cadr p) (match-string 1 text)))
                                   projects)))
             (_ (when named (setq text (substring text (match-end 0)))))
             (priority (and (string-match "\\`\\[#\\([A-Za-z][0-9]*\\|[0-9]+\\)\\][ \t]*" text)
                            (prog1 (match-string 1 text)
                              (setq text (substring text (match-end 0))))))
             (code (writing-habit-lists-entry-code entry))
             (project (if named (cdr named) (cdr (assoc (or code "") projects)))))
        (list (if code (concat code ":") "-")
              (or (car project) "") (or (cdr project) "")
              (if priority (format "[#%s]" (upcase priority)) "")
              (string-trim text))))))

(defun writing-habit-columns-render (header rows)
  "Return the lines of an aligned org table with HEADER and ROWS."
  (let* ((table (cons header rows))
         (widths (cl-loop for i below (length header)
                          collect (apply #'max (mapcar (lambda (r) (length (nth i r))) table))))
         (line (lambda (r)
                 (concat "| "
                         (mapconcat #'identity
                                    (cl-loop for c in r for w in widths
                                             collect (concat c (make-string (- w (length c)) ?\s)))
                                    " | ")
                         " |")))
         (rule (concat "|" (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "+") "|")))
    (append (list (funcall line header) rule) (mapcar line rows))))

;;;; Whole tables

(defun writing-habit-columns-read-table (lines)
  "Read the table LINES.  Return (COLUMNAR . ROWS) with ROWS in one-cell form.
A table whose first row is a Code header is read by its columns, and any
other table by its first cell."
  (let ((rows (mapcar #'writing-habit-columns-cells
                      (seq-filter (lambda (l) (and (string-prefix-p "|" (string-trim-left l))
                                                   (not (writing-habit-columns--rule-p l))))
                                  lines))))
    (if (not (and rows (writing-habit-columns-header-p (car rows))))
        (cons nil (delq nil (mapcar (lambda (r) (and r (not (string-empty-p (car r))) (car r)))
                                    rows)))
      (let ((convert (if (writing-habit-columns-task-header-p (car rows))
                         #'writing-habit-columns-task-cell
                       #'writing-habit-columns-project-cell)))
        (cons t (delq nil (mapcar convert (cdr rows))))))))

(defun writing-habit-columns-task-projects (lines)
  "Return (CODE . (NUMBER . NAME)) for each code a grasscatcher in LINES names."
  (let ((rows (mapcar #'writing-habit-columns-cells
                      (seq-filter (lambda (l) (and (string-prefix-p "|" (string-trim-left l))
                                                   (not (writing-habit-columns--rule-p l))))
                                  lines)))
        (out '()))
    (when (and rows (writing-habit-columns-task-header-p (car rows)))
      (dolist (row (cdr rows))
        (let* ((row (writing-habit-columns--pad row 5))
               (head (writing-habit-columns--head (nth 0 row))))
          (when (and head (not (string= head "-")))
            (let ((code (substring head 0 -1)))
              (unless (assoc code out)
                (push (cons code (cons (nth 1 row) (nth 2 row))) out)))))))
    (nreverse out)))

(defun writing-habit-columns--name (line)
  "Return the name on the #+NAME: LINE, or nil."
  (let ((case-fold-search t))
    (when (string-match "\\`[ \t]*#\\+NAME:[ \t]*\\(.*?\\)[ \t]*\\'" line)
      (match-string 1 line))))

(defun writing-habit-columns-normalize (name)
  "Return NAME in lower case with its spaces collapsed."
  (downcase (string-trim (replace-regexp-in-string "[ \t]+" " " (or name "")))))

(defun writing-habit-columns--table-end (lines index)
  "Return (START . END) of the table named on line INDEX of LINES.
START is the first table line after any #+ lines, END one past the last."
  (let ((n (length lines)) (start (1+ index)))
    (while (and (< start n)
                (string-prefix-p "#+" (string-trim-left (nth start lines)))
                (not (writing-habit-columns--name (nth start lines))))
      (setq start (1+ start)))
    (let ((end start))
      (while (and (< end n) (string-prefix-p "|" (string-trim-left (nth end lines))))
        (setq end (1+ end)))
      (cons start end))))

(defun writing-habit-columns-to-one-cell (lines &optional first-line)
  "Read every table of LINES kept in columns into the one-cell form.
Return a plist with keys :lines, :sources, :legend, :legend-sources,
:tasks, and :found.  :lines has each named table in columns rewritten as
one-cell rows, and the This week table replaced by
`writing-habit-columns-legend-slot'.  :sources gives, for each line of
:lines, the index of the line of LINES it came from, offset by
FIRST-LINE, or nil.  :legend holds the This week rows in one-cell form,
and :legend-sources their line indexes.  :tasks is an alist of
\(CODE . (NUMBER . NAME)) from the grasscatcher, and :found says whether
any table was in columns."
  (let ((vec (vconcat lines)) (first-line (or first-line 0))
        (out '()) (sources '()) (legend nil) (legend-sources nil)
        (tasks '()) (found nil) (index 0))
    (while (< index (length vec))
      (let* ((line (aref vec index)) (name (writing-habit-columns--name line)))
        (if (not name)
            (progn (push line out) (push (+ first-line index) sources)
                   (setq index (1+ index)))
          (pcase-let* ((`(,start . ,end) (writing-habit-columns--table-end lines index))
                       (block (append (seq-subseq vec start end) nil))
                       (`(,columnar . ,rows) (writing-habit-columns-read-table block)))
            (if (or (not columnar)
                    ;; A table this package does not keep stays exactly as
                    ;; written, because it could not be written back in its
                    ;; own columns.
                    (not (or (assoc (writing-habit-columns-normalize name)
                                    (writing-habit-columns--list-names))
                             (string= (writing-habit-columns-normalize name)
                                      (writing-habit-columns-normalize
                                       writing-habit-columns-legend-name)))))
                (cl-loop for i from index below end
                         do (push (aref vec i) out) (push (+ first-line i) sources))
              (setq found t)
              (let ((row-lines (cl-loop for i from start below end
                                        for l = (aref vec i)
                                        when (and (not (writing-habit-columns--rule-p l))
                                                  (not (writing-habit-columns-header-p
                                                        (writing-habit-columns-cells l))))
                                        collect (+ first-line i))))
                (if (and (null legend)
                         (string= (writing-habit-columns-normalize name)
                                  (writing-habit-columns-normalize
                                   writing-habit-columns-legend-name)))
                    (progn (setq legend rows legend-sources row-lines)
                           (push writing-habit-columns-legend-slot out)
                           (push (+ first-line index) sources))
                  (setq tasks (append tasks (writing-habit-columns-task-projects block)))
                  (cl-loop for i from index below start
                           do (push (aref vec i) out) (push (+ first-line i) sources))
                  (cl-loop for row in rows
                           for src = (pop row-lines)
                           do (push (format "| %s |" row) out) (push src sources)))))
            (setq index end)))))
    (list :lines (nreverse out) :sources (nreverse sources)
          :legend legend :legend-sources legend-sources
          :tasks tasks :found found)))

(defun writing-habit-columns--list-names ()
  "Return every list name written in columns, with its plain title."
  (append writing-habit-lists writing-habit-columns-extra-lists))

(defun writing-habit-columns-to-columns (lines legend projects)
  "Return LINES with every project list written in columns.
LEGEND, the week's legend in one-cell form, is written as the This week
table at `writing-habit-columns-legend-slot', or at the top when LINES has
no slot.  PROJECTS is an alist of (CODE . (NUMBER . NAME)) for the
grasscatcher rows.  A list keeps the #+CAPTION: line it has, and a list
without one gets its plain title."
  (let* ((vec (vconcat lines))
         (names (writing-habit-columns--list-names))
         (legend-table (cons (concat "#+NAME: " writing-habit-columns-legend-name)
                             (writing-habit-columns-render
                              writing-habit-columns-project
                              (mapcar #'writing-habit-columns-project-columns legend))))
         (out '()) (placed nil) (index 0))
    (while (< index (length vec))
      (let* ((line (aref vec index))
             (name (and (writing-habit-columns--name line)
                        (writing-habit-columns-normalize (writing-habit-columns--name line))))
             (known (and name (assoc name names))))
        (cond
         ((equal line writing-habit-columns-legend-slot)
          (setq out (append (reverse legend-table) out) placed t index (1+ index)))
         ((not known) (push line out) (setq index (1+ index)))
         (t
          (pcase-let* ((`(,start . ,end) (writing-habit-columns--table-end lines index))
                       (meta (append (seq-subseq vec (1+ index) start) nil))
                       (rows (cl-loop for i from start below end
                                      for l = (aref vec i)
                                      for c = (car (writing-habit-columns-cells l))
                                      unless (or (writing-habit-columns--rule-p l)
                                                 (null c) (string-empty-p c))
                                      collect c)))
            (push (concat "#+NAME: " (car known)) out)
            (unless (seq-some (lambda (m) (string-prefix-p "#+caption:" (downcase (string-trim m))))
                              meta)
              (push (concat "#+CAPTION: " (cdr known)) out))
            (dolist (m meta) (push m out))
            (dolist (l (writing-habit-columns-render
                        (if (string= (car known) "grasscatcher")
                            writing-habit-columns-task writing-habit-columns-project)
                        (mapcar (lambda (c)
                                  (if (string= (car known) "grasscatcher")
                                      (writing-habit-columns-task-columns c projects)
                                    (writing-habit-columns-project-columns c)))
                                rows)))
              (push l out))
            (setq index end))))))
    (setq out (nreverse out))
    (if (or placed (null legend))
        out
      (let ((rest (if (and out (string-empty-p (string-trim (car out)))) (cdr out) out)))
        (append (list "") legend-table (and rest (cons "" rest)))))))

(provide 'writing-habit-columns)
;;; writing-habit-columns.el ends here
