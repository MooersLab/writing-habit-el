;;; writing-habit-table.el --- The weekly block table as an editable document -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1") (writing-schedule "0.3.1"))
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT license.

;;; Commentary:

;; This module ports `gui/weekly_table.py' from the Python package.  It
;; holds a weekly table as a list of classified lines and edits it without
;; losing a byte, which is what the table editing mode builds on.  It needs
;; no buffer, so its tests run in batch.
;;
;; The weekly table is the one plain-text file that both the scheduler and
;; the tracker read, so the editor must keep it intact.  The document keeps
;; every line of the file in order, classified by the same four row kinds
;; the scheduler parser recognizes, so an untouched file is written back
;; byte for byte.  Each cell edit rewrites one line, and each move swaps
;; lines rather than rewriting them.
;;
;; Classification follows `writing-schedule-parse-table' in both its rules
;; and its order (header, legend, time block, section), because the first
;; match wins there.  The row reader, the day and time parsers, the minute
;; arithmetic, and the overlap test all come from the public API of
;; writing-schedule.el 0.3.1, so the editor never reads a table differently
;; from the scheduler.
;;
;; Row indexes are zero-based and count every table line, rules included,
;; as in the Python module, so its tests port directly.
;;
;; Public entry points:
;;   `writing-habit-table-from-text', `writing-habit-table-from-file'
;;   `writing-habit-table-to-text', `writing-habit-table-save'
;;   `writing-habit-table-set-cell', `writing-habit-table-set-legend'
;;   `writing-habit-table-insert-block', `writing-habit-table-move-block'
;;   `writing-habit-table-insert-legend', `writing-habit-table-move-legend'
;;   `writing-habit-table-sync-legend', `writing-habit-table-rows-clear-of'
;;   `writing-habit-table-totals', `writing-habit-table-code-or-problem'
;;   `writing-habit-table-rename-to-canonical'

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-schedule)
(require 'writing-habit-name)
(require 'writing-habit-plan)

;;;; Constants

(defconst writing-habit-table-section-re "\\`[A-Za-z][A-Za-z ]*:?\\'"
  "A row of letters and spaces with an optional trailing colon.
This mirrors the section rule of the scheduler parser.")

(defconst writing-habit-table-stray-tag-re
  "\\(?::\\([A-Za-z][A-Za-z-]*\\):\\|(\\([A-Za-z][A-Za-z-]*\\))\\)[ \t]*\\'"
  "A trailing tag that the risk reader did not consume, such as :spec:.")

(defconst writing-habit-table-default-section "Writing"
  "The section a block belongs to when no header precedes it.")

(defconst writing-habit-table-default-category "generative"
  "The activity of a section that names none, as the plan importer uses.")

(defconst writing-habit-table-risk-to-tag '(("safe" . "safe") ("speculative" . "risky"))
  "The tag written for each risk class.")

(defconst writing-habit-table-risk-label '(("safe" . "safe") ("speculative" . "risky"))
  "The word shown for each risk class in a tooltip.")

(defconst writing-habit-table--month
  "\\(?:jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oc\\|nov\\|dec\\)[a-z]*\\.?"
  "The start of a month name, so a slip such as Ocotober still reads.")

(defconst writing-habit-table-due-date-re
  (concat "\\(?:,[ \t]*\\|[ \t]+\\)"
          "\\(?:\\(?:due\\|by\\|before\\)[ \t]*:?[ \t]*\\)?"
          "\\(?1:" writing-habit-table--month
          "[ \t]+[0-9]\\{1,2\\}\\(?:st\\|nd\\|rd\\|th\\)?\\(?:,?[ \t]*[0-9]\\{4\\}\\)?"
          "\\|[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
          "\\|[0-9]\\{1,2\\}/[0-9]\\{1,2\\}\\(?:/[0-9]\\{2,4\\}\\)?"
          "\\)[ \t]*\\'")
  "A due date at the end of a legend description, in group 1.")

;;;; Small helpers

(defun writing-habit-table-split-due-date (description)
  "Return (NAME . DUE) for a legend DESCRIPTION.
DUE is the due date as the writer typed it, or nil.  A bare trailing
comma, as in \"0485GUIsc,\", is dropped from the name."
  (let ((text (string-trim description))
        (case-fold-search t))
    (if (string-match writing-habit-table-due-date-re text)
        (cons (string-trim-right (substring text 0 (match-beginning 0)) "[ ,]+")
              (string-trim (match-string 1 text)))
      (cons (string-trim-right text "[ ,]+") nil))))

(defun writing-habit-table--to-minutes (clock)
  "Return the minutes after midnight of the HH:MM string CLOCK."
  (let ((parts (split-string clock ":")))
    (+ (* 60 (string-to-number (car parts))) (string-to-number (cadr parts)))))

(defun writing-habit-table--from-minutes (total)
  "Return TOTAL minutes, clamped to one day, as an HH:MM string."
  (let ((total (max 0 (min total (* 24 60)))))
    (format "%02d:%02d" (/ total 60) (% total 60))))

(defun writing-habit-table--interval (start end)
  "Return (FIRST . LAST) minutes of the half-open range START to END.
A range whose end is not after its start runs past midnight."
  (let ((first (writing-habit-table--to-minutes start))
        (last (writing-habit-table--to-minutes end)))
    (when (<= last first) (setq last (+ last (* 24 60))))
    (cons first last)))

(defun writing-habit-table--category (section)
  "Return the activity category of the section named SECTION."
  (or (cdr (assoc (downcase (string-trim section))
                  writing-habit-plan--section-to-category))
      writing-habit-table-default-category))

(defun writing-habit-table--split-lines (text)
  "Split TEXT into lines the way Python's splitlines does for the usual breaks."
  (let ((lines (split-string text "\r\n\\|[\n\r]")))
    (when (and lines (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    lines))

(defun writing-habit-table--legend-text (code description risk)
  "Return the first cell of a legend row for CODE, DESCRIPTION, and RISK."
  (let ((tag (cdr (assoc (or risk "") writing-habit-table-risk-to-tag))))
    (concat (string-trim-right (format "%s: %s" code (string-trim description)))
            (if tag (format " :%s:" tag) ""))))

;;;; Data

(cl-defstruct (writing-habit-table-row
               (:constructor writing-habit-table-row-create)
               (:copier nil))
  "One line of the file, classified and kept verbatim."
  kind raw cells parsed (section writing-habit-table-default-section))

(cl-defstruct (writing-habit-table
               (:constructor writing-habit-table--create)
               (:copier nil))
  "A parsed weekly table that can be written back unchanged.
ROWS is the list of table lines.  COLUMNS lists (CELL-INDEX OFFSET LABEL)
for every day column.  BEFORE and AFTER hold the lines around the table.
SYNCED-CODES are the codes whose legend row `writing-habit-table-sync-legend'
added, the only rows it ever drops again."
  path rows columns before after dirty synced-codes)

(cl-defstruct (writing-habit-table-block
               (:constructor writing-habit-table-block-create)
               (:copier nil))
  "One filled cell of a block row, which is one planned writing block."
  offset start end letter section row column)

(defun writing-habit-table-block-category (block)
  "Return the activity category of BLOCK."
  (writing-habit-table--category (writing-habit-table-block-section block)))

(defun writing-habit-table-block-minutes (block)
  "Return the planned minutes of BLOCK."
  (writing-schedule-minutes-between (writing-habit-table-block-start block)
                                    (writing-habit-table-block-end block)))

(defun writing-habit-table-row-at (table index)
  "Return row INDEX of TABLE."
  (nth index (writing-habit-table-rows table)))

(defun writing-habit-table--kind (table index)
  "Return the kind of row INDEX of TABLE, or nil when out of range."
  (when (and (integerp index) (>= index 0)
             (< index (length (writing-habit-table-rows table))))
    (writing-habit-table-row-kind (writing-habit-table-row-at table index))))

(defun writing-habit-table--insert-row (table at row)
  "Insert ROW into TABLE so that it becomes row AT."
  (let ((rows (writing-habit-table-rows table)))
    (setf (writing-habit-table-rows table)
          (append (seq-take rows at) (list row) (nthcdr at rows)))))

(defun writing-habit-table--pop-row (table index)
  "Remove row INDEX from TABLE and return it."
  (let* ((rows (writing-habit-table-rows table))
         (row (nth index rows)))
    (setf (writing-habit-table-rows table)
          (append (seq-take rows index) (nthcdr (1+ index) rows)))
    row))

(defun writing-habit-table--indexes (table kinds)
  "Return the indexes of the rows of TABLE whose kind is in KINDS."
  (let ((i -1) (out '()))
    (dolist (row (writing-habit-table-rows table))
      (setq i (1+ i))
      (when (memq (writing-habit-table-row-kind row) kinds) (push i out)))
    (nreverse out)))

;;;; Reading

(defun writing-habit-table-from-text (text &optional path)
  "Return a table document parsed from TEXT, remembering PATH."
  (let ((table (writing-habit-table--create :path path))
        (rows '()) (columns nil) (before '()) (after '())
        (in-table nil) (done nil)
        (section writing-habit-table-default-section))
    (dolist (line (writing-habit-table--split-lines text))
      (let ((is-row (string-prefix-p "|" (string-trim line))))
        (if (or done (not is-row))
            (progn
              (if in-table (push line after) (push line before))
              (when (and in-table (not is-row)) (setq done t)))
          (setq in-table t)
          (if (string-match-p "\\`[ \t]*|[-+]" line)
              (push (writing-habit-table-row-create :kind 'rule :raw line) rows)
            (let* ((cells (writing-schedule-split-row line))
                   (first (or (car cells) ""))
                   legend times)
              (cond
               ((and (null columns)
                     (seq-some #'writing-schedule-day-offset (cdr cells)))
                (let ((index 0))
                  (dolist (cell cells)
                    (let ((offset (writing-schedule-day-offset cell)))
                      (when (and offset (> index 0))
                        (push (list index offset cell) columns)))
                    (setq index (1+ index))))
                (setq columns (nreverse columns))
                (push (writing-habit-table-row-create :kind 'header :raw line :cells cells)
                      rows))
               ((setq legend (writing-habit-name-parse-legend-cell first))
                (push (writing-habit-table-row-create
                       :kind 'legend :raw line :cells cells :parsed legend)
                      rows))
               ((and columns (setq times (writing-schedule-parse-time first)))
                (push (writing-habit-table-row-create
                       :kind 'block :raw line :cells cells
                       :parsed times :section section)
                      rows))
               ((and (not (string-empty-p first))
                     (string-match-p writing-habit-table-section-re first)
                     (not (writing-schedule-parse-time first)))
                (setq section (string-trim (string-replace ":" "" first)))
                (push (writing-habit-table-row-create
                       :kind 'section :raw line :cells cells :parsed section)
                      rows))
               (t
                (push (writing-habit-table-row-create :kind 'other :raw line :cells cells)
                      rows))))))))
    (setf (writing-habit-table-rows table) (nreverse rows)
          (writing-habit-table-columns table) columns
          (writing-habit-table-before table) (nreverse before)
          (writing-habit-table-after table) (nreverse after))
    table))

(defun writing-habit-table-from-file (path)
  "Return a table document read from the file at PATH."
  (writing-habit-table-from-text
   (with-temp-buffer
     (let ((coding-system-for-read 'utf-8))
       (insert-file-contents path))
     (buffer-string))
   path))

;;;; Writing

(defun writing-habit-table-to-text (table)
  "Return the file text of TABLE.
An untouched document returns its input byte for byte, apart from a final
newline that a file without one gains."
  (let ((parts (append (writing-habit-table-before table)
                       (mapcar #'writing-habit-table-row-raw
                               (writing-habit-table-rows table))
                       (writing-habit-table-after table))))
    (if parts (concat (mapconcat #'identity parts "\n") "\n") "")))

(defun writing-habit-table-save (table &optional path)
  "Write TABLE to PATH, or to its own path, and return the path written."
  (let ((target (or path (writing-habit-table-path table))))
    (unless target (error "No path to save to"))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (writing-habit-table-to-text table) nil target nil 'silent))
    (setf (writing-habit-table-path table) target
          (writing-habit-table-dirty table) nil)
    target))

;;;; Rendering one line

(defun writing-habit-table--fit (chunk value)
  "Return CHUNK carrying VALUE, keeping the column width when VALUE fits.
A value too long for its slot widens that slot alone."
  (let ((width (if (>= (length chunk) 2) (- (length chunk) 2) 0)))
    (if (<= (length value) width)
        (concat " " value (make-string (- width (length value)) ?\s) " ")
      (concat " " value " "))))

(defun writing-habit-table--render-cell (row index)
  "Return the raw line of ROW with only cell INDEX rewritten."
  (let* ((parts (split-string (writing-habit-table-row-raw row) "|"))
         (slot (1+ index)))
    (when (< slot (length parts))
      (setf (nth slot parts)
            (writing-habit-table--fit (nth slot parts)
                                      (nth index (writing-habit-table-row-cells row)))))
    (mapconcat #'identity parts "|")))

(defun writing-habit-table--render (row)
  "Return the raw line of ROW with every cell rewritten, for a new line."
  (let ((parts (split-string (writing-habit-table-row-raw row) "|"))
        (index 0))
    (dolist (value (writing-habit-table-row-cells row))
      (let ((slot (1+ index)))
        (when (< slot (length parts))
          (setf (nth slot parts) (writing-habit-table--fit (nth slot parts) value))))
      (setq index (1+ index)))
    (mapconcat #'identity parts "|")))

(defun writing-habit-table--blank-raw (model-raw width)
  "Return MODEL-RAW with its first WIDTH cells blanked to their own widths."
  (let ((n -1))
    (mapconcat (lambda (part)
                 (setq n (1+ n))
                 (if (or (= n 0) (> n width))
                     part
                   (make-string (max (length part) 3) ?\s)))
               (split-string model-raw "|") "|")))

;;;; Content

(defun writing-habit-table-day-labels (table)
  "Return the day labels of TABLE's header, in column order."
  (mapcar (lambda (c) (nth 2 c)) (writing-habit-table-columns table)))

(defun writing-habit-table-block-rows (table)
  "Return the indexes of TABLE's time-block rows."
  (writing-habit-table--indexes table '(block)))

(defun writing-habit-table-legend-rows (table)
  "Return the indexes of TABLE's legend rows, in file order."
  (writing-habit-table--indexes table '(legend)))

(defun writing-habit-table-cell (table row-index column-index)
  "Return cell COLUMN-INDEX of row ROW-INDEX in TABLE, or the empty string."
  (or (nth column-index (writing-habit-table-row-cells
                         (writing-habit-table-row-at table row-index)))
      ""))

(defun writing-habit-table-blocks (table)
  "Return every filled block of TABLE, in file order."
  (let ((out '()) (row-index -1))
    (dolist (row (writing-habit-table-rows table))
      (setq row-index (1+ row-index))
      (when (eq (writing-habit-table-row-kind row) 'block)
        (let ((times (writing-habit-table-row-parsed row)))
          (dolist (col (writing-habit-table-columns table))
            (let ((text (writing-habit-table-cell table row-index (car col))))
              (unless (string-empty-p text)
                (push (writing-habit-table-block-create
                       :offset (nth 1 col) :start (car times) :end (cdr times)
                       :letter (upcase text)
                       :section (writing-habit-table-row-section row)
                       :row row-index :column (car col))
                      out)))))))
    (nreverse out)))

(defun writing-habit-table-events (table)
  "Return TABLE's blocks as scheduler event plists, for its overlap check."
  (mapcar (lambda (b)
            (list :section (writing-habit-table-block-section b)
                  :offset (writing-habit-table-block-offset b)
                  :start (writing-habit-table-block-start b)
                  :end (writing-habit-table-block-end b)
                  :letter (writing-habit-table-block-letter b)))
          (writing-habit-table-blocks table)))

(defun writing-habit-table-legend (table)
  "Return TABLE's legend as an alist of (CODE DESCRIPTION RISK).
A code defined twice keeps its first definition, as the readers do."
  (let ((out '()))
    (dolist (row (writing-habit-table-rows table))
      (when (eq (writing-habit-table-row-kind row) 'legend)
        (let ((parsed (writing-habit-table-row-parsed row)))
          (unless (assoc (car parsed) out) (push parsed out)))))
    (nreverse out)))

(defun writing-habit-table-used-codes (table)
  "Return the codes used in the grid of TABLE, in first-appearance order."
  (let ((out '()))
    (dolist (b (writing-habit-table-blocks table))
      (let ((letter (writing-habit-table-block-letter b)))
        (unless (or (string-empty-p letter) (member letter out))
          (push letter out))))
    (nreverse out)))

(defun writing-habit-table-project-info (table code)
  "Return a plist (:code :name :due :risk) for CODE in TABLE, or nil.
This is what a cell explains about itself when point rests on it."
  (let* ((code (upcase (string-trim code)))
         (entry (cdr (assoc code (writing-habit-table-legend table)))))
    (when entry
      (let ((split (writing-habit-table-split-due-date (car entry))))
        (list :code code :name (car split) :due (cdr split)
              :risk (cdr (assoc (or (cadr entry) "") writing-habit-table-risk-label)))))))

;;;; Editing cells and the legend

(defun writing-habit-table-set-cell (table row-index column-index value)
  "Set cell COLUMN-INDEX of block row ROW-INDEX in TABLE to VALUE.
Return non-nil when anything changed.  Only that one line is rewritten."
  (let ((row (writing-habit-table-row-at table row-index))
        (value (upcase (string-trim value))))
    (unless (and row (eq (writing-habit-table-row-kind row) 'block))
      (error "Only a time-block row holds project codes"))
    (let ((cells (writing-habit-table-row-cells row)))
      (while (<= (length cells) column-index)
        (setq cells (append cells (list ""))))
      (setf (writing-habit-table-row-cells row) cells)
      (unless (equal (nth column-index cells) value)
        (setf (nth column-index (writing-habit-table-row-cells row)) value)
        (setf (writing-habit-table-row-raw row)
              (writing-habit-table--render-cell row column-index))
        (setf (writing-habit-table-dirty table) t)
        t))))

(defun writing-habit-table-set-legend (table row-index code description risk)
  "Rewrite legend row ROW-INDEX of TABLE with CODE, DESCRIPTION, and RISK.
RISK is a class name, \"safe\" or \"speculative\", or nil.  Return
non-nil when anything changed."
  (let ((row (writing-habit-table-row-at table row-index)))
    (unless (and row (eq (writing-habit-table-row-kind row) 'legend))
      (error "Not a legend row"))
    (let ((text (writing-habit-table--legend-text
                 (upcase (string-trim code)) description risk)))
      (unless (equal (car (writing-habit-table-row-cells row)) text)
        (if (writing-habit-table-row-cells row)
            (setf (car (writing-habit-table-row-cells row)) text)
          (setf (writing-habit-table-row-cells row) (list text)))
        (setf (writing-habit-table-row-raw row) (writing-habit-table--render-cell row 0)
              (writing-habit-table-row-parsed row) (writing-habit-name-parse-legend-cell text)
              (writing-habit-table-dirty table) t)
        t))))

(defun writing-habit-table-add-legend (table code &optional description risk)
  "Add a legend row for CODE to TABLE and return its index.
The new line copies the shape of the last legend row, so the column
widths survive.  A table with no legend yet gets a line as wide as its
header."
  (let* ((text (writing-habit-table--legend-text
                (upcase (string-trim code)) (or description "") risk))
         (existing (writing-habit-table-legend-rows table))
         cells raw at)
    (if existing
        (let ((model (writing-habit-table-row-at table (car (last existing)))))
          (setq cells (make-list (max (length (writing-habit-table-row-cells model)) 1) "")
                raw (writing-habit-table-row-raw model)
                at (1+ (car (last existing)))))
      (let ((width (max 1 (apply #'max 0
                                 (mapcar (lambda (r)
                                           (if (memq (writing-habit-table-row-kind r)
                                                     '(header block))
                                               (length (writing-habit-table-row-cells r))
                                             0))
                                         (writing-habit-table-rows table))))))
        (setq cells (make-list width "")
              raw (concat "|" (mapconcat #'identity (make-list width "   ") "|") "|")
              at (length (writing-habit-table-rows table)))))
    (setf (car cells) text)
    (let ((row (writing-habit-table-row-create
                :kind 'legend :raw raw :cells cells
                :parsed (writing-habit-name-parse-legend-cell text))))
      (setf (writing-habit-table-row-raw row) (writing-habit-table--render row))
      (writing-habit-table--insert-row table at row))
    (setf (writing-habit-table-dirty table) t)
    at))

(defun writing-habit-table-next-free-code (table)
  "Return the first letter used by neither the legend nor the grid of TABLE."
  (let ((taken (append (mapcar #'car (writing-habit-table-legend table))
                       (writing-habit-table-used-codes table))))
    (or (seq-find (lambda (l) (not (member l taken)))
                  (mapcar #'char-to-string (number-sequence ?A ?Z)))
        "")))

(defun writing-habit-table-insert-legend (table near above code
                                                &optional description risk)
  "Insert a legend row for CODE beside legend row NEAR of TABLE.
ABOVE non-nil puts it before NEAR.  NEAR nil puts it at the end of the
legend, or starts a legend.  Return the new row's index.  Signal an error
for a code the readers would not recognize, and for a code the legend
already defines, because the readers keep the first definition."
  (let ((code (upcase (string-trim code)))
        (existing (writing-habit-table-legend-rows table)))
    (unless (writing-habit-name-parse-legend-cell (concat code ": x"))
      (error "%s is not a project code.  A code is a capital letter followed by up to three capitals or digits"
             (if (string-empty-p code) "An empty code" code)))
    (when (assoc code (writing-habit-table-legend table))
      (error "%s is already in the legend" code))
    (when (and near (not (memq near existing)))
      (error "Not a legend row"))
    (if (null near)
        (writing-habit-table-add-legend table code description risk)
      (let* ((text (writing-habit-table--legend-text code (or description "") risk))
             (model (writing-habit-table-row-at table near))
             (cells (make-list (max (length (writing-habit-table-row-cells model)) 1) ""))
             (at (if above near (1+ near))))
        (setf (car cells) text)
        (let ((row (writing-habit-table-row-create
                    :kind 'legend
                    :raw (writing-habit-table--blank-raw
                          (writing-habit-table-row-raw model) (length cells))
                    :cells cells
                    :parsed (writing-habit-name-parse-legend-cell text))))
          (setf (writing-habit-table-row-raw row) (writing-habit-table--render row))
          (writing-habit-table--insert-row table at row))
        (setf (writing-habit-table-dirty table) t)
        at))))

(defun writing-habit-table-remove-legend (table row-index)
  "Drop legend row ROW-INDEX from TABLE."
  (unless (eq (writing-habit-table--kind table row-index) 'legend)
    (error "Not a legend row"))
  (writing-habit-table--pop-row table row-index)
  (setf (writing-habit-table-dirty table) t))

(defun writing-habit-table-sync-legend (table)
  "Make the legend of TABLE cover every code used in its grid.
A code typed into a cell gains a legend row.  A row whose code has left
the grid is dropped only when this function added it and it is still
blank, so a project the writer inserted or one the file held stays.
Return non-nil when the legend changed."
  (let ((used (writing-habit-table-used-codes table))
        (defined (mapcar #'car (writing-habit-table-legend table)))
        (changed nil))
    (dolist (code used)
      (unless (member code defined)
        (writing-habit-table-add-legend table code)
        (push code (writing-habit-table-synced-codes table))
        (setq changed t)))
    (dolist (index (reverse (writing-habit-table-legend-rows table)))
      (pcase-let ((`(,code ,description ,risk)
                   (writing-habit-table-row-parsed (writing-habit-table-row-at table index))))
        (when (and (not (member code used))
                   (string-empty-p description)
                   (null risk)
                   (member code (writing-habit-table-synced-codes table)))
          (writing-habit-table-remove-legend table index)
          (setf (writing-habit-table-synced-codes table)
                (delete code (writing-habit-table-synced-codes table)))
          (setq changed t))))
    changed))

;;;; Inserting time-block rows

(defun writing-habit-table--section-at (table index)
  "Return the section a block placed at row INDEX of TABLE would join."
  (let ((section writing-habit-table-default-section))
    (cl-loop for row in (seq-take (writing-habit-table-rows table) index)
             when (eq (writing-habit-table-row-kind row) 'section)
             do (setq section (writing-habit-table-row-parsed row)))
    section))

(defun writing-habit-table--block-model (table near)
  "Return the row whose shape a new block beside row NEAR of TABLE copies."
  (let ((blocks (writing-habit-table-block-rows table)))
    (if blocks
        (writing-habit-table-row-at
         table (car (seq-sort-by (lambda (i) (abs (- i near))) #'< blocks)))
      (seq-find (lambda (r) (eq (writing-habit-table-row-kind r) 'header))
                (writing-habit-table-rows table)))))

(defun writing-habit-table-suggest-times (table near above)
  "Return (START . END) for a block inserted beside row NEAR of TABLE.
A block above another ends where that one starts, and a block below
starts where it ends, taking the length of its neighbour.  Beside a
section header, the nearest block of the neighbouring section serves."
  (let ((blocks (writing-habit-table-block-rows table))
        (anchor nil))
    (cond
     ((memq near blocks) (setq anchor near))
     (above
      (let ((before (seq-filter (lambda (i) (< i near)) blocks)))
        (when before (setq anchor (car (last before)) above nil))))
     (t
      (let ((after (seq-filter (lambda (i) (> i near)) blocks)))
        (when after (setq anchor (car after) above t)))))
    (if (null anchor)
        (cons "09:00" "10:00")
      (let* ((times (writing-habit-table-row-parsed (writing-habit-table-row-at table anchor)))
             (start (writing-habit-table--to-minutes (car times)))
             (end (writing-habit-table--to-minutes (cdr times)))
             (len (max (- end start) 15)))
        (if above
            (cons (writing-habit-table--from-minutes (max (- start len) 0))
                  (writing-habit-table--from-minutes start))
          (cons (writing-habit-table--from-minutes end)
                (writing-habit-table--from-minutes (min (+ end len) (* 24 60)))))))))

(defun writing-habit-table--render-time (row model)
  "Write the time cell of the new ROW, justified as MODEL's time cell is."
  (let ((parts (split-string (writing-habit-table-row-raw row) "|")))
    (if (< (length parts) 2)
        (writing-habit-table-row-raw row)
      (let* ((right (and model (eq (writing-habit-table-row-kind model) 'block)
                         (let ((slot (nth 1 (split-string
                                             (writing-habit-table-row-raw model) "|"))))
                           (> (- (length slot) (length (string-trim-left slot " +"))) 1))))
             (width (- (length (nth 1 parts)) 2))
             (value (car (writing-habit-table-row-cells row))))
        (setf (nth 1 parts)
              (if (<= (length value) width)
                  (concat " "
                          (if right
                              (concat (make-string (- width (length value)) ?\s) value)
                            (concat value (make-string (- width (length value)) ?\s)))
                          " ")
                (concat " " value " ")))
        (mapconcat #'identity parts "|")))))

(defun writing-habit-table-insert-block (table near above start end)
  "Insert an empty time block from START to END beside row NEAR of TABLE.
ABOVE non-nil puts it before NEAR.  NEAR must be a block or a section
row.  Return the new row's index.  Every other line is left untouched."
  (unless (writing-habit-table-columns table)
    (error "The table has no header row naming the days"))
  (unless (memq (writing-habit-table--kind table near) '(block section))
    (error "Select a time block or a section row first"))
  (let ((times (writing-schedule-parse-time (format "%s-%s" start end))))
    (unless times (error "Not a time range: %s-%s" start end))
    (when (<= (writing-habit-table--to-minutes (cdr times))
              (writing-habit-table--to-minutes (car times)))
      (error "The block must end after it starts"))
    (let* ((model (writing-habit-table--block-model table near))
           (width (max (if model (length (writing-habit-table-row-cells model)) 0)
                       (1+ (apply #'max (mapcar #'car (writing-habit-table-columns table))))))
           (cells (make-list width ""))
           (at (if above near (1+ near))))
      (setf (car cells) (format "%s-%s" (car times) (cdr times)))
      (let ((row (writing-habit-table-row-create
                  :kind 'block
                  :raw (if model
                           (writing-habit-table--blank-raw
                            (writing-habit-table-row-raw model) width)
                         (concat "|" (mapconcat #'identity (make-list width "   ") "|") "|"))
                  :cells cells :parsed times
                  :section (writing-habit-table--section-at table at))))
        (setf (writing-habit-table-row-raw row) (writing-habit-table--render-time row model))
        (writing-habit-table--insert-row table at row))
      (setf (writing-habit-table-dirty table) t)
      at)))

;;;; Moving time-block rows

(defun writing-habit-table--grid-rows (table)
  "Return the indexes of TABLE's section and block rows."
  (writing-habit-table--indexes table '(section block)))

(defun writing-habit-table-move-target (table row-index up)
  "Return the grid row that moving block ROW-INDEX of TABLE would pass, or nil.
UP non-nil moves toward the top.  A block never moves above the first
section header, and nothing moves past either end of the grid."
  (when (eq (writing-habit-table--kind table row-index) 'block)
    (let* ((grid (writing-habit-table--grid-rows table))
           (position (+ (cl-position row-index grid) (if up -1 1))))
      (when (and (>= position 0) (< position (length grid)))
        (let ((target (nth position grid)))
          (unless (and up
                       (eq (writing-habit-table--kind table target) 'section)
                       (not (seq-some (lambda (i) (eq (writing-habit-table--kind table i)
                                                      'section))
                                      (seq-take grid position))))
            target))))))

(defun writing-habit-table-can-move (table row-index up)
  "Return non-nil when block ROW-INDEX of TABLE can move UP or down."
  (and (writing-habit-table-move-target table row-index up) t))

(defun writing-habit-table--resection (table)
  "Give every block row of TABLE the section of the header above it."
  (let ((section writing-habit-table-default-section))
    (dolist (row (writing-habit-table-rows table))
      (pcase (writing-habit-table-row-kind row)
        ('section (setq section (writing-habit-table-row-parsed row)))
        ('block (setf (writing-habit-table-row-section row) section))))))

(defun writing-habit-table-move-block (table row-index up)
  "Move block ROW-INDEX of TABLE one place UP or down; return its new index.
The line is moved, not rewritten.  Passing a section header moves the
block into the neighbouring section."
  (let ((target (writing-habit-table-move-target table row-index up)))
    (unless target
      (if (eq (writing-habit-table--kind table row-index) 'block)
          (error "The row is already at the %s of the grid" (if up "top" "bottom"))
        (error "Only a time-block row can be moved")))
    (writing-habit-table--insert-row table target
                                     (writing-habit-table--pop-row table row-index))
    (writing-habit-table--resection table)
    (setf (writing-habit-table-dirty table) t)
    target))

;;;; Moving legend rows

(defun writing-habit-table-legend-move-target (table row-index up)
  "Return the legend row that moving ROW-INDEX of TABLE UP would pass, or nil."
  (let* ((legend (writing-habit-table-legend-rows table))
         (pos (cl-position row-index legend)))
    (when pos
      (let ((p (+ pos (if up -1 1))))
        (when (and (>= p 0) (< p (length legend))) (nth p legend))))))

(defun writing-habit-table-can-move-legend (table row-index up)
  "Return non-nil when legend row ROW-INDEX of TABLE can move UP or down."
  (and (writing-habit-table-legend-move-target table row-index up) t))

(defun writing-habit-table-move-legend (table row-index up)
  "Move legend row ROW-INDEX of TABLE one place UP or down.
Return its new index.  The order decides which definition wins when a
code is defined twice, because the readers keep the first."
  (let ((target (writing-habit-table-legend-move-target table row-index up)))
    (unless target
      (if (eq (writing-habit-table--kind table row-index) 'legend)
          (error "The project is already at the %s of the legend" (if up "top" "bottom"))
        (error "Only a legend row can be moved in the legend")))
    (writing-habit-table--insert-row table target
                                     (writing-habit-table--pop-row table row-index))
    (setf (writing-habit-table-dirty table) t)
    target))

;;;; Derived readings

(defun writing-habit-table-rows-clear-of (table row-index)
  "Return the block rows of TABLE whose time range does not overlap ROW-INDEX.
Ranges are half-open, so blocks that only touch do not overlap, and the
row itself is left out."
  (unless (eq (writing-habit-table--kind table row-index) 'block)
    (error "Only a time-block row has a time range"))
  (let* ((times (writing-habit-table-row-parsed (writing-habit-table-row-at table row-index)))
         (iv (writing-habit-table--interval (car times) (cdr times))))
    (seq-filter
     (lambda (index)
       (and (/= index row-index)
            (let* ((ot (writing-habit-table-row-parsed (writing-habit-table-row-at table index)))
                   (ov (writing-habit-table--interval (car ot) (cdr ot))))
              (not (and (< (car iv) (cdr ov)) (< (car ov) (cdr iv)))))))
     (writing-habit-table-block-rows table))))

(defun writing-habit-table-overlaps (table)
  "Return TABLE's clashing pairs, by the scheduler's own rule."
  (writing-schedule-overlaps (writing-habit-table-events table)))

(defun writing-habit-table-overlap-lines (table)
  "Return one readable line per clash in TABLE, worded as the scheduler words it."
  (writing-schedule-overlap-lines (writing-habit-table-overlaps table)))

(defun writing-habit-table-conflicting-cells (table)
  "Return (ROW . COLUMN) for every block of TABLE that takes part in a clash."
  (let ((ids (writing-schedule-conflicting-identities (writing-habit-table-events table))))
    (delete-dups
     (delq nil
           (mapcar (lambda (b)
                     (when (member (list (writing-habit-table-block-offset b)
                                         (writing-habit-table-block-start b)
                                         (writing-habit-table-block-end b)
                                         (writing-habit-table-block-letter b)
                                         (writing-habit-table-block-section b))
                                   ids)
                       (cons (writing-habit-table-block-row b)
                             (writing-habit-table-block-column b))))
                   (writing-habit-table-blocks table))))))

(defun writing-habit-table-week (table)
  "Return TABLE's week as `writing-habit-name-encode' expects it.
Each day lists (CATEGORY . LETTER) for its blocks in time order."
  (let* ((last (apply #'max -1 (mapcar #'cadr (writing-habit-table-columns table))))
         (days (make-vector (1+ last) nil)))
    (dolist (b (seq-sort (lambda (a b)
                           (let ((oa (writing-habit-table-block-offset a))
                                 (ob (writing-habit-table-block-offset b)))
                             (or (< oa ob)
                                 (and (= oa ob)
                                      (string< (writing-habit-table-block-start a)
                                               (writing-habit-table-block-start b))))))
                         (writing-habit-table-blocks table)))
      (let ((o (writing-habit-table-block-offset b)))
        (aset days o (append (aref days o)
                             (list (cons (writing-habit-table-block-category b)
                                         (writing-habit-table-block-letter b)))))))
    (append days nil)))

(defun writing-habit-table-code (table)
  "Return the canonical schedule code of TABLE's grid.
Signal an error when a cell holds a code of more than one letter."
  (writing-habit-name-encode (writing-habit-table-week table)))

(defun writing-habit-table-code-or-problem (table)
  "Return (CODE . nil), or (nil . REASON) when TABLE's week has no name."
  (condition-case err
      (cons (writing-habit-table-code table) nil)
    (error
     (let ((reason (error-message-string err)))
       (when (string-match-p "one uppercase letter" reason)
         (setq reason (concat reason ".  Give that project a single-letter alias "
                              "for the file name, and keep its full code in the legend.")))
       (cons nil reason)))))

(defun writing-habit-table-name-matches-code (table)
  "Return non-nil when TABLE's file name is the canonical name of its grid."
  (let ((code (car (writing-habit-table-code-or-problem table)))
        (path (writing-habit-table-path table)))
    (and code path (equal (file-name-base path) code))))

(defun writing-habit-table-rename-to-canonical (table)
  "Move TABLE's file to its canonical name and return the new path.
Signal an error when the week has no canonical name, when TABLE has
unsaved edits or no file, or when another file holds the target name."
  (when (writing-habit-table-dirty table) (error "Save the table before renaming it"))
  (unless (writing-habit-table-path table) (error "This table has never been saved"))
  (let ((result (writing-habit-table-code-or-problem table)))
    (unless (car result) (error "%s" (cdr result)))
    (let* ((source (writing-habit-table-path table))
           (target (expand-file-name (concat (car result) ".org")
                                     (file-name-directory (expand-file-name source)))))
      (cond
       ((equal (expand-file-name source) target) source)
       ((file-exists-p target)
        (error "%s already exists" (file-name-nondirectory target)))
       (t (rename-file source target)
          (setf (writing-habit-table-path table) target)
          target)))))

(defun writing-habit-table-totals (table)
  "Return planned minutes of TABLE by day, project, and category.
The result is a plist (:day ALIST :project ALIST :category ALIST)."
  (let ((by-day (mapcar (lambda (l) (cons l 0)) (writing-habit-table-day-labels table)))
        (by-project '()) (by-category '())
        (labels (mapcar (lambda (c) (cons (nth 1 c) (nth 2 c)))
                        (writing-habit-table-columns table))))
    (cl-flet ((add (alist key n)
                (let ((cell (assoc key alist)))
                  (if cell (progn (setcdr cell (+ (cdr cell) n)) alist)
                    (append alist (list (cons key n)))))))
      (dolist (b (writing-habit-table-blocks table))
        (let ((m (writing-habit-table-block-minutes b)))
          (setq by-day (add by-day (cdr (assoc (writing-habit-table-block-offset b) labels)) m)
                by-project (add by-project (writing-habit-table-block-letter b) m)
                by-category (add by-category (writing-habit-table-block-category b) m)))))
    (list :day by-day :project by-project :category by-category)))

(defun writing-habit-table-legend-check (table)
  "Return (ROWS PROBLEMS) of the project-letter check of TABLE."
  (writing-habit-name-check-against-legend
   (cl-mapcar #'cons writing-habit-name-days (writing-habit-table-week table))
   (writing-habit-table-legend table)))

(defun writing-habit-table-duplicate-legend-codes (table)
  "Return an alist (CODE . DESCRIPTIONS) for each code defined twice in TABLE."
  (let ((seen '()))
    (dolist (row (writing-habit-table-rows table))
      (when (eq (writing-habit-table-row-kind row) 'legend)
        (let* ((parsed (writing-habit-table-row-parsed row))
               (cell (assoc (car parsed) seen)))
          (if cell
              (setcdr cell (append (cdr cell) (list (nth 1 parsed))))
            (setq seen (append seen (list (list (car parsed) (nth 1 parsed)))))))))
    (seq-filter (lambda (c) (> (length (cdr c)) 1)) seen)))

(defun writing-habit-table-stray-risk-tags (table)
  "Return (CODE . TAG) for each legend entry of TABLE whose risk tag was not read."
  (let ((out '()))
    (dolist (entry (writing-habit-table-legend table))
      (when (and (null (nth 2 entry))
                 (string-match writing-habit-table-stray-tag-re (nth 1 entry)))
        (push (cons (car entry) (or (match-string 1 (nth 1 entry))
                                    (match-string 2 (nth 1 entry))))
              out)))
    (nreverse out)))

(defun writing-habit-table-unknown-sections (table)
  "Return the section names of TABLE that map to no activity category."
  (let ((seen '()))
    (dolist (row (writing-habit-table-rows table))
      (when (eq (writing-habit-table-row-kind row) 'section)
        (let ((name (writing-habit-table-row-parsed row)))
          (unless (or (assoc (downcase (string-trim name))
                             writing-habit-plan--section-to-category)
                      (member name seen))
            (push name seen)))))
    (nreverse seen)))

(provide 'writing-habit-table)
;;; writing-habit-table.el ends here
