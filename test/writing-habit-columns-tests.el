;;; writing-habit-columns-tests.el --- Tests for project tables in columns -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A weekly table file may keep its legend and its lists as tables with
;; one column per field.  These tests mirror tests/test_columns.py of the
;; Python package: one row at a time, the whole file read and written, the
;; readers that take a file name, and the editing mode in such a buffer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))

(require 'writing-habit-columns)

(defconst writing-habit-columns-tests--ok
  (and (require 'writing-schedule nil t) (fboundp 'writing-schedule-split-row))
  "Non-nil when writing-schedule.el is available.")

(when writing-habit-columns-tests--ok
  (require 'writing-habit-table)
  (require 'writing-habit-table-mode))

(defconst writing-habit-columns-tests--grid
  (concat "#+TITLE: Writing schedule\n"
          "|        Time <l> | M  | Tu |\n"
          "|-----------------+----+----|\n"
          "|     04:00-06:00 | gB | gB |\n"
          "|     06:15-08:15 | e0 |    |\n"
          "|-----------------+----+----|\n")
  "The week itself.")

(defconst writing-habit-columns-tests--tables
  (concat "\n"
          "#+NAME: This week\n"
          "| Code | Project Number | Project Name    | Deadline   | Risk tag | Default activity |\n"
          "|------+----------------+-----------------+------------+----------+------------------|\n"
          "| 0:   | 8721           | NoahlanaMonzon  | 2026-10-19 | :safe:   |                  |\n"
          "| B:   | 1006           | AIrxOpt         | 2026-12-15 | :safe:   | editing          |\n"
          "\n"
          "#+NAME: current-semester-projects\n"
          "#+CAPTION: Fall 2026 projects\n"
          "| Code | Project Number | Project Name | Deadline   | Risk tag | Default activity |\n"
          "|------+----------------+--------------+------------+----------+------------------|\n"
          "| A:   | 1094           | RNAstrucDet  |            | :safe:   |                  |\n"
          "| -    | 1452           | PHFSboXt     | 2027-03-01 | :risky:  |                  |\n"
          "| Y:   | 2026           | words        |            | :risky:  |                  |\n"
          "\n"
          "#+NAME: completed-this-semester\n"
          "#+CAPTION: Completed in Fall 2026\n"
          "| Code | Project Number | Project Name  | Deadline   | Risk tag | Default activity |\n"
          "|------+----------------+---------------+------------+----------+------------------|\n"
          "| A:   | 3292           | ABRF-SC3-2026 | 2026-10-09 | :safe:   |                  |\n"
          "\n"
          "#+NAME: grasscatcher\n"
          "#+CAPTION: Grasscatcher\n"
          "| Code | Project Number | Project Name   | Priority | Task                  |\n"
          "|------+----------------+----------------+----------+-----------------------|\n"
          "| 0:   | 8721           | NoahlanaMonzon | [#A1]    | Read the key reviews. |\n"
          "| -    |                |                |          | Tidy the bib file     |\n")
  "The legend, two lists, and the grasscatcher, all in columns.")

(defconst writing-habit-columns-tests--file
  (concat writing-habit-columns-tests--grid writing-habit-columns-tests--tables)
  "A whole weekly table file in the column layout.")

;;;; One row at a time

(ert-deftest writing-habit-columns/project-cell ()
  "A row of columns reads as the one-cell form."
  (should (equal (writing-habit-columns-project-cell
                  '("0:" "8721" "NoahlanaMonzon" "2026-10-19" ":safe:" ""))
                 "0: 8721NoahlanaMonzon 2026-10-19 :safe:"))
  (should (equal (writing-habit-columns-project-cell
                  '("-" "1452" "PHFSboXt" "2027-03-01" "risky" ""))
                 "- 1452PHFSboXt 2027-03-01 :risky:"))
  (should (equal (writing-habit-columns-project-cell
                  '("B" "1006" "AIrxOpt" "" ":safe:" "@editing"))
                 "B: 1006AIrxOpt @editing :safe:"))
  (should (equal (writing-habit-columns-project-cell '("E:" "" "email" "" "" "support"))
                 "E: email @support"))
  (should (equal (writing-habit-columns-project-cell
                  '("V:" "0379" "pymolSceneManager" "2026-12- 31" ":risky:" ""))
                 "V: 0379pymolSceneManager 2026-12-31 :risky:"))
  (should-not (writing-habit-columns-project-cell '("" "" "" "" "" ""))))

(ert-deftest writing-habit-columns/project-round-trip ()
  "The one-cell form survives a trip through the columns."
  (dolist (cell '("0: 8721NoahlanaMonzon 2026-10-19 :safe:"
                  "- 1452PHFSboXt 2027-03-01 :risky:"
                  "B: 1006AIrxOpt @editing :safe:"
                  "Y: 2026words :risky:" "E: email"))
    (should (equal (writing-habit-columns-project-cell
                    (writing-habit-columns-project-columns cell))
                   cell))))

(ert-deftest writing-habit-columns/task-rows ()
  "A grasscatcher row reads and writes with its priority cookie and project number."
  (should (equal (writing-habit-columns-task-cell '("1:" "0142" "RET" "[#A1]" "Read it."))
                 "1: [#A1] Read it."))
  (should (equal (writing-habit-columns-task-cell '("-" "" "" "B2" "tidy")) "- [#B2] tidy"))
  ;; A row the Python editor wrote with the number after the code.
  (should (equal (writing-habit-columns-task-columns "1: 0142 [#A1] Read it."
                                                     '(("1" . ("0142" . "RET"))))
                 '("1:" "0142" "RET" "[#A1]" "Read it.")))
  ;; A row from before numbers were written still finds its number by code.
  (should (equal (writing-habit-columns-task-columns "1: [#A1] Read it."
                                                     '(("1" . ("0142" . "RET"))))
                 '("1:" "0142" "RET" "[#A1]" "Read it.")))
  ;; The number wins over a code that now names another project.
  (should (equal (writing-habit-columns-task-columns "A: 0142 Read it."
                                                     '(("A" . ("1006" . "AIrxOpt"))
                                                       ("1" . ("0142" . "RET"))))
                 '("A:" "0142" "RET" "" "Read it.")))
  ;; A year at the start of a task is not a project number.
  (should (equal (writing-habit-columns-task-columns "E: 2026 budget"
                                                     '(("E" . ("" . "email"))))
                 '("E:" "" "email" "" "2026 budget"))))

(ert-deftest writing-habit-columns/render ()
  "A rendered table is aligned as org aligns it."
  (should (equal (writing-habit-columns-render '("Code" "Task") '(("B:" "a long task")))
                 '("| Code | Task        |"
                   "|------+-------------|"
                   "| B:   | a long task |"))))

;;;; The whole file

(ert-deftest writing-habit-columns/table-reads-into-the-model ()
  "The legend, the lists, and the grasscatcher reach the one-cell model."
  (skip-unless writing-habit-columns-tests--ok)
  (let ((table (writing-habit-table-from-text writing-habit-columns-tests--file)))
    (should (writing-habit-table-columnar table))
    (should (equal (writing-habit-table-legend table)
                   '(("0" "8721NoahlanaMonzon 2026-10-19" "safe")
                     ("B" "1006AIrxOpt 2026-12-15" "safe"))))
    (should (equal (writing-habit-table-legend-defaults table) '(("B" . "editing"))))
    (should (equal (mapcar #'writing-habit-lists-entry-project
                           (writing-habit-table-list-entries table writing-habit-lists-current))
                   '("1094RNAstrucDet" "1452PHFSboXt" "2026words")))
    (should (equal (mapcar #'writing-habit-lists-entry-description
                           (writing-habit-lists-entries (writing-habit-table-after table)
                                                        "grasscatcher"))
                   '("[#A1] Read the key reviews." "Tidy the bib file")))))

(ert-deftest writing-habit-columns/untouched-file-saves-unchanged ()
  "An untouched file in columns comes back byte for byte."
  (skip-unless writing-habit-columns-tests--ok)
  (should (equal (writing-habit-table-to-text
                  (writing-habit-table-from-text writing-habit-columns-tests--file))
                 writing-habit-columns-tests--file)))

(ert-deftest writing-habit-columns/an-edit-writes-columns-again ()
  "After an edit every table is written in columns, and reads back the same."
  (skip-unless writing-habit-columns-tests--ok)
  (let* ((table (writing-habit-table-from-text writing-habit-columns-tests--file))
         (row (nth 1 (writing-habit-table-block-rows table)))
         (column (car (nth 1 (writing-habit-table-columns table)))))
    (writing-habit-table-set-cell table row column "gB")
    (let ((text (writing-habit-table-to-text table)))
      (should (string-match-p "| e0 | gB |" text))
      (should (string-match-p (regexp-quote "| B:   | 1006           | AIrxOpt        | 2026-12-15 | :safe:   | editing          |") text))
      (should (string-match-p "#\\+NAME: grasscatcher\n#\\+CAPTION: Grasscatcher\n| Code | Project Number" text))
      (should (string-match-p "#\\+CAPTION: Fall 2026 projects" text))
      (should-not (string-match-p "| 0: 8721" text))
      (let ((again (writing-habit-table-from-text text)))
        (should (equal (writing-habit-table-legend again) (writing-habit-table-legend table)))
        (should (equal (mapcar #'writing-habit-lists-entry-text
                               (writing-habit-table-list-entries again writing-habit-lists-current))
                       (mapcar #'writing-habit-lists-entry-text
                               (writing-habit-table-list-entries table writing-habit-lists-current))))))))

(ert-deftest writing-habit-columns/the-scheduler-reads-the-columns ()
  "writing-schedule reads the This week legend of the file."
  (skip-unless (and writing-habit-columns-tests--ok (fboundp 'writing-schedule-column-legend)))
  (let ((parsed (writing-schedule-parse-text writing-habit-columns-tests--file)))
    (should (equal (plist-get parsed :legend)
                   '(("0" . "8721NoahlanaMonzon 2026-10-19 :safe:")
                     ("B" . "1006AIrxOpt 2026-12-15 :safe:"))))
    (should (= (length (plist-get parsed :events)) 3))))

(ert-deftest writing-habit-columns/readers-that-take-a-file ()
  "The legend reader and the semester numbers read a file in columns."
  (skip-unless writing-habit-columns-tests--ok)
  (require 'writing-habit-track)
  (let ((file (make-temp-file "wh-columns" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file (insert writing-habit-columns-tests--file))
          (should (equal (mapcar #'car (writing-habit-name-read-legend file)) '("0" "B")))
          (let ((numbers (writing-habit-track-semester-numbers file)))
            (dolist (n '("1094" "1452" "3292")) (should (member n numbers)))))
      (delete-file file))))

(ert-deftest writing-habit-columns/titles-name-a-list ()
  "A list named by its plain tab title is found under its standard name."
  (let ((lines '("#+NAME: Current semester projects" "| A: one |"
                 "#+NAME:  3*Next semester projects" "| B: two |")))
    (should (equal (mapcar #'writing-habit-lists-entry-description
                           (writing-habit-lists-entries lines writing-habit-lists-current))
                   '("one")))
    (should (equal (mapcar #'writing-habit-lists-entry-description
                           (writing-habit-lists-entries lines "next-next-next-semester-projects"))
                   '("two")))))

;;;; The editing mode

(defmacro writing-habit-columns-tests--in-buffer (&rest body)
  "Run BODY in a buffer visiting a copy of the column file, with the mode on."
  (declare (indent 0))
  `(progn
     (skip-unless writing-habit-columns-tests--ok)
     (let* ((dir (make-temp-file "wh-columns" t))
            (file (expand-file-name "Oct19.org" dir)))
       (with-temp-file file (insert writing-habit-columns-tests--file))
       (let ((buf (find-file-noselect file)))
         (unwind-protect
             (with-current-buffer buf
               (writing-habit-table-mode 1)
               ,@body)
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)
           (delete-directory dir t))))))

(defun writing-habit-columns-tests--goto (text)
  "Move to the start of the line that holds TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(ert-deftest writing-habit-columns/mode-finds-the-legend-and-list-rows ()
  "Point on a row of the This week table or of a list maps to the model."
  (writing-habit-columns-tests--in-buffer
    (let ((model (writing-habit-table-current)))
      (writing-habit-columns-tests--goto "| B:   | 1006")
      (let ((row (writing-habit-table--row-at-point model)))
        (should (eq (writing-habit-table--kind model row) 'legend))
        (should (equal (car (writing-habit-table-row-parsed (writing-habit-table-row-at model row)))
                       "B")))
      (writing-habit-columns-tests--goto "| -    | 1452")
      (should (equal (writing-habit-table--list-at-point model)
                     (cons writing-habit-lists-current 1)))
      (writing-habit-columns-tests--goto "|     04:00-06:00")
      (should (eq (writing-habit-table--kind model (writing-habit-table--row-at-point model))
                  'block)))))

(ert-deftest writing-habit-columns/mode-copy-to-legend ()
  "A list project copied into the week lands in the This week table."
  (writing-habit-columns-tests--in-buffer
    (writing-habit-columns-tests--goto "| -    | 1452")
    (writing-habit-table-copy-to-legend)
    (should (string-prefix-p "| A:   | 1452" (buffer-substring (line-beginning-position)
                                                            (line-end-position))))
    (let ((text (buffer-string)))
      (should (< (string-match "| A:   | 1452" text)
                 (string-match "#\\+NAME: current-semester-projects" text)))
      (should (string-match-p "#\\+CAPTION: Fall 2026 projects" text)))))

(ert-deftest writing-habit-columns/mode-mark-completed ()
  "Mark completed moves a project between two lists in columns."
  (writing-habit-columns-tests--in-buffer
    (writing-habit-columns-tests--goto "| A:   | 1094")
    (writing-habit-table-mark-completed)
    (let ((model (writing-habit-table-current)))
      (should (equal (mapcar #'writing-habit-lists-entry-project
                             (writing-habit-table-list-entries model writing-habit-lists-completed))
                     '("3292ABRF-SC3-2026" "1094RNAstrucDet"))))
    (should (string-match-p "| A:   | 1094           | RNAstrucDet   |" (buffer-string)))))

(ert-deftest writing-habit-columns/mode-sort-list ()
  "Sort by number orders the list that holds point, in columns."
  (writing-habit-columns-tests--in-buffer
    (writing-habit-columns-tests--goto "| Y:   | 2026")
    (writing-habit-table-sort-by-number)
    (let ((model (writing-habit-table-current)))
      (should (equal (mapcar #'writing-habit-lists-entry-project
                             (writing-habit-table-list-entries model writing-habit-lists-current))
                     '("1094RNAstrucDet" "1452PHFSboXt" "2026words"))))))

(provide 'writing-habit-columns-tests)
;;; writing-habit-columns-tests.el ends here
