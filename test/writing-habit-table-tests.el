;;; writing-habit-table-tests.el --- ERT tests for the weekly table model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ports the model tests of the Python package's graphical layer
;; (test_gui_weekly_table, test_gui_table_editing, test_gui_insert_row,
;; test_gui_move_row, test_gui_insert_project, test_gui_move_project,
;; test_gui_legend_sync, test_gui_time_tint, and test_gui_cell_tooltip).
;; The model needs writing-schedule.el 0.3.1 on the load-path, and the
;; shipped templates are read from its templates directory.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst writing-habit-table-tests--have-schedule
  (and (require 'writing-schedule nil t) (fboundp 'writing-schedule-split-row))
  "Non-nil when writing-schedule.el 0.3.1 is available.")

(when writing-habit-table-tests--have-schedule
  (require 'writing-habit-table))

(defconst writing-habit-table-tests--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this file.")

(defun writing-habit-table-tests--fixture (name)
  "Return the path of the fixture NAME."
  (expand-file-name (concat "fixtures/" name) writing-habit-table-tests--dir))

(defun writing-habit-table-tests--templates ()
  "Return the shipped weekly templates from the writing-schedule checkout."
  (let ((lib (locate-library "writing-schedule")))
    (when lib
      (directory-files (expand-file-name "templates" (file-name-directory lib))
                       t "\\.org\\'"))))

(defun writing-habit-table-tests--slurp (path)
  "Return the contents of PATH as UTF-8 text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents path))
    (buffer-string)))

(defmacro writing-habit-table-tests--deftest (name doc &rest body)
  "Define an ERT test NAME with DOC that skips without writing-schedule.
BODY runs with `example' bound to the named example week."
  (declare (indent 1) (doc-string 2))
  `(ert-deftest ,(intern (format "writing-habit-table/%s" name)) ()
     ,doc
     (skip-unless writing-habit-table-tests--have-schedule)
     (let ((example (writing-habit-table-from-file
                     (writing-habit-table-tests--fixture "my-week-named.org"))))
       (ignore example)
       ,@body)))

(defun writing-habit-table-tests--sections (table)
  "Return the indexes of TABLE's section rows."
  (writing-habit-table--indexes table '(section)))

(defun writing-habit-table-tests--lines (table)
  "Return TABLE's text as a list of lines."
  (butlast (split-string (writing-habit-table-to-text table) "\n")))

(defconst writing-habit-table-tests--clash
  (concat "| Time <l>    | M  | Tu |\n"
          "|-------------+----+----|\n"
          "| Generative: |    |    |\n"
          "| 04:00-05:30 | A  |    |\n"
          "| Rewriting:  |    |    |\n"
          "| 05:00-06:30 | B  | A  |\n"
          "|-------------+----+----|\n"
          "| A: one :safe: |  |  |\n"
          "| B: two        |  |  |\n")
  "A table with one clash on Monday.")

(defconst writing-habit-table-tests--faults
  (concat "| Time <l>    | M |\n"
          "|-------------+---|\n"
          "| Generative: |   |\n"
          "| 04:00-05:30 | H |\n"
          "|-------------+---|\n"
          "| H: first project :safe:  |  |\n"
          "| H: second project :safe: |  |\n"
          "| G: an aim :spec:         |  |\n")
  "A legend with a duplicate code and an unread tag.")

;;;; Agreement with the scheduler and the tracker

(writing-habit-table-tests--deftest templates-parse-like-the-scheduler
  "Every shipped template gives the scheduler's events and round-trips."
  (let ((files (append (writing-habit-table-tests--templates)
                       (list (writing-habit-table-tests--fixture "my-week-named.org")
                             (expand-file-name "../examples/aug24.org"
                                               writing-habit-table-tests--dir)))))
    (should (> (length files) 10))
    (dolist (f files)
      (let* ((text (writing-habit-table-tests--slurp f))
             (table (writing-habit-table-from-text text f)))
        (should (equal (writing-habit-table-to-text table) text))
        (should (equal (writing-habit-table-events table)
                       (plist-get (writing-schedule-parse-text text) :events)))
        (should (equal (writing-habit-table-legend table)
                       (writing-habit-name-read-legend f)))))))

(writing-habit-table-tests--deftest templates-name-themselves
  "The code of each template's grid decodes to the week its name gives."
  (dolist (f (writing-habit-table-tests--templates))
    (let ((result (writing-habit-table-code-or-problem
                   (writing-habit-table-from-file f))))
      (should (car result))
      (should (equal (writing-habit-name-decode (car result))
                     (writing-habit-name-decode (file-name-base f)))))))

(writing-habit-table-tests--deftest multi-letter-project-has-no-name
  "A file name gives one letter to one block, so EM cannot be named."
  (let* ((table (writing-habit-table-from-text
                 (concat "| Time <l>    | M  |\n|-------------+----|\n"
                         "| Supporting: |    |\n| 13:15-14:45 | EM |\n"
                         "|-------------+----|\n| EM: email   |    |\n")))
         (result (writing-habit-table-code-or-problem table)))
    (should-not (car result))
    (should (string-match-p "EM" (cdr result)))
    (should (string-match-p "single-letter alias" (cdr result)))))

(writing-habit-table-tests--deftest totals
  "Totals by day, project, and activity agree with each other."
  (let ((totals (writing-habit-table-totals example)))
    (should (= (cdr (assoc "M" (plist-get totals :day))) 360))
    (should (= (cdr (assoc "Sa" (plist-get totals :day))) 180))
    (should (= (cdr (assoc "A" (plist-get totals :project))) 720))
    (should (equal (plist-get totals :category)
                   '(("generative" . 990) ("editing" . 540) ("support" . 450))))))

(writing-habit-table-tests--deftest legend-check-resolves-example
  "Every letter of the example resolves in its legend."
  (let ((check (writing-habit-table-legend-check example)))
    (should (null (cadr check)))
    (should (equal (sort (mapcar #'car (car check)) #'string<)
                   '("A" "B" "E" "T" "W")))))

(writing-habit-table-tests--deftest clash-found-and-located
  "A clash is found and both of its cells are located."
  (let ((table (writing-habit-table-from-text writing-habit-table-tests--clash)))
    (should (= (length (writing-habit-table-overlaps table)) 1))
    (let ((cells (writing-habit-table-conflicting-cells table)))
      (should (= (length cells) 2))
      (should (= (length (delete-dups (mapcar #'car cells))) 2))
      (should (seq-every-p (lambda (c) (= (cdr c) 1)) cells)))
    (should (string-match-p "\\`Monday: 04:00-05:30 \\[A, Generative\\] overlaps"
                            (car (writing-habit-table-overlap-lines table))))))

(writing-habit-table-tests--deftest touching-blocks-do-not-clash
  "Blocks that only touch do not clash."
  (let ((table (writing-habit-table-from-text
                (string-replace "05:00-06:30" "05:30-07:00"
                                writing-habit-table-tests--clash))))
    (should (null (writing-habit-table-overlaps table)))
    (should (null (writing-habit-table-conflicting-cells table)))))

(writing-habit-table-tests--deftest unknown-section-reported
  "A section with no activity is reported and counted as generative."
  (let ((table (writing-habit-table-from-text
                (string-replace "Rewriting:" "Pondering:"
                                writing-habit-table-tests--clash))))
    (should (equal (writing-habit-table-unknown-sections table) '("Pondering")))
    (should (= (cdr (assoc "generative" (plist-get (writing-habit-table-totals table)
                                                   :category)))
               270))))

(writing-habit-table-tests--deftest keeps-text-around-table
  "The lines before and after the table survive."
  (let* ((text (concat "#+TITLE: A week\n\n" writing-habit-table-tests--clash
                       "\nA closing note.\n"))
         (table (writing-habit-table-from-text text)))
    (should (equal (writing-habit-table-to-text table) text))
    (should (equal (writing-habit-table-day-labels table) '("M" "Tu")))))

(writing-habit-table-tests--deftest real-week
  "Only lettered cells of the real week count, and it names itself."
  (let ((table (writing-habit-table-from-file
                (expand-file-name "../examples/aug24.org" writing-habit-table-tests--dir))))
    (should (= (length (writing-habit-table-block-rows table)) 126))
    (should (= (length (writing-habit-table-blocks table)) 5))
    (should (= (cdr (assoc "Th" (plist-get (writing-habit-table-totals table) :day))) 570))
    (should (equal (car (writing-habit-table-code-or-problem table)) "3o-gHgAeCsIsB"))
    (should (null (writing-habit-table-duplicate-legend-codes table)))
    (should (equal (nth 2 (assoc "G" (writing-habit-table-legend table))) "speculative"))
    (should (equal (nth 1 (assoc "H" (writing-habit-table-legend table))) "1006AIrxOpt"))))

(writing-habit-table-tests--deftest legend-faults
  "Duplicate codes and unread tags are reported."
  (let ((table (writing-habit-table-from-text writing-habit-table-tests--faults)))
    (should (equal (writing-habit-table-duplicate-legend-codes table)
                   '(("H" "first project" "second project"))))
    (should (equal (nth 1 (assoc "H" (writing-habit-table-legend table))) "first project"))
    (should (equal (writing-habit-table-stray-risk-tags table) '(("G" . "spec")))))
  (let ((table (writing-habit-table-from-text
                (string-replace ":spec:" ":risky:" writing-habit-table-tests--faults))))
    (should (equal (nth 2 (assoc "G" (writing-habit-table-legend table))) "speculative"))
    (should (null (writing-habit-table-stray-risk-tags table))))
  (let ((table (writing-habit-table-from-text
                (string-replace ":spec:" ":speculative:" writing-habit-table-tests--faults))))
    (should (null (nth 2 (assoc "G" (writing-habit-table-legend table)))))
    (should (equal (writing-habit-table-stray-risk-tags table) '(("G" . "speculative"))))))

;;;; Cell and legend edits

(writing-habit-table-tests--deftest same-value-changes-nothing
  "Writing a cell its own value changes nothing."
  (let* ((row (car (writing-habit-table-block-rows example)))
         (col (car (car (writing-habit-table-columns example))))
         (text (writing-habit-table-to-text example)))
    (should-not (writing-habit-table-set-cell
                 example row col (writing-habit-table-cell example row col)))
    (should (equal (writing-habit-table-to-text example) text))
    (should-not (writing-habit-table-dirty example))))

(writing-habit-table-tests--deftest one-cell-edit-one-line
  "One cell edit rewrites one line and keeps its width when it fits."
  (let* ((row (car (writing-habit-table-block-rows example)))
         (col (car (car (writing-habit-table-columns example))))
         (before (writing-habit-table-tests--lines example))
         (raw (writing-habit-table-row-raw (writing-habit-table-row-at example row))))
    (should (writing-habit-table-set-cell example row col "b"))
    (let ((after (writing-habit-table-tests--lines example)))
      (should (= (length before) (length after)))
      (should (= 1 (cl-count nil (cl-mapcar #'equal before after))))
      (should (= (length raw)
                 (length (writing-habit-table-row-raw
                          (writing-habit-table-row-at example row))))))
    (should (equal (writing-habit-table-cell example row col) "B"))
    (should (writing-habit-table-dirty example))))

(writing-habit-table-tests--deftest longer-value-widens-own-slot
  "A value too long for its slot widens that slot alone."
  (let* ((row (car (writing-habit-table-block-rows example)))
         (col (car (car (writing-habit-table-columns example))))
         (raw (writing-habit-table-row-raw (writing-habit-table-row-at example row))))
    (writing-habit-table-set-cell example row col "ZEBRA")
    (let ((new (writing-habit-table-row-raw (writing-habit-table-row-at example row))))
      (should (string-match-p "| ZEBRA |" new))
      (should (equal (nth 1 (split-string new "|")) (nth 1 (split-string raw "|")))))))

(writing-habit-table-tests--deftest only-block-row-takes-cell
  "Only a time-block row takes a cell edit."
  (should-error (writing-habit-table-set-cell
                 example (car (writing-habit-table-legend-rows example)) 1 "A")))

(writing-habit-table-tests--deftest legend-edit-writes-risky
  "A legend edit rewrites one line and writes :risky: for speculative."
  (let* ((row (car (writing-habit-table-legend-rows example)))
         (before (writing-habit-table-tests--lines example)))
    (should (writing-habit-table-set-legend example row "A" "New name" "speculative"))
    (should (= 1 (cl-count nil (cl-mapcar #'equal before
                                          (writing-habit-table-tests--lines example)))))
    (should (string-prefix-p "A: New name :risky:"
                             (car (writing-habit-table-row-cells
                                   (writing-habit-table-row-at example row)))))
    (should (equal (writing-habit-table-row-parsed (writing-habit-table-row-at example row))
                   '("A" "New name" "speculative")))
    (writing-habit-table-set-legend example row "A" "New name" nil)
    (should (equal (car (writing-habit-table-row-cells (writing-habit-table-row-at example row)))
                   "A: New name"))))

(writing-habit-table-tests--deftest save-and-rename
  "Saving clears the dirty flag and a drifted name is corrected."
  (let* ((dir (make-temp-file "wh-table" t))
         (path (expand-file-name "drifted.org" dir)))
    (unwind-protect
        (progn
          (copy-file (writing-habit-table-tests--fixture "my-week-named.org") path)
          (let ((table (writing-habit-table-from-file path)))
            (should-not (writing-habit-table-name-matches-code table))
            (writing-habit-table-set-cell table (car (writing-habit-table-block-rows table))
                                          1 "B")
            (should-error (writing-habit-table-rename-to-canonical table))
            (writing-habit-table-save table)
            (should-not (writing-habit-table-dirty table))
            (let ((new (writing-habit-table-rename-to-canonical table)))
              (should (file-exists-p new))
              (should-not (file-exists-p path))
              (should (writing-habit-table-name-matches-code table))
              (should (equal (writing-habit-table-rename-to-canonical table) new)))))
      (delete-directory dir t))))

(writing-habit-table-tests--deftest rename-refuses-to-overwrite
  "Renaming refuses to overwrite another file and needs a file."
  (let* ((dir (make-temp-file "wh-table" t))
         (path (expand-file-name "drifted.org" dir)))
    (unwind-protect
        (progn
          (copy-file (writing-habit-table-tests--fixture "my-week-named.org") path)
          (let* ((table (writing-habit-table-from-file path))
                 (code (car (writing-habit-table-code-or-problem table))))
            (write-region "x" nil (expand-file-name (concat code ".org") dir))
            (should-error (writing-habit-table-rename-to-canonical table))
            (should (file-exists-p path))))
      (delete-directory dir t)))
  (should-error (writing-habit-table-rename-to-canonical
                 (writing-habit-table-from-text writing-habit-table-tests--clash))))

;;;; Inserting time blocks

(writing-habit-table-tests--deftest insert-below-adds-one-line
  "Insert below adds exactly one line and leaves the rest alone."
  (let* ((before (writing-habit-table-tests--lines example))
         (near (car (writing-habit-table-block-rows example)))
         (at (writing-habit-table-insert-block example near nil "05:30" "05:45"))
         (after (writing-habit-table-tests--lines example))
         (offset (length (writing-habit-table-before example))))
    (should (= at (1+ near)))
    (should (= (length after) (1+ (length before))))
    (should (string-prefix-p "| 05:30-05:45" (nth (+ offset at) after)))
    (should (equal (append (seq-take after (+ offset at)) (nthcdr (+ offset at 1) after))
                   before))
    (should (writing-habit-table-dirty example))))

(writing-habit-table-tests--deftest insert-above-and-width
  "Insert above takes the row's place and copies its neighbour's width."
  (let* ((near (cadr (writing-habit-table-block-rows example)))
         (at (writing-habit-table-insert-block example near t "05:30" "05:45")))
    (should (= at near))
    (should (equal (writing-habit-table-row-parsed (writing-habit-table-row-at example (1+ at)))
                   '("05:45" . "07:15")))
    (should (= (length (writing-habit-table-row-raw (writing-habit-table-row-at example at)))
               (length (writing-habit-table-row-raw (writing-habit-table-row-at example (1+ at))))))
    (should (seq-every-p #'string-empty-p
                         (cdr (writing-habit-table-row-cells
                               (writing-habit-table-row-at example at)))))))

(writing-habit-table-tests--deftest insert-beside-section-headers
  "A block above a header joins the section before, below joins that section."
  (let* ((rewriting (nth 1 (writing-habit-table-tests--sections example)))
         (at (writing-habit-table-insert-block example rewriting t "07:30" "08:00")))
    (should (equal (writing-habit-table-row-section (writing-habit-table-row-at example at))
                   "Generative")))
  (let* ((table (writing-habit-table-from-file
                 (writing-habit-table-tests--fixture "my-week-named.org")))
         (rewriting (nth 1 (writing-habit-table-tests--sections table)))
         (at (writing-habit-table-insert-block table rewriting nil "08:00" "09:00"))
         (reread (writing-habit-table-from-text (writing-habit-table-to-text table))))
    (should (equal (writing-habit-table-row-section (writing-habit-table-row-at table at))
                   "Rewriting"))
    (should (equal (writing-habit-table-row-section (writing-habit-table-row-at reread at))
                   "Rewriting"))))

(writing-habit-table-tests--deftest bad-ranges-refused
  "A bad time range and a legend anchor are refused without a change."
  (let ((text (writing-habit-table-to-text example))
        (near (car (writing-habit-table-block-rows example))))
    (dolist (range '(("5:30" . "abc") ("10:00" . "09:00") ("" . "")))
      (should-error (writing-habit-table-insert-block example near nil (car range) (cdr range))))
    (should (equal (writing-habit-table-to-text example) text))
    (should-not (writing-habit-table-dirty example))
    (should-error (writing-habit-table-insert-block
                   example (car (writing-habit-table-legend-rows example)) nil "05:30" "05:45"))))

(writing-habit-table-tests--deftest suggested-times
  "Suggested times sit flush against the neighbouring block."
  (let ((blocks (writing-habit-table-block-rows example))
        (rewriting (nth 1 (writing-habit-table-tests--sections example))))
    (should (equal (writing-habit-table-suggest-times example (car blocks) t) '("02:30" . "04:00")))
    (should (equal (writing-habit-table-suggest-times example (cadr blocks) nil) '("07:15" . "08:45")))
    (should (equal (writing-habit-table-suggest-times example rewriting t) '("07:15" . "08:45")))
    (should (equal (writing-habit-table-suggest-times example rewriting nil) '("07:45" . "09:15"))))
  (let ((table (writing-habit-table-from-text "| Time | M |\n|------+---|\n| 23:00-23:45 | A |\n")))
    (should (equal (writing-habit-table-suggest-times
                    table (car (writing-habit-table-block-rows table)) nil)
                   '("23:45" . "24:00")))))

(writing-habit-table-tests--deftest left-padded-time-stays-left-padded
  "A Time column padded on the left stays so for a new row."
  (let* ((table (writing-habit-table-from-text
                 (concat "|          Time | M | Tu |\n|---------------+---+----|\n"
                         "| Generative:   |   |    |\n|   04:00-05:30 | A | B  |\n"
                         "|---------------+---+----|\n| A: one        |   |    |\n"
                         "| B: two        |   |    |\n")))
         (at (writing-habit-table-insert-block
              table (car (writing-habit-table-block-rows table)) nil "05:30" "05:45")))
    (should (equal (nth 1 (split-string (writing-habit-table-row-raw
                                         (writing-habit-table-row-at table at))
                                        "|"))
                   "   05:30-05:45 "))))

;;;; Moving time blocks

(writing-habit-table-tests--deftest move-down-swaps-two-lines
  "Moving down inside a section swaps two lines, and up undoes it."
  (let* ((text (writing-habit-table-to-text example))
         (blocks (writing-habit-table-block-rows example))
         (before (writing-habit-table-tests--lines example))
         (at (writing-habit-table-move-block example (car blocks) nil)))
    (should (= at (cadr blocks)))
    (should (= 2 (cl-count nil (cl-mapcar #'equal before
                                          (writing-habit-table-tests--lines example)))))
    (should (equal (sort (copy-sequence before) #'string<)
                   (sort (writing-habit-table-tests--lines example) #'string<)))
    (writing-habit-table-move-block example at t)
    (should (equal (writing-habit-table-to-text example) text))))

(writing-habit-table-tests--deftest move-across-headers
  "A block passing a header joins the neighbouring section and its activity."
  (let* ((before (plist-get (writing-habit-table-totals example) :category))
         (rewriting (nth 1 (writing-habit-table-tests--sections example)))
         (at (writing-habit-table-move-block
              example (cadr (writing-habit-table-block-rows example)) nil)))
    (should (= at rewriting))
    (should (equal (writing-habit-table-row-section (writing-habit-table-row-at example at))
                   "Rewriting"))
    (let ((after (plist-get (writing-habit-table-totals example) :category)))
      (should (= (cdr (assoc "generative" after)) (- (cdr (assoc "generative" before)) 540)))
      (should (= (cdr (assoc "editing" after)) (+ (cdr (assoc "editing" before)) 540))))
    (let ((back (writing-habit-table-move-block example at t)))
      (should (equal (writing-habit-table-row-section (writing-habit-table-row-at example back))
                     "Generative")))))

(writing-habit-table-tests--deftest move-limits
  "Nothing moves above the first section or below the last block."
  (let ((blocks (writing-habit-table-block-rows example))
        (text (writing-habit-table-to-text example)))
    (should-not (writing-habit-table-can-move example (car blocks) t))
    (should-error (writing-habit-table-move-block example (car blocks) t))
    (should-not (writing-habit-table-can-move example (car (last blocks)) nil))
    (should-not (writing-habit-table-can-move
                 example (nth 1 (writing-habit-table-tests--sections example)) t))
    (should-not (writing-habit-table-can-move
                 example (car (writing-habit-table-legend-rows example)) nil))
    (should (equal (writing-habit-table-to-text example) text))))

(writing-habit-table-tests--deftest table-without-sections-moves-freely
  "A table with no sections moves rows freely."
  (let* ((table (writing-habit-table-from-text
                 "| Time | M |\n|------+---|\n| 09:00-10:00 | A |\n| 10:00-11:00 | B |\n"))
         (blocks (writing-habit-table-block-rows table)))
    (should (= (writing-habit-table-move-block table (cadr blocks) t) (car blocks)))
    (should (equal (mapcar (lambda (i) (writing-habit-table-row-parsed
                                        (writing-habit-table-row-at table i)))
                           (writing-habit-table-block-rows table))
                   '(("10:00" . "11:00") ("09:00" . "10:00"))))))

;;;; The legend

(writing-habit-table-tests--deftest insert-project
  "A project is inserted beside the selected entry with its neighbour's width."
  (let* ((legend (writing-habit-table-legend-rows example))
         (near (car legend))
         (before (writing-habit-table-tests--lines example))
         (at (writing-habit-table-insert-legend example near nil "c" "Third" "safe")))
    (should (= at (1+ near)))
    (should (= (length (writing-habit-table-tests--lines example)) (1+ (length before))))
    (should (equal (writing-habit-table-row-parsed (writing-habit-table-row-at example at))
                   '("C" "Third" "safe")))
    (should (= (length (writing-habit-table-row-raw (writing-habit-table-row-at example at)))
               (length (writing-habit-table-row-raw (writing-habit-table-row-at example near)))))
    (should (= (writing-habit-table-insert-legend example near t "D") near))
    (should (= (writing-habit-table-insert-legend example nil nil "F")
               (car (last (writing-habit-table-legend-rows example)))))))

(writing-habit-table-tests--deftest insert-project-refusals
  "A bad code, a duplicate, and a non-legend anchor are refused."
  (dolist (code '("" "1A" "ABCDE" "a b"))
    (should-error (writing-habit-table-insert-legend example nil nil code)))
  (should-error (writing-habit-table-insert-legend example nil nil "A"))
  (should-error (writing-habit-table-insert-legend
                 example (car (writing-habit-table-block-rows example)) nil "Q")))

(writing-habit-table-tests--deftest next-free-code
  "The next free code skips the legend and the grid."
  (should (equal (writing-habit-table-next-free-code example) "C"))
  (writing-habit-table-set-cell example (car (writing-habit-table-block-rows example)) 1 "C")
  (should (equal (writing-habit-table-next-free-code example) "D")))

(writing-habit-table-tests--deftest blank-project-read-by-both-readers
  "A blank project row is a legend row to both the editor and the scheduler."
  (writing-habit-table-insert-legend example nil nil "C")
  (let ((text (writing-habit-table-to-text example)))
    (should (assoc "C" (writing-habit-table-legend (writing-habit-table-from-text text))))
    (should (assoc "C" (plist-get (writing-schedule-parse-text text) :legend)))))

(writing-habit-table-tests--deftest sync-legend
  "Sync adds a row for a new code and drops only its own blank rows."
  (let* ((row (car (writing-habit-table-block-rows example)))
         (old (writing-habit-table-cell example row 1)))
    (writing-habit-table-insert-legend example nil nil "C")
    (writing-habit-table-set-cell example row 1 "Q")
    (should (writing-habit-table-sync-legend example))
    (should (assoc "Q" (writing-habit-table-legend example)))
    (should-not (writing-habit-table-sync-legend example))
    (writing-habit-table-set-cell example row 1 old)
    (should (writing-habit-table-sync-legend example))
    (should-not (assoc "Q" (writing-habit-table-legend example)))
    (should (assoc "C" (writing-habit-table-legend example)))))

(writing-habit-table-tests--deftest sync-starts-a-legend
  "A table with no legend gains one when a code appears."
  (let ((table (writing-habit-table-from-text
                "| Time | M |\n|------+---|\n| 09:00-10:00 | A |\n")))
    (should (writing-habit-table-sync-legend table))
    (should (equal (mapcar #'car (writing-habit-table-legend table)) '("A")))
    (should (assoc "A" (plist-get (writing-schedule-parse-text
                                   (writing-habit-table-to-text table))
                                  :legend)))))

(writing-habit-table-tests--deftest move-project
  "A project trades places with its neighbour and the grid is untouched."
  (let* ((legend (writing-habit-table-legend-rows example))
         (text (writing-habit-table-to-text example))
         (events (writing-habit-table-events example))
         (at (writing-habit-table-move-legend example (car legend) nil)))
    (should (= at (cadr legend)))
    (should (equal (writing-habit-table-events example) events))
    (should-not (writing-habit-table-can-move-legend example (car (last legend)) nil))
    (should-error (writing-habit-table-move-legend
                   example (car (writing-habit-table-block-rows example)) t))
    (writing-habit-table-move-legend example at t)
    (should (equal (writing-habit-table-to-text example) text))))

(writing-habit-table-tests--deftest order-decides-duplicate
  "Moving the second definition above the first changes which one wins."
  (let* ((table (writing-habit-table-from-text writing-habit-table-tests--faults))
         (second (cadr (writing-habit-table-legend-rows table))))
    (writing-habit-table-move-legend table second t)
    (should (equal (nth 1 (assoc "H" (writing-habit-table-legend table))) "second project"))))

;;;; Clear times and tooltips

(defconst writing-habit-table-tests--day
  (concat "| Time <l>    | M |\n|-------------+---|\n| Generative: |   |\n"
          "| 04:00-05:30 | A |\n| 05:30-07:00 | B |\n| 05:00-06:00 |   |\n"
          "| Supporting: |   |\n| 23:00-01:00 |   |\n| 00:30-02:00 |   |\n")
  "A day of touching, overlapping, and overnight blocks.")

(writing-habit-table-tests--deftest rows-clear-of
  "Touching blocks are clear, overlaps are not, and overnight runs on."
  (let* ((table (writing-habit-table-from-text writing-habit-table-tests--day))
         (b (writing-habit-table-block-rows table)))
    (should (memq (nth 1 b) (writing-habit-table-rows-clear-of table (nth 0 b))))
    (should-not (memq (nth 2 b) (writing-habit-table-rows-clear-of table (nth 0 b))))
    (should-not (memq (nth 0 b) (writing-habit-table-rows-clear-of table (nth 0 b))))
    ;; 00:30-02:00 is compared on the same clock, as the scheduler does.
    (should (memq (nth 4 b) (writing-habit-table-rows-clear-of table (nth 3 b))))
    (should (memq (nth 3 b) (writing-habit-table-rows-clear-of table (nth 0 b))))
    (should-error (writing-habit-table-rows-clear-of
                   table (car (writing-habit-table-tests--sections table))))))

(writing-habit-table-tests--deftest split-due-date
  "A due date at the end of a description is split off."
  (dolist (case '(("1003molGraphicsR01, Sept 25" "1003molGraphicsR01" . "Sept 25")
                  ("0201dusp1 September 18" "0201dusp1" . "September 18")
                  ("paper due 2026-10-01" "paper" . "2026-10-01")
                  ("grant by: Ocotober 3, 2026" "grant" . "Ocotober 3, 2026")
                  ("review 10/3" "review" . "10/3")
                  ("0485GUIsc," "0485GUIsc")
                  ("0382CCinJN, 4072UsersMeeting2026" "0382CCinJN, 4072UsersMeeting2026")))
    (should (equal (writing-habit-table-split-due-date (car case)) (cdr case)))))

(writing-habit-table-tests--deftest project-info
  "Project info gives the name, due date, and risk word."
  (let ((table (writing-habit-table-from-text
                (concat "| Time | M |\n|------+---|\n| 09:00-10:00 | A |\n|-\n"
                        "| A: 1003molGraphicsR01, Sept 25 :risky: | |\n"))))
    (should (equal (writing-habit-table-project-info table "a")
                   '(:code "A" :name "1003molGraphicsR01" :due "Sept 25" :risk "risky"
                     :activity nil)))
    (should-not (writing-habit-table-project-info table "Z"))))

;;;; Deleting rows and two-letter codes

(defun writing-habit-table-tests--full-legend ()
  "Return a one-block table whose legend defines A to Z."
  (writing-habit-table-from-text
   (concat "| Time <l>    | M |\n"
           "|-------------+---|\n"
           "| Generative: |   |\n"
           "| 09:00-10:00 |   |\n"
           "|-------------+---|\n"
           (mapconcat (lambda (c) (format "| %c: |   |\n" c))
                      (number-sequence ?A ?Z) ""))))

(writing-habit-table-tests--deftest delete-block-removes-one-line
  "Deleting a block removes its line and leaves every other line alone."
  (let* ((before (writing-habit-table-tests--lines example))
         (target (nth 1 (writing-habit-table-block-rows example)))
         (raw (writing-habit-table-row-raw (writing-habit-table-row-at example target)))
         (row (writing-habit-table-remove-block example target)))
    (should (equal (writing-habit-table-row-raw row) raw))
    (should (equal (writing-habit-table-tests--lines example) (remove raw before)))
    (should (writing-habit-table-dirty example))))

(writing-habit-table-tests--deftest delete-block-keeps-sections
  "The remaining blocks keep their sections."
  (writing-habit-table-remove-block example (car (writing-habit-table-block-rows example)))
  (should (equal (mapcar (lambda (i) (writing-habit-table-row-section
                                      (writing-habit-table-row-at example i)))
                         (writing-habit-table-block-rows example))
                 '("Generative" "Rewriting" "Supporting"))))

(writing-habit-table-tests--deftest delete-only-a-block
  "A section header, a legend row, and an index out of range are refused."
  (dolist (index (list (car (writing-habit-table-tests--sections example))
                       (car (writing-habit-table-legend-rows example))
                       -1 (length (writing-habit-table-rows example))))
    (should-not (writing-habit-table-can-remove-block example index))
    (should-error (writing-habit-table-remove-block example index)))
  (should-not (writing-habit-table-dirty example)))

(writing-habit-table-tests--deftest delete-block-keeps-the-legend
  "A project whose last block goes keeps its legend entry."
  (writing-habit-table-remove-block example (car (last (writing-habit-table-block-rows example))))
  (should (= (writing-habit-table-cells-using example "E") 0))
  (should (assoc "E" (writing-habit-table-legend example))))

(writing-habit-table-tests--deftest delete-project-removes-one-line
  "Deleting a legend entry removes its line and leaves the grid alone."
  (let ((grid (mapcar #'writing-habit-table-row-raw
                      (seq-filter (lambda (r) (eq (writing-habit-table-row-kind r) 'block))
                                  (writing-habit-table-rows example))))
        (count (length (writing-habit-table-tests--lines example))))
    (writing-habit-table-remove-legend example (nth 2 (writing-habit-table-legend-rows example)))
    (should (equal (mapcar #'car (writing-habit-table-legend example)) '("A" "B" "T" "E")))
    (should (= (length (writing-habit-table-tests--lines example)) (1- count)))
    (should (equal (mapcar #'writing-habit-table-row-raw
                           (seq-filter (lambda (r) (eq (writing-habit-table-row-kind r) 'block))
                                       (writing-habit-table-rows example)))
                   grid))))

(writing-habit-table-tests--deftest cells-using-counts-the-grid
  "The count of cells per code."
  (should (= (writing-habit-table-cells-using example "A") 8))
  (should (= (writing-habit-table-cells-using example "e") 5))
  (should (= (writing-habit-table-cells-using example "Q") 0)))

(writing-habit-table-tests--deftest deleted-synced-code-is-forgotten
  "Removing a legend row that a sync added forgets the code."
  (writing-habit-table-set-cell example (car (writing-habit-table-block-rows example)) 6 "Q")
  (writing-habit-table-sync-legend example)
  (should (member "Q" (writing-habit-table-synced-codes example)))
  (writing-habit-table-remove-legend example (car (last (writing-habit-table-legend-rows example))))
  (should-not (member "Q" (writing-habit-table-synced-codes example))))

(writing-habit-table-tests--deftest project-codes-run-from-a-to-zz
  "The single letters come first and the two-letter codes follow."
  (should (equal (seq-take writing-habit-table-project-codes 28)
                 (append (mapcar #'char-to-string (number-sequence ?A ?Z)) '("AA" "AB"))))
  (should (equal (car (last writing-habit-table-project-codes)) "ZZ"))
  (should (= (length (delete-dups (copy-sequence writing-habit-table-project-codes))) 702)))

(writing-habit-table-tests--deftest next-free-code-follows-z-with-aa
  "Once A to Z are taken the next free code is AA, then AB."
  (let ((table (writing-habit-table-tests--full-legend)))
    (should (= (length (writing-habit-table-legend table)) 26))
    (should (equal (writing-habit-table-next-free-code table) "AA"))
    (writing-habit-table-insert-legend table nil nil "AA" "a 27th project")
    (should (equal (writing-habit-table-next-free-code table) "AB"))
    (writing-habit-table-set-cell table (car (writing-habit-table-block-rows table)) 1 "ab")
    (should (equal (writing-habit-table-next-free-code table) "AC"))))

(writing-habit-table-tests--deftest two-letter-cell-reaches-the-readers
  "A two-letter code in a cell is read whole and has no canonical name."
  (let ((table (writing-habit-table-tests--full-legend)))
    (writing-habit-table-insert-legend table nil nil "AB" "a 28th project" "safe")
    (writing-habit-table-set-cell table (car (writing-habit-table-block-rows table)) 1 "AB")
    (should (equal (writing-habit-table-used-codes table) '("AB")))
    (should (equal (plist-get (writing-habit-table-project-info table "AB") :risk) "safe"))
    (should (equal (mapcar (lambda (ev) (plist-get ev :letter))
                           (plist-get (writing-schedule-parse-text
                                       (writing-habit-table-to-text table))
                                      :events))
                   '("AB")))
    (should-not (car (writing-habit-table-code-or-problem table)))))

;;;; Activity letters in cells and legend default activities

(defconst writing-habit-table-tests--free
  (concat "| Time        | M  | Tu | W  |\n"
          "|-------------+----+----+----|\n"
          "| 12:15-13:00 | sE | E  | sE |\n"
          "| 17:30-19:00 | eB | eA |    |\n"
          "| 21:00-23:30 | gA | gB | B  |\n"
          "|-------------+----+----+----|\n"
          "| A: DNPH1 docking :safe: |  |  |  |\n"
          "| B: DUSP1 radiation |  |  |  |\n"
          "| E: email @support |  |  |  |\n")
  "A week with activity letters and no section headers.")

(defun writing-habit-table-tests--sections-by (events)
  "Return ((OFFSET START LETTER) . SECTION) for scheduler EVENTS."
  (sort (mapcar (lambda (e) (cons (list (plist-get e :offset) (plist-get e :start)
                                        (plist-get e :letter))
                                  (plist-get e :section)))
                events)
        (lambda (a b) (string< (format "%S" a) (format "%S" b)))))

(writing-habit-table-tests--deftest split-cell-matches-the-scheduler
  "The cell rule of the editor is the scheduler's rule."
  (dolist (cell '("gA" "eEM" "sW2" "A" "ga" "gem" " sE " "xA" "" "Zebra" "q"))
    (should (equal (writing-habit-table-split-cell cell)
                   (writing-schedule-split-cell cell)))))

(writing-habit-table-tests--deftest normalize-cell
  "A cell keeps a valid letter and raises the code."
  (dolist (case '(("gA" . "gA") ("ea" . "EA") ("a" . "A") (" sEM " . "sEM") ("" . "")))
    (should (equal (writing-habit-table-normalize-cell (car case)) (cdr case))))
  (let ((row (car (writing-habit-table-block-rows example))))
    (writing-habit-table-set-cell example row 1 "eA")
    (should (equal (writing-habit-table-cell example row 1) "eA"))))

(writing-habit-table-tests--deftest prefix-beats-the-section
  "A cell letter sets the activity of its block."
  (let ((row (car (writing-habit-table-block-rows example))))
    (writing-habit-table-set-cell example row 1 "eA")
    (let ((b (seq-find (lambda (b) (and (= (writing-habit-table-block-row b) row)
                                        (= (writing-habit-table-block-column b) 1)))
                       (writing-habit-table-blocks example))))
      (should (equal (writing-habit-table-block-letter b) "A"))
      (should (equal (writing-habit-table-block-category b) "editing"))
      (should (eq (writing-habit-table-block-activity-source b) 'cell))
      (should (equal (writing-habit-table-block-event-section b) "Rewriting")))))

(writing-habit-table-tests--deftest legend-default-beats-the-section
  "A default activity on the legend entry applies to the bare cells."
  (let ((e-row (car (last (writing-habit-table-legend-rows example)))))
    (writing-habit-table-set-legend example e-row "E" "email" nil "editing")
    (dolist (b (writing-habit-table-blocks example))
      (when (equal (writing-habit-table-block-letter b) "E")
        (should (equal (writing-habit-table-block-category b) "editing"))
        (should (eq (writing-habit-table-block-activity-source b) 'legend))))))

(writing-habit-table-tests--deftest editor-and-scheduler-agree
  "The editor's events match the scheduler's, with and without headers."
  (let ((free (writing-habit-table-from-text writing-habit-table-tests--free)))
    (should (equal (writing-habit-table-tests--sections-by (writing-habit-table-events free))
                   (writing-habit-table-tests--sections-by
                    (plist-get (writing-schedule-parse-text
                                (writing-habit-table-to-text free))
                               :events)))))
  (writing-habit-table-set-cell example (car (writing-habit-table-block-rows example)) 2 "sB")
  (writing-habit-table-set-legend example (nth 3 (writing-habit-table-legend-rows example))
                                  "T" "teaching" nil "support")
  (should (equal (writing-habit-table-tests--sections-by (writing-habit-table-events example))
                 (writing-habit-table-tests--sections-by
                  (plist-get (writing-schedule-parse-text
                              (writing-habit-table-to-text example))
                             :events)))))

(writing-habit-table-tests--deftest activity-totals-name-and-fallbacks
  "Totals, the canonical name, and the fallbacks follow the resolved activity."
  (let ((free (writing-habit-table-from-text writing-habit-table-tests--free)))
    (should (equal (sort (copy-sequence (plist-get (writing-habit-table-totals free) :category))
                         (lambda (a b) (string< (car a) (car b))))
                   '(("editing" . 180) ("generative" . 450) ("support" . 135))))
    (should (equal (writing-habit-table-code free) "sEeBgA-sEeAgB-sEgB"))
    (should (equal (mapcar #'writing-habit-table-block-letter
                           (writing-habit-table-activity-fallbacks free))
                   '("B")))
    (should (equal (writing-habit-table-used-codes free) '("E" "B" "A")))
    (should (= (writing-habit-table-cells-using free "gA") 2))
    (should (equal (plist-get (writing-habit-table-project-info free "sE") :activity)
                   "support"))))

(writing-habit-table-tests--deftest prefix-overrides-are-listed
  "Only a letter that disagrees with its section is an override."
  (let ((rows (writing-habit-table-block-rows example)))
    (writing-habit-table-set-cell example (nth 0 rows) 1 "eA")
    (writing-habit-table-set-cell example (nth 1 rows) 1 "gA")
    (should (equal (mapcar (lambda (b) (cons (writing-habit-table-block-row b)
                                             (writing-habit-table-block-column b)))
                           (writing-habit-table-prefix-overrides example))
                   (list (cons (nth 0 rows) 1))))))

(writing-habit-table-tests--deftest legend-activity-tags
  "The name module strips the tag, and set-legend keeps or removes it."
  (should (equal (writing-habit-name-parse-legend-cell "E: email @support :safe:")
                 '("E" "email" "safe")))
  (should (equal (writing-habit-name-parse-legend-cell "E: email :safe: @support")
                 '("E" "email" "safe")))
  (should (equal (writing-habit-name-legend-activity "E: email @Support") "support"))
  (should-not (writing-habit-name-legend-activity "E: me@support.org"))
  (let* ((free (writing-habit-table-from-text writing-habit-table-tests--free))
         (row (nth 2 (writing-habit-table-legend-rows free))))
    (writing-habit-table-set-legend free row "E" "inbox" "safe")
    (should (equal (car (writing-habit-table-row-cells (writing-habit-table-row-at free row)))
                   "E: inbox @support :safe:"))
    (writing-habit-table-set-legend free row "E" "inbox" "safe" 'none)
    (should (equal (car (writing-habit-table-row-cells (writing-habit-table-row-at free row)))
                   "E: inbox :safe:"))
    (let ((at (writing-habit-table-insert-legend free nil nil "T" "teaching" nil "support")))
      (should (equal (car (writing-habit-table-row-cells (writing-habit-table-row-at free at)))
                     "T: teaching @support")))))

(writing-habit-table-tests--deftest move-activities-into-cells
  "Section letters move into the cells, and the headers can then go."
  (let ((categories (lambda (tb) (mapcar (lambda (b) (list (writing-habit-table-block-offset b)
                                                           (writing-habit-table-block-start b)
                                                           (writing-habit-table-block-category b)))
                                         (writing-habit-table-blocks tb))))
        (rows (writing-habit-table-block-rows example)))
    (let ((before (funcall categories example))
          (code (writing-habit-table-code example)))
      (should (= (writing-habit-table-move-activities-into-cells example) 22))
      (should (equal (writing-habit-table-cell example (nth 0 rows) 1) "gA"))
      (should (equal (writing-habit-table-cell example (nth 2 rows) 1) "eB"))
      (should (equal (writing-habit-table-cell example (nth 3 rows) 1) "sE"))
      (should (equal (funcall categories example) before))
      (should (= (writing-habit-table-remove-section-rows example) 3))
      (should-not (writing-habit-table--indexes example '(section)))
      (should (equal (funcall categories example) before))
      (should (equal (writing-habit-table-code example) code))
      (should (= (writing-habit-table-move-activities-into-cells example) 0)))))

(provide 'writing-habit-table-tests)
;;; writing-habit-table-tests.el ends here
