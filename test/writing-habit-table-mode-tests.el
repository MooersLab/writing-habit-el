;;; writing-habit-table-mode-tests.el --- ERT tests for the table editing mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Ports the behavior tests of the Python Schedule tab to the Emacs mode:
;; turning on by file name, the insert and move commands, the legend
;; commands, completion, eldoc, the clash and clear tints, the report, and
;; the rename.  Each test runs in a buffer visiting a temporary copy of a
;; fixture, and prompts are answered by binding the reader functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst writing-habit-table-mode-tests--ok
  (and (require 'writing-schedule nil t) (fboundp 'writing-schedule-split-row))
  "Non-nil when writing-schedule.el 0.3.1 is available.")

(when writing-habit-table-mode-tests--ok
  (require 'writing-habit-table-mode))
(require 'writing-habit-table-auto)

(defconst writing-habit-table-mode-tests--fixtures
  (expand-file-name "fixtures" (file-name-directory (or load-file-name buffer-file-name)))
  "Fixture directory.")

(defconst writing-habit-table-mode-tests--clash
  (concat "#+TITLE: clash\n\n"
          "| Time <l>    | M  | Tu |\n"
          "|-------------+----+----|\n"
          "| Generative: |    |    |\n"
          "| 04:00-05:30 | A  |    |\n"
          "| Rewriting:  |    |    |\n"
          "| 05:00-06:30 | B  | A  |\n"
          "| 09:00-10:00 |    | B  |\n"
          "|-------------+----+----|\n"
          "| A: one, Sept 25 :safe: |  |  |\n"
          "| B: two :risky:         |  |  |\n")
  "A small week with one clash on Monday.")

(defmacro writing-habit-table-mode-tests--with (spec &rest body)
  "Visit a temporary table named by SPEC and run BODY in its buffer.
SPEC is (NAME CONTENT), where CONTENT is a string or a fixture file name."
  (declare (indent 1))
  `(progn
     (skip-unless writing-habit-table-mode-tests--ok)
     (let* ((dir (make-temp-file "wh-mode" t))
            (file (expand-file-name ,(car spec) dir))
            (content ,(cadr spec)))
       (if (string-suffix-p ".org" content)
           (copy-file (expand-file-name content writing-habit-table-mode-tests--fixtures) file)
         (with-temp-file file (insert content)))
       (let ((buf (find-file-noselect file)))
         (unwind-protect
             (with-current-buffer buf
               (writing-habit-table-mode 1)
               ,@body)
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)
           (delete-directory dir t))))))

(defun writing-habit-table-mode-tests--goto (text &optional cell)
  "Move to the line that starts with the table row TEXT, into CELL."
  (goto-char (point-min))
  (re-search-forward (concat "^| " (regexp-quote text)))
  (beginning-of-line)
  (when cell (search-forward "|" (line-end-position) t (1+ cell)) (forward-char 1)))

(defun writing-habit-table-mode-tests--line ()
  "Return the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;;;; Turning on

(ert-deftest writing-habit-table-mode/schedule-file-names ()
  "Schedule-code names are recognized, with or without a date prefix."
  (should (writing-habit-table-schedule-file-p "/x/4gAeA-gW.org"))
  (should (writing-habit-table-schedule-file-p "/x/2026-01-19_4gAeA-gW.org"))
  (should-not (writing-habit-table-schedule-file-p "/x/my-week.org"))
  (should-not (writing-habit-table-schedule-file-p "/x/4gAeA-gW.txt"))
  (should-not (writing-habit-table-schedule-file-p nil)))

(ert-deftest writing-habit-table-mode/turns-on-by-name ()
  "The mode turns on in a schedule-code file and stays off elsewhere."
  (skip-unless writing-habit-table-mode-tests--ok)
  (let ((dir (make-temp-file "wh-auto" t)))
    (unwind-protect
        (dolist (case '(("gA-gB.org" . t) ("notes.org" . nil)))
          (let ((file (expand-file-name (car case) dir)))
            (with-temp-file file (insert writing-habit-table-mode-tests--clash))
            (let ((buf (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buf
                    (should (eq (and writing-habit-table-mode t) (cdr case))))
                (kill-buffer buf)))))
      (delete-directory dir t))))

(ert-deftest writing-habit-table-mode/auto-can-be-turned-off ()
  "With the option off, a schedule-code file opens without the mode."
  (skip-unless writing-habit-table-mode-tests--ok)
  (let ((dir (make-temp-file "wh-auto" t))
        (writing-habit-table-mode-auto nil))
    (unwind-protect
        (let ((file (expand-file-name "gA-gB.org" dir)))
          (with-temp-file file (insert writing-habit-table-mode-tests--clash))
          (let ((buf (find-file-noselect file)))
            (unwind-protect (with-current-buffer buf (should-not writing-habit-table-mode))
              (kill-buffer buf))))
      (delete-directory dir t))))

;;;; Inserting and moving

(ert-deftest writing-habit-table-mode/insert-below-with-suggestion ()
  "Insert below offers a flush range and adds exactly one line."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (let ((lines (count-lines (point-min) (point-max)))
          (offered nil))
      (writing-habit-table-mode-tests--goto "04:00-05:30" 1)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt initial &rest _) (setq offered initial) initial)))
        (writing-habit-table-insert-below))
      (should (equal offered "05:30-07:00"))
      (should (= (count-lines (point-min) (point-max)) (1+ lines)))
      (should (string-prefix-p "| 05:30-07:00" (writing-habit-table-mode-tests--line))))))

(ert-deftest writing-habit-table-mode/insert-needs-a-block-or-section ()
  "Insert refuses a legend row."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (writing-habit-table-mode-tests--goto "A: ")
    (should-error (writing-habit-table-insert-above) :type 'user-error)))

(ert-deftest writing-habit-table-mode/move-block-across-section ()
  "Moving a block past a header names its new section and keeps the column."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (writing-habit-table-mode-tests--goto "05:45-07:15" 2)
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (writing-habit-table-move-down))
      (should (equal msg "Moved the 05:45-07:15 block down into Rewriting"))
      (should (string-prefix-p "| 05:45-07:15" (writing-habit-table-mode-tests--line)))
      (should (= (writing-habit-table--cell-at-point) 2))
      (forward-line -1)
      (should (string-prefix-p "| Rewriting:" (writing-habit-table-mode-tests--line))))))

(ert-deftest writing-habit-table-mode/move-up-undoes-move-down ()
  "A move up undoes a move down byte for byte."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (let ((text (buffer-string)))
      (writing-habit-table-mode-tests--goto "04:00-05:30" 1)
      (writing-habit-table-move-down)
      (should-not (equal (buffer-string) text))
      (writing-habit-table-move-up)
      (should (equal (buffer-string) text)))))

(ert-deftest writing-habit-table-mode/move-refused-at-edges ()
  "The first block cannot rise above the first section."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (writing-habit-table-mode-tests--goto "04:00-05:30" 1)
    (should-error (writing-habit-table-move-up) :type 'user-error)
    (writing-habit-table-mode-tests--goto "Rewriting:")
    (should-error (writing-habit-table-move-down) :type 'user-error)))

(ert-deftest writing-habit-table-mode/move-legend-entry ()
  "The same keys move a legend entry within the legend."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (writing-habit-table-mode-tests--goto "A: ")
    (writing-habit-table-move-down)
    (should (string-prefix-p "| A: " (writing-habit-table-mode-tests--line)))
    (forward-line -1)
    (should (string-prefix-p "| B: " (writing-habit-table-mode-tests--line)))))

(ert-deftest writing-habit-table-mode/keys-are-bound ()
  "M-<up> and M-<down> run the mode's move commands."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (should (eq (key-binding (kbd "M-<up>")) #'writing-habit-table-move-up))
    (should (eq (key-binding (kbd "M-<down>")) #'writing-habit-table-move-down))
    (should (eq (key-binding (kbd writing-habit-table-mode-prefix))
                #'writing-habit-table-menu))))

;;;; The legend

(ert-deftest writing-habit-table-mode/insert-project ()
  "A project is inserted below the entry at point with the next free code."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (writing-habit-table-mode-tests--goto "A: ")
    (let ((answers '("New thing, Oct 3"))
          (offered nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial &rest _)
                   (if (string-prefix-p "Project code" prompt)
                       (progn (setq offered initial) initial)
                     (pop answers))))
                ((symbol-function 'completing-read) (lambda (&rest _) "risky")))
        (writing-habit-table-insert-project-below))
      (should (equal offered "C"))
      (should (string-prefix-p "| C: New thing, Oct 3 :risky:"
                               (writing-habit-table-mode-tests--line)))
      (forward-line -1)
      (should (string-prefix-p "| A: " (writing-habit-table-mode-tests--line))))))

(ert-deftest writing-habit-table-mode/duplicate-project-refused ()
  "A code the legend already defines is refused without a change."
  (writing-habit-table-mode-tests--with ("gA.org" "my-week-named.org")
    (let ((text (buffer-string)))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _) (if (string-prefix-p "Project code" prompt) "A" "x")))
                ((symbol-function 'completing-read) (lambda (&rest _) "none")))
        (should-error (writing-habit-table-insert-project-below) :type 'user-error))
      (should (equal (buffer-string) text)))))

(ert-deftest writing-habit-table-mode/update-legend ()
  "A typed code gains a legend row on request."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (writing-habit-table-mode-tests--goto "09:00-10:00" 1)
    (let ((b (writing-habit-table--cell-bounds 1)))
      (delete-region (car b) (cdr b))
      (goto-char (car b))
      (insert " Q  "))
    (writing-habit-table-update-legend)
    (should (assoc "Q" (writing-habit-table-legend (writing-habit-table-current))))))

;;;; Completion and eldoc

(ert-deftest writing-habit-table-mode/completion-offers-legend-codes ()
  "Completion in a day cell offers the legend codes."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (writing-habit-table-mode-tests--goto "09:00-10:00" 1)
    (let ((capf (writing-habit-table-completion-at-point)))
      (should capf)
      (should (equal (nth 2 capf) '("A" "B"))))
    (writing-habit-table-mode-tests--goto "09:00-10:00" 0)
    (should-not (writing-habit-table-completion-at-point))))

(ert-deftest writing-habit-table-mode/eldoc-describes-code ()
  "Eldoc gives the name, due date, and risk of the code at point."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (writing-habit-table-mode-tests--goto "04:00-05:30" 1)
    (should (equal (writing-habit-table-eldoc) "A: one, due Sept 25, safe"))
    (writing-habit-table-mode-tests--goto "09:00-10:00" 2)
    (should (equal (writing-habit-table-eldoc) "B: two, risky"))
    (writing-habit-table-mode-tests--goto "09:00-10:00" 1)
    (should-not (writing-habit-table-eldoc))))

;;;; Tints

(defun writing-habit-table-mode-tests--faces ()
  "Return the (FACE . TEXT) of each tint overlay, sorted by position."
  (mapcar (lambda (ov) (cons (overlay-get ov 'face)
                             (string-trim (buffer-substring-no-properties
                                           (overlay-start ov) (overlay-end ov)))))
          (sort (copy-sequence writing-habit-table--overlays)
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest writing-habit-table-mode/clash-cells-are-red ()
  "The two clashing Monday cells are tinted, and the lighter counts them."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (goto-char (point-min))
    (writing-habit-table--refresh t)
    (should (equal (writing-habit-table-mode-tests--faces)
                   '((writing-habit-table-clash . "A") (writing-habit-table-clash . "B"))))
    (should (equal (writing-habit-table--lighter) " WH[1 clash]"))))

(ert-deftest writing-habit-table-mode/time-cell-tints-clear-rows ()
  "Point in the Time column tints the rows clear of that block."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (writing-habit-table-mode-tests--goto "04:00-05:30" 0)
    (writing-habit-table--refresh t)
    (let ((clear (seq-filter (lambda (f) (eq (car f) 'writing-habit-table-clear))
                             (writing-habit-table-mode-tests--faces))))
      (should (= (length clear) 1))
      (should (string-prefix-p "| 09:00-10:00" (cdar clear))))
    (writing-habit-table-mode-tests--goto "04:00-05:30" 1)
    (writing-habit-table--refresh t)
    (should-not (seq-find (lambda (f) (eq (car f) 'writing-habit-table-clear))
                          (writing-habit-table-mode-tests--faces)))))

(ert-deftest writing-habit-table-mode/turning-off-clears-tints ()
  "Turning the mode off removes its overlays."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (writing-habit-table--refresh t)
    (should writing-habit-table--overlays)
    (writing-habit-table-mode -1)
    (should-not writing-habit-table--overlays)))

;;;; Report and rename

(ert-deftest writing-habit-table-mode/report ()
  "The report names the week and lists clashes, totals, and the legend."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (let ((report (writing-habit-table-report-string (writing-habit-table-current))))
      (should (string-match-p "^\\* Name" report))
      (should (string-match-p "Canonical name: =gAeB-eAeB.org=" report))
      (should (string-match-p "The file is named =gA.org=" report))
      (should (string-match-p "^\\* Clashes (1)" report))
      (should (string-match-p "^- Monday: 04:00-05:30 \\[A, Generative\\] overlaps" report))
      (should (string-match-p "^- generative :: 90 min" report))
      (should (string-match-p "^\\* Legend" report)))))

(ert-deftest writing-habit-table-mode/rename-file ()
  "Renaming moves the file and the buffer to the canonical name."
  (writing-habit-table-mode-tests--with ("gA.org" writing-habit-table-mode-tests--clash)
    (let ((old buffer-file-name))
      (writing-habit-table-rename-file)
      (should (equal (file-name-nondirectory buffer-file-name) "gAeB-eAeB.org"))
      (should (file-exists-p buffer-file-name))
      (should-not (file-exists-p old)))))

(provide 'writing-habit-table-mode-tests)
;;; writing-habit-table-mode-tests.el ends here
