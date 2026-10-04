;;; writing-habit-table-mode.el --- Edit a weekly block table with live checks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1") (transient "0.4") (writing-schedule "0.3.1"))
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT license.

;;; Commentary:

;; `writing-habit-table-mode' is the Emacs counterpart of the Schedule tab
;; of the Python package's graphical interface.  The org table in the
;; buffer is the grid, so the mode adds only what the table cannot show on
;; its own:
;;
;;  - Clash cells are tinted red, using the scheduler's own overlap test.
;;  - With point in the Time column of a block, every block whose range
;;    does not overlap it is tinted yellow.  A clash stays red inside a
;;    yellow row, because the clash is the more urgent thing to see.
;;  - Completion in a day cell offers the legend codes, and eldoc shows the
;;    project name, due date, and risk of the code at point.
;;  - Commands insert a time block with a suggested range, move a block or
;;    a legend entry with M-<up> and M-<down>, insert a project with the
;;    next free code, rename the file to its canonical schedule code, and
;;    open a side window that reports the name, clashes, totals, and legend
;;    of the week.
;;
;; Every edit goes through the model in writing-habit-table.el, so a cell
;; edit rewrites one line and a move swaps lines, and the scheduler always
;; reads the file the way the editor shows it.
;;
;; The mode turns itself on in an org buffer whose file name is a schedule
;; code, such as 4gAeA-gW.org or 2026-01-19_4gAeA-gW.org, and that holds a
;; weekly table.  Set `writing-habit-table-mode-auto' to nil to turn that
;; off, and call `writing-habit-table-mode' by hand instead.
;;
;; Keys, in the table:
;;   M-<up>, M-<down>   move the block or legend entry at point
;;   C-c C-;            the table menu (see `writing-habit-table-mode-prefix')

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-table)
(require 'transient)
(require 'writing-schedule)
(require 'writing-habit-name)
(require 'writing-habit-plan)
(require 'writing-habit-table)
(require 'writing-habit-table-auto)

(defcustom writing-habit-table-mode-prefix "C-c C-;"
  "Key, in `kbd' syntax, that opens the table menu in the mode.
Set it before the mode first loads."
  :type 'string
  :group 'writing-habit-table)

(defcustom writing-habit-table-idle-delay 0.25
  "Seconds of idle time before the tints and the mode line refresh."
  :type 'number
  :group 'writing-habit-table)

(defface writing-habit-table-clash
  '((((background dark)) :background "#6b1f1f" :foreground "#ffd7d7")
    (t :background "#ffd0d0" :foreground "#7a0000"))
  "Face for a day cell whose block overlaps another block on that day."
  :group 'writing-habit-table)

(defface writing-habit-table-clear
  '((((background dark)) :background "#4a4400")
    (t :background "#fff3a8"))
  "Face for a block row whose range does not overlap the selected one."
  :group 'writing-habit-table)

(defvar writing-habit-table-mode)

(defvar-local writing-habit-table--overlays nil
  "Overlays this mode placed in the buffer.")

(defvar-local writing-habit-table--timer nil
  "Idle timer that refreshes the tints of this buffer.")

(defvar-local writing-habit-table--clash-count 0
  "Number of clashing pairs found at the last refresh.")

(defvar-local writing-habit-table--last-tick nil
  "Buffer modification tick and point at the last refresh.")

(defvar-local writing-habit-table--synced nil
  "Codes whose legend row the mode added through a sync.")

;;;; Mapping between the buffer and the model

(defun writing-habit-table-current ()
  "Return the model of the weekly table in the current buffer."
  (let ((model (writing-habit-table-from-text
                (buffer-substring-no-properties (point-min) (point-max))
                buffer-file-name)))
    (setf (writing-habit-table-synced-codes model) writing-habit-table--synced)
    model))

(defun writing-habit-table--row-at-point (model)
  "Return the model row index of the line at point in MODEL, or nil."
  (let ((index (- (line-number-at-pos) 1 (length (writing-habit-table-before model)))))
    (when (and (>= index 0) (< index (length (writing-habit-table-rows model))))
      index)))

(defun writing-habit-table--cell-at-point ()
  "Return the zero-based cell index at point, or nil outside a table row."
  (save-excursion
    (let ((end (point)))
      (beginning-of-line)
      (when (looking-at-p "[ \t]*|")
        (let ((pipes 0))
          (while (search-forward "|" end t) (setq pipes (1+ pipes)))
          (and (> pipes 0) (1- pipes)))))))

(defun writing-habit-table--goto (model row &optional cell)
  "Move point to row ROW of MODEL, inside CELL when CELL is non-nil."
  (goto-char (point-min))
  (forward-line (+ row (length (writing-habit-table-before model))))
  (when cell
    (let ((bound (line-end-position)))
      (when (search-forward "|" bound t (1+ cell))
        (skip-chars-forward " " bound)))))

(defun writing-habit-table--cell-bounds (cell)
  "Return (BEG . END) of the text of CELL on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (let ((bound (line-end-position)))
      (when (search-forward "|" bound t (1+ cell))
        (let ((beg (point)))
          (when (search-forward "|" bound t)
            (cons beg (1- (point)))))))))

(defun writing-habit-table--apply (model)
  "Replace the buffer text with MODEL's text, keeping unchanged text intact."
  (let ((text (writing-habit-table-to-text model))
        (source (generate-new-buffer " *writing-habit-table*")))
    (unwind-protect
        (progn
          (with-current-buffer source (insert text))
          (replace-buffer-contents source))
      (kill-buffer source))
    (setq writing-habit-table--synced (writing-habit-table-synced-codes model))
    (writing-habit-table--refresh t)))

(defun writing-habit-table--require-row (model kinds)
  "Return the row index at point in MODEL when its kind is in KINDS.
Signal a user error naming what to select otherwise."
  (let ((row (writing-habit-table--row-at-point model)))
    (unless (and row (memq (writing-habit-table--kind model row) kinds))
      (user-error "Put point on %s first"
                  (mapconcat (lambda (k) (pcase k
                                           ('block "a time block")
                                           ('section "a section header")
                                           ('legend "a legend entry")))
                             kinds " or ")))
    row))

;;;; Inserting and moving time blocks

(defun writing-habit-table--read-range (default)
  "Read a time range, offering DEFAULT, a cons of HH:MM strings."
  (let* ((text (read-string "Time range of the new block: "
                            (format "%s-%s" (car default) (cdr default))))
         (times (writing-schedule-parse-time text)))
    (unless times (user-error "Not a time range: %s" text))
    times))

(defun writing-habit-table--insert (above)
  "Insert an empty time block ABOVE or below the row at point."
  (let* ((model (writing-habit-table-current))
         (row (writing-habit-table--require-row model '(block section)))
         (times (writing-habit-table--read-range
                 (writing-habit-table-suggest-times model row above)))
         (at (condition-case err
                 (writing-habit-table-insert-block model row above (car times) (cdr times))
               (error (user-error "%s" (error-message-string err))))))
    (writing-habit-table--apply model)
    (writing-habit-table--goto model at (car (car (writing-habit-table-columns model))))
    (message "Inserted the %s-%s block into %s" (car times) (cdr times)
             (writing-habit-table-row-section (writing-habit-table-row-at model at)))))

(defun writing-habit-table-insert-above ()
  "Insert an empty time block above the block or section header at point.
The prompt offers a range of the same length placed flush against the
neighbouring block."
  (interactive)
  (writing-habit-table--insert t))

(defun writing-habit-table-insert-below ()
  "Insert an empty time block below the block or section header at point.
The prompt offers a range of the same length placed flush against the
neighbouring block."
  (interactive)
  (writing-habit-table--insert nil))

(defun writing-habit-table--move (up)
  "Move the block or legend entry at point UP or down.
Outside the rows the mode handles, fall back to the org command."
  (let* ((model (writing-habit-table-current))
         (row (writing-habit-table--row-at-point model))
         (kind (and row (writing-habit-table--kind model row)))
         (cell (writing-habit-table--cell-at-point)))
    (pcase kind
      ('block
       (let ((section (writing-habit-table-row-section (writing-habit-table-row-at model row)))
             (times (writing-habit-table-row-parsed (writing-habit-table-row-at model row))))
         (unless (writing-habit-table-can-move model row up)
           (user-error "This block is already at the %s of the grid" (if up "top" "bottom")))
         (let* ((at (writing-habit-table-move-block model row up))
                (new (writing-habit-table-row-section (writing-habit-table-row-at model at))))
           (writing-habit-table--apply model)
           (writing-habit-table--goto model at cell)
           (unless (equal section new)
             (message "Moved the %s-%s block %s into %s"
                      (car times) (cdr times) (if up "up" "down") new)))))
      ('legend
       (unless (writing-habit-table-can-move-legend model row up)
         (user-error "This project is already at the %s of the legend" (if up "top" "bottom")))
       (let ((at (writing-habit-table-move-legend model row up)))
         (writing-habit-table--apply model)
         (writing-habit-table--goto model at cell)))
      ('section (user-error "A section header does not move; move the blocks around it"))
      (_ (call-interactively (if up #'org-metaup #'org-metadown))))))

(defun writing-habit-table-move-up ()
  "Move the time block or legend entry at point up by one row.
A block that passes a section header joins the section above.  Elsewhere
in the buffer, run `org-metaup'."
  (interactive)
  (writing-habit-table--move t))

(defun writing-habit-table-move-down ()
  "Move the time block or legend entry at point down by one row.
A block that passes a section header joins the section below.  Elsewhere
in the buffer, run `org-metadown'."
  (interactive)
  (writing-habit-table--move nil))

;;;; The legend

(defun writing-habit-table--read-project (model)
  "Read a code, a description, and a risk class for a new project in MODEL.
Return (CODE DESCRIPTION RISK)."
  (let* ((code (upcase (string-trim
                        (read-string "Project code: " (writing-habit-table-next-free-code model)))))
         (description (read-string (format "Description of %s (a due date may follow): " code)))
         (tag (completing-read "Risk tag: " '("none" "safe" "risky") nil t nil nil "none")))
    (list code description (pcase tag ("safe" "safe") ("risky" "speculative") (_ nil)))))

(defun writing-habit-table--insert-project (above)
  "Insert a project into the legend ABOVE or below the entry at point.
With point outside the legend, add the project at the end of the legend."
  (let* ((model (writing-habit-table-current))
         (row (writing-habit-table--row-at-point model))
         (near (and row (eq (writing-habit-table--kind model row) 'legend) row))
         (spec (writing-habit-table--read-project model))
         (at (condition-case err
                 (apply #'writing-habit-table-insert-legend model near above spec)
               (error (user-error "%s" (error-message-string err))))))
    (writing-habit-table--apply model)
    (writing-habit-table--goto model at 0)
    (message "Added project %s to the legend" (car spec))))

(defun writing-habit-table-insert-project-above ()
  "Insert a project into the legend above the entry at point.
The code starts as the first letter neither the legend nor the grid uses.
A code the legend already defines is refused."
  (interactive)
  (writing-habit-table--insert-project t))

(defun writing-habit-table-insert-project-below ()
  "Insert a project into the legend below the entry at point.
With point outside the legend, the project goes to the end of the legend."
  (interactive)
  (writing-habit-table--insert-project nil))

(defun writing-habit-table-update-legend ()
  "Give every code used in the grid a legend row.
Drop a blank row that this command added once its code leaves the grid."
  (interactive)
  (let ((model (writing-habit-table-current)))
    (if (writing-habit-table-sync-legend model)
        (progn (writing-habit-table--apply model) (message "Legend updated"))
      (when (called-interactively-p 'interactive)
        (message "The legend already covers the grid")))))

;;;; Completion and eldoc

(defun writing-habit-table-completion-at-point ()
  "Complete a project code in a day cell of a time block."
  (let* ((model (writing-habit-table-current))
         (row (writing-habit-table--row-at-point model))
         (cell (writing-habit-table--cell-at-point)))
    (when (and row (eq (writing-habit-table--kind model row) 'block)
               cell (assoc cell (writing-habit-table-columns model)))
      (let ((bounds (writing-habit-table--cell-bounds cell)))
        (when bounds
          (let ((beg (save-excursion (goto-char (car bounds))
                                     (skip-chars-forward " " (cdr bounds)) (point)))
                (end (save-excursion (goto-char (cdr bounds))
                                     (skip-chars-backward " " (car bounds)) (point))))
            (list (min beg (point)) (max end (point))
                  (mapcar #'car (writing-habit-table-legend model))
                  :exclusive 'no
                  :annotation-function
                  (lambda (code)
                    (concat "  " (nth 1 (assoc code (writing-habit-table-legend model))))))))))))

(defun writing-habit-table-eldoc (&rest _)
  "Describe the project code in the day cell at point."
  (let* ((model (writing-habit-table-current))
         (row (writing-habit-table--row-at-point model))
         (cell (writing-habit-table--cell-at-point)))
    (when (and row (eq (writing-habit-table--kind model row) 'block)
               cell (assoc cell (writing-habit-table-columns model)))
      (let ((code (writing-habit-table-cell model row cell)))
        (unless (string-empty-p code)
          (let ((info (writing-habit-table-project-info model code)))
            (if (null info)
                (format "%s is not in the legend" (upcase code))
              (concat (plist-get info :code) ": " (plist-get info :name)
                      (if (plist-get info :due) (format ", due %s" (plist-get info :due)) "")
                      (if (plist-get info :risk) (format ", %s" (plist-get info :risk)) "")))))))))

;;;; Tints

(defun writing-habit-table--clear-overlays ()
  "Remove the overlays this mode placed."
  (mapc #'delete-overlay writing-habit-table--overlays)
  (setq writing-habit-table--overlays nil))

(defun writing-habit-table--overlay (beg end face priority)
  "Put FACE on BEG to END with PRIORITY and remember the overlay."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'writing-habit-table t)
    (push ov writing-habit-table--overlays)
    ov))

(defun writing-habit-table--refresh (&optional force)
  "Recompute the tints and the clash count of the current buffer.
Skip the work when neither the text nor point moved, unless FORCE."
  (when writing-habit-table-mode
    (let ((tick (cons (buffer-chars-modified-tick) (point))))
      (when (or force (not (equal tick writing-habit-table--last-tick)))
        (setq writing-habit-table--last-tick tick)
        (writing-habit-table--clear-overlays)
        (let* ((model (ignore-errors (writing-habit-table-current)))
               (offset (and model (length (writing-habit-table-before model)))))
          (when (and model (writing-habit-table-columns model))
            (setq writing-habit-table--clash-count
                  (length (writing-habit-table-overlaps model)))
            (save-excursion
              (dolist (rc (writing-habit-table-conflicting-cells model))
                (goto-char (point-min))
                (forward-line (+ offset (car rc)))
                (let ((b (writing-habit-table--cell-bounds (cdr rc))))
                  (when b (writing-habit-table--overlay (car b) (cdr b)
                                                        'writing-habit-table-clash 20)))))
            (let ((row (writing-habit-table--row-at-point model)))
              (when (and row (eq (writing-habit-table--kind model row) 'block)
                         (eql (writing-habit-table--cell-at-point) 0))
                (save-excursion
                  (dolist (r (writing-habit-table-rows-clear-of model row))
                    (goto-char (point-min))
                    (forward-line (+ offset r))
                    (writing-habit-table--overlay (line-beginning-position) (line-end-position)
                                                  'writing-habit-table-clear 10)))))
            (force-mode-line-update)))))))

(defun writing-habit-table--idle-refresh (buffer)
  "Refresh the tints of BUFFER when it is live and current."
  (when (and (buffer-live-p buffer) (eq buffer (current-buffer)))
    (with-current-buffer buffer (writing-habit-table--refresh))))

(defun writing-habit-table--lighter ()
  "Return the mode-line lighter, which carries the clash count."
  (if (> writing-habit-table--clash-count 0)
      (format " WH[%d clash%s]" writing-habit-table--clash-count
              (if (= writing-habit-table--clash-count 1) "" "es"))
    " WH"))

;;;; Report

(defun writing-habit-table-report-string (model)
  "Return the four-part report on MODEL as Org text."
  (let* ((path (writing-habit-table-path model))
         (result (writing-habit-table-code-or-problem model))
         (code (car result))
         (overlaps (writing-habit-table-overlap-lines model))
         (totals (writing-habit-table-totals model))
         (check (writing-habit-table-legend-check model))
         (defined (mapcar #'car (writing-habit-table-legend model)))
         (used (writing-habit-table-used-codes model))
         (unused (seq-remove (lambda (c) (member c used)) defined)))
    (cl-flet ((alist-lines (alist)
                (mapconcat (lambda (c) (format "- %s :: %d min" (car c) (cdr c))) alist "\n")))
      (concat
       "* Name\n"
       (if code
           (concat (format "Canonical name: =%s.org=\n\n" code)
                   (writing-habit-name-format-week (writing-habit-name-decode code)) "\n"
                   (cond ((null path) "\nThe table has not been saved yet.\n")
                         ((writing-habit-table-name-matches-code model)
                          "\nThe file is named canonically.\n")
                         (t (format "\nThe file is named =%s=.  Run =writing-habit-table-rename-file= to fix it.\n"
                                    (file-name-nondirectory path)))))
         (format "No canonical name.  %s\n" (cdr result)))
       (format "\n* Clashes (%d)\n" (length overlaps))
       (if overlaps (mapconcat (lambda (l) (concat "- " l)) overlaps "\n") "No blocks overlap.")
       "\n\n* Totals\n** By day\n" (alist-lines (plist-get totals :day))
       "\n** By project\n" (alist-lines (plist-get totals :project))
       "\n** By activity\n" (alist-lines (plist-get totals :category))
       (let ((unknown (writing-habit-table-unknown-sections model)))
         (if unknown
             (format "\n\nSections that name no activity, counted as generative: %s"
                     (string-join unknown ", "))
           ""))
       "\n\n* Legend\n"
       (mapconcat (lambda (r) (format "- %s :: %s %s%s" (nth 0 r) (nth 4 r) (nth 2 r)
                                      (if (nth 3 r) (format " (%s)" (nth 3 r)) "")))
                  (car check) "\n")
       (if (cadr check) (format "\n\nUnresolved letters: %s" (string-join (cadr check) ", ")) "")
       (if unused (format "\n\nUnused legend entries: %s" (string-join unused ", ")) "")
       (let ((dups (writing-habit-table-duplicate-legend-codes model)))
         (if dups
             (concat "\n\nCodes defined more than once (the first wins):\n"
                     (mapconcat (lambda (d) (format "- %s :: %s" (car d) (string-join (cdr d) " / ")))
                                dups "\n"))
           ""))
       (let ((stray (writing-habit-table-stray-risk-tags model)))
         (if stray
             (concat "\n\nRisk tags that name no class (use :safe: or :risky:):\n"
                     (mapconcat (lambda (s) (format "- %s :: :%s:" (car s) (cdr s))) stray "\n"))
           ""))
       "\n"))))

(defun writing-habit-table-report ()
  "Show the name, clashes, totals, and legend of the week in a side window."
  (interactive)
  (let* ((model (writing-habit-table-current))
         (text (writing-habit-table-report-string model))
         (buf (get-buffer-create "*writing-habit week*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (org-mode)
        (org-fold-show-all)
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (display-buffer-in-side-window buf '((side . right) (window-width . 0.35)))))

(defun writing-habit-table--update-report ()
  "Refresh the report window when it is showing."
  (when (get-buffer-window "*writing-habit week*" t)
    (save-selected-window (writing-habit-table-report))))

;;;; Files

(defun writing-habit-table-rename-file ()
  "Rename the visited file to the canonical name of its grid.
The tracker files a week under the schedule code taken from the file name
at plan import, so a name that drifted from the grid files the week under
the wrong plan shape."
  (interactive)
  (unless buffer-file-name (user-error "Save the table to a file first"))
  (when (buffer-modified-p)
    (if (y-or-n-p "Save the table first? ") (save-buffer) (user-error "Save the table first")))
  (let* ((model (writing-habit-table-current))
         (new (condition-case err (writing-habit-table-rename-to-canonical model)
                (error (user-error "%s" (error-message-string err))))))
    (if (equal (expand-file-name new) (expand-file-name buffer-file-name))
        (message "The file already has its canonical name")
      (set-visited-file-name new t t)
      (message "Renamed to %s" (file-name-nondirectory new)))))

;;;; Menu and keymap

;;;###autoload (autoload 'writing-habit-table-menu "writing-habit-table-mode" nil t)
(transient-define-prefix writing-habit-table-menu ()
  "Commands for editing the weekly block table."
  ["Weekly table"
   ["Time blocks"
    ("a" "Insert block above" writing-habit-table-insert-above)
    ("b" "Insert block below" writing-habit-table-insert-below)
    ("<up>" "Move up" writing-habit-table-move-up :transient t)
    ("<down>" "Move down" writing-habit-table-move-down :transient t)]
   ["Legend"
    ("p" "Insert project above" writing-habit-table-insert-project-above)
    ("P" "Insert project below" writing-habit-table-insert-project-below)
    ("s" "Sync legend with grid" writing-habit-table-update-legend)]
   ["Week"
    ("r" "Report" writing-habit-table-report)
    ("c" "Rename to canonical" writing-habit-table-rename-file)
    ("n" "New week from template" writing-schedule-new-week-from-template)
    ("o" "Open most recent table" writing-schedule-open-recent)]])

(defvar writing-habit-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") #'writing-habit-table-move-up)
    (define-key map (kbd "M-<down>") #'writing-habit-table-move-down)
    (define-key map (kbd writing-habit-table-mode-prefix) #'writing-habit-table-menu)
    map)
  "Keymap of `writing-habit-table-mode'.")

;;;###autoload
(define-minor-mode writing-habit-table-mode
  "Minor mode for editing a weekly block table.
Tint clashes and the blocks clear of the one at point, complete legend
codes, describe the code at point, and give the table its own commands.

\\{writing-habit-table-mode-map}"
  :lighter (:eval (writing-habit-table--lighter))
  :keymap writing-habit-table-mode-map
  (if writing-habit-table-mode
      (progn
        (add-hook 'completion-at-point-functions
                  #'writing-habit-table-completion-at-point nil t)
        (add-hook 'eldoc-documentation-functions #'writing-habit-table-eldoc nil t)
        (add-hook 'after-save-hook #'writing-habit-table--update-report nil t)
        (eldoc-mode 1)
        (setq writing-habit-table--timer
              (run-with-idle-timer writing-habit-table-idle-delay t
                                   #'writing-habit-table--idle-refresh (current-buffer)))
        (writing-habit-table--refresh t))
    (remove-hook 'completion-at-point-functions #'writing-habit-table-completion-at-point t)
    (remove-hook 'eldoc-documentation-functions #'writing-habit-table-eldoc t)
    (remove-hook 'after-save-hook #'writing-habit-table--update-report t)
    (when writing-habit-table--timer (cancel-timer writing-habit-table--timer))
    (setq writing-habit-table--timer nil)
    (writing-habit-table--clear-overlays)))

(provide 'writing-habit-table-mode)
;;; writing-habit-table-mode.el ends here
