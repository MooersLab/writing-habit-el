;;; writing-habit-name.el --- Decode writing-schedule file-name codes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT license.

;;; Commentary:

;; This is Phase 1 of the Emacs Lisp port of the Python writing-habit
;; package.  It is a direct port of `name.py' and needs neither a database
;; nor a third-party package, so it stands on its own.
;;
;; A schedule code names a weekly table file, for example
;; `4gAAeAsA-gWW.org'.  See `docs/table-file-naming-rules.org' for the full
;; specification.  The grammar:
;;
;;     schedule = daygroup { "-" daygroup }
;;     daygroup = [count] pattern
;;     pattern  = "o" | run+
;;     run      = activity project+
;;     activity = g | e | s          (generative, editing, support)
;;     project  = A..Z               (one letter is one block)
;;     count    = digits             (consecutive days, a leading 1 is omitted)
;;
;; This module decodes a code into the week it represents and checks the
;; project letters against a weekly table legend.
;;
;; Entry points:
;;   `writing-habit-name'                interactive command, shows a report
;;   `writing-habit-name-decode'         code -> alist of (DAY . BLOCKS)
;;   `writing-habit-name-summary'        block totals by activity and project
;;   `writing-habit-name-read-legend'    read a weekly table legend
;;   `writing-habit-name-check-against-legend'  match letters to the legend

;;; Code:

(require 'seq)
(require 'subr-x)

(defconst writing-habit-name-days
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "Day names in order, used to label the decoded week.")

(defconst writing-habit-name-activities
  '((?g . "generative") (?e . "editing") (?s . "support"))
  "Map the lowercase activity letters to their full names.")

(defconst writing-habit-name--group-re
  "\\`\\([0-9]*\\)\\([A-Za-z]+\\)\\'"
  "Match one day-group: an optional day count then a letter pattern.")

(defconst writing-habit-name--legend-re
  "\\`\\([A-Z][A-Z0-9]\\{0,3\\}\\)[ \t]*:[ \t]*\\(.*\\)\\'"
  "Match a legend cell: an uppercase code, a colon, then a description.")

(defconst writing-habit-name--risk-re
  "\\(?:(\\(safe\\|speculative\\|support\\))\\|:\\(safe\\|speculative\\|support\\):\\)[ \t]*\\'"
  "Match a trailing risk tag in either the (safe) or :safe: form.")


;;;; Decoding

(defun writing-habit-name-decode (code)
  "Return an alist of (DAY . BLOCKS) for the week named by CODE.
DAY is a string from `writing-habit-name-days'.  BLOCKS is a list of
cons cells (ACTIVITY . PROJECT), both strings, in the order the blocks
appear in CODE.  An open day has an empty BLOCKS list.  Signal an error
when CODE is malformed."
  (let ((days '()))
    (dolist (grp (split-string code "-"))
      (unless (string-match writing-habit-name--group-re grp)
        (error "Invalid day-group %S" grp))
      (let* ((count-str (match-string 1 grp))
             (pat (match-string 2 grp))
             (n (if (string= count-str "") 1 (string-to-number count-str)))
             (blocks '()))
        (if (string= pat "o")
            (setq blocks '())
          (let ((act nil))
            (dolist (ch (append pat nil))
              (cond
               ((assq ch writing-habit-name-activities)
                (setq act ch))
               ((and (<= ?A ch) (<= ch ?Z))
                (unless act
                  (error "Project %c before any activity in %S" ch grp))
                (push (cons (cdr (assq act writing-habit-name-activities))
                            (char-to-string ch))
                      blocks))
               (t (error "Invalid character %c in %S" ch grp))))
            (setq blocks (nreverse blocks))))
        (dotimes (_ n)
          (push blocks days))))
    (setq days (nreverse days))
    (when (null days)
      (error "Empty schedule code"))
    (when (> (length days) 7)
      (error "Schedule covers %d days, more than a week" (length days)))
    (let ((i -1))
      (mapcar (lambda (blocks)
                (setq i (1+ i))
                (cons (nth i writing-habit-name-days) blocks))
              days))))

(defun writing-habit-name-format-week (decoded)
  "Return a short multi-line description of DECODED, one line per day."
  (mapconcat
   (lambda (entry)
     (let ((day (car entry))
           (blocks (cdr entry)))
       (if (null blocks)
           (format "  %s  open" day)
         (format "  %s  %s" day
                 (mapconcat (lambda (b) (format "%s %s" (car b) (cdr b)))
                            blocks ", ")))))
   decoded "\n"))

(defun writing-habit-name--incr (alist key)
  "Return ALIST with the count for KEY raised by one, preserving order."
  (let ((cell (assoc key alist)))
    (if cell
        (progn (setcdr cell (1+ (cdr cell))) alist)
      (append alist (list (cons key 1))))))

(defun writing-habit-name-summary (decoded)
  "Return a list (TOTAL ACT-COUNTS PROJ-COUNTS) for DECODED.
TOTAL is the block count.  ACT-COUNTS and PROJ-COUNTS are alists that map
an activity name or a project letter to its block count."
  (let ((act '()) (proj '()) (total 0))
    (dolist (entry decoded)
      (dolist (b (cdr entry))
        (setq total (1+ total))
        (setq act (writing-habit-name--incr act (car b)))
        (setq proj (writing-habit-name--incr proj (cdr b)))))
    (list total act proj)))


;;;; Legend reading and checking

(defun writing-habit-name-read-legend (table-path)
  "Return the legend of the weekly org table at TABLE-PATH.
The result is an alist of (CODE . (DESCRIPTION RISK)).  RISK is a
lowercase string, one of \"safe\" or \"speculative\", or nil.  Support is an
activity category, not a risk class, so a legacy support tag records no risk.
A legend row carries the code and description in its first cell, for
example |A: DNPH1 docking :safe:|.  A trailing risk tag is stripped."
  (let ((legend '()))
    (with-temp-buffer
      (insert-file-contents table-path)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((s (string-trim
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))))
          (when (and (string-prefix-p "|" s)
                     (not (string-match-p "\\`[|+ -]*\\'" s)))
            (let ((first (car (split-string s "[|]" t "[ \t]*"))))
              (when (and first
                         (let ((case-fold-search nil))
                           (string-match writing-habit-name--legend-re first)))
                (let ((code (match-string 1 first))
                      (desc (string-trim (match-string 2 first)))
                      (risk nil))
                  (let ((case-fold-search t))
                    (when (string-match writing-habit-name--risk-re desc)
                      (let ((tag (downcase (or (match-string 1 desc)
                                               (match-string 2 desc)))))
                        ;; Only safe and speculative are risk classes; a legacy
                        ;; support tag is stripped but records no risk.
                        (setq risk (and (member tag '("safe" "speculative")) tag)))
                      (setq desc (string-trim
                                  (replace-regexp-in-string
                                   writing-habit-name--risk-re "" desc)))))
                  (setq legend (assoc-delete-all code legend))
                  (setq legend (append legend (list (cons code (list desc risk))))))))))
        (forward-line 1)))
    legend))

(defun writing-habit-name-check-against-legend (decoded legend)
  "Match every project letter in DECODED to an entry in LEGEND.
Return a list (ROWS PROBLEMS).  Each element of ROWS is
 (LETTER MATCHED-CODE DESCRIPTION RISK STATUS), where STATUS is one of
\"exact\", \"alias\", \"ambiguous\", or \"unknown\".  PROBLEMS lists the
letters that did not resolve to exactly one legend entry."
  (let ((letters '()))
    (dolist (entry decoded)
      (dolist (b (cdr entry))
        (unless (member (cdr b) letters)
          (setq letters (append letters (list (cdr b)))))))
    (let ((rows '()) (problems '()))
      (dolist (letter letters)
        (if (assoc letter legend)
            (let ((dr (cdr (assoc letter legend))))
              (setq rows (append rows
                                 (list (list letter letter
                                             (nth 0 dr) (nth 1 dr) "exact")))))
          (let ((prefix (seq-filter
                         (lambda (c) (and (> (length c) 0)
                                          (eq (aref c 0) (aref letter 0))))
                         (mapcar #'car legend))))
            (cond
             ((= (length prefix) 1)
              (let ((dr (cdr (assoc (car prefix) legend))))
                (setq rows (append rows
                                   (list (list letter (car prefix)
                                               (nth 0 dr) (nth 1 dr) "alias"))))))
             ((> (length prefix) 1)
              (setq rows (append rows
                                 (list (list letter
                                             (mapconcat #'identity
                                                        (sort (copy-sequence prefix)
                                                              #'string<)
                                                        "/")
                                             "" nil "ambiguous"))))
              (setq problems (append problems (list letter))))
             (t
              (setq rows (append rows (list (list letter "-" "" nil "unknown"))))
              (setq problems (append problems (list letter))))))))
      (list rows problems))))


;;;; Report and command

(defun writing-habit-name-report-string (code &optional table)
  "Return a human-readable report for schedule CODE.
When TABLE is non-nil, or a file named CODE.org exists in the current
directory, append a legend check."
  (let* ((decoded (writing-habit-name-decode code))
         (out (list (format "Schedule %s" code)
                    (writing-habit-name-format-week decoded))))
    (pcase-let ((`(,total ,act ,proj) (writing-habit-name-summary decoded)))
      (let* ((order '("generative" "editing" "support"))
             (by-act (mapconcat
                      (lambda (a) (format "%d %s" (or (cdr (assoc a act)) 0) a))
                      order ", "))
             (projects (sort (mapcar #'car proj) #'string<))
             (by-proj (mapconcat
                       (lambda (p) (format "%s (%d)" p (cdr (assoc p proj))))
                       projects ", ")))
        (setq out (append out
                          (list (format "\n%d blocks over %d days: %s"
                                        total (length decoded) by-act))))
        (when projects
          (setq out (append out (list (format "projects used: %s" by-proj)))))))
    (let ((tbl table))
      (when (and (null tbl) (file-exists-p (concat code ".org")))
        (setq tbl (concat code ".org")))
      (when tbl
        (pcase-let* ((legend (writing-habit-name-read-legend tbl))
                     (`(,rows ,problems)
                      (writing-habit-name-check-against-legend decoded legend)))
          (setq out (append out (list (format "\nLegend check against %s:" tbl))))
          (dolist (row rows)
            (pcase-let ((`(,letter ,mcode ,desc ,risk ,status) row))
              (let* ((rk (if (and risk (> (length risk) 0)) (format " [%s]" risk) ""))
                     (detail (string-trim-right (format "%s  %s%s" mcode desc rk))))
                (setq out (append out
                                  (list (format "  %s -> %-40s %s"
                                                letter detail status)))))))
          (when problems
            (setq out (append out
                              (list (format
                                     "\n%d project letter(s) not resolved to a legend entry: %s"
                                     (length problems)
                                     (mapconcat #'identity problems ", ")))))))))
    (mapconcat #'identity out "\n")))

;;;###autoload
(defun writing-habit-name (code &optional table)
  "Decode schedule CODE and show the week it represents in a buffer.
Interactively, default CODE to the base name of the current buffer's
file.  With a prefix argument, also prompt for a weekly table TABLE whose
legend the project letters are checked against.  With no table, a file
named CODE.org in the current directory is used when it exists."
  (interactive
   (let* ((default (when buffer-file-name (file-name-base buffer-file-name)))
          (code (read-string
                 (if default
                     (format "Schedule code (%s): " default)
                   "Schedule code: ")
                 nil nil default))
          (table (when current-prefix-arg
                   (read-file-name "Weekly table: " nil nil t))))
     (list code table)))
  (let ((buf (get-buffer-create "*writing-habit name*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (writing-habit-name-report-string code table))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)))

(provide 'writing-habit-name)
;;; writing-habit-name.el ends here
