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
;; This module decodes a code into the week it represents, encodes a week
;; back into its canonical code, and checks the project letters against a
;; weekly table legend.
;;
;; Entry points:
;;   `writing-habit-name'                interactive command, shows a report
;;   `writing-habit-name-decode'         code -> alist of (DAY . BLOCKS)
;;   `writing-habit-name-encode'         week -> the canonical code
;;   `writing-habit-name-encode-day'     one day of blocks -> its pattern
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
  "\\(?:(\\(safe\\|risky\\|support\\))\\|:\\(safe\\|risky\\|support\\):\\)[ \t]*\\'"
  "Match a trailing risk tag in either the (safe) or :safe: form.")

(defconst writing-habit-name-tag-to-risk
  '(("safe" . "safe") ("risky" . "speculative"))
  "Map the tag a writer types to the risk class the database stores.
The class keeps the name \"speculative\", which the schema and both
dashboards use, while the tag in a table reads :risky:.  Support is an
activity category rather than a class, so it names nothing.")


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


;;;; Encoding

(defconst writing-habit-name-risk-to-tag
  '(("safe" . "safe") ("speculative" . "risky"))
  "Map the stored risk class to the tag written in a table.")

(defun writing-habit-name--activity-letter (activity)
  "Return the code letter for ACTIVITY, which is a name or a letter string."
  (or (car (rassoc activity writing-habit-name-activities))
      (and (= (length activity) 1)
           (assq (aref activity 0) writing-habit-name-activities)
           (aref activity 0))
      (error "Unknown activity %S" activity)))

(defun writing-habit-name-encode-day (blocks)
  "Return the day-pattern for BLOCKS, for example \"gAAeAsA\" or \"o\".
BLOCKS is a list of cons cells (ACTIVITY . PROJECT) in the order the
blocks occur across the day.  A run covers consecutive blocks that share
both the activity and the project, so three support blocks on B give
\"sBBB\".  A change of project opens a new run and repeats the activity
letter, so support on B then C then D reads \"sBBBsCCCsD\"."
  (if (null blocks)
      "o"
    (let ((out '()) (current nil))
      (dolist (b blocks)
        (let ((letter (writing-habit-name--activity-letter (car b)))
              (project (cdr b)))
          (unless (and (stringp project) (= (length project) 1)
                       (<= ?A (aref project 0)) (<= (aref project 0) ?Z))
            (error "Project must be one uppercase letter, got %S" project))
          (unless (equal (cons letter project) current)
            (push (char-to-string letter) out)
            (setq current (cons letter project)))
          (push project out)))
      (apply #'concat (nreverse out)))))

(defun writing-habit-name-encode (week)
  "Return the canonical schedule code for WEEK.
WEEK is either the alist `writing-habit-name-decode' returns or a bare
list of block lists, one per day, filled from Monday.  Every maximal run
of identical consecutive days collapses into one group carrying the day
count, a count of one is omitted, and trailing open days are dropped
because they are implied.  Days that share a pattern without being
adjacent are written out again, so a Monday, Wednesday, Friday week reads
\"gA-o-gA-o-gA\".  The function inverts `writing-habit-name-decode' for
every canonical code."
  (when (null week)
    (error "Empty week"))
  (when (> (length week) 7)
    (error "Week covers %d days, more than a week" (length week)))
  (let ((patterns
         (mapcar (lambda (day)
                   (writing-habit-name-encode-day
                    (if (and (consp day) (stringp (car day))
                             (member (car day) writing-habit-name-days))
                        (cdr day)
                      day)))
                 week)))
    ;; Trailing open days are implied, so drop them.
    (setq patterns (nreverse patterns))
    (while (and patterns (string= (car patterns) "o"))
      (setq patterns (cdr patterns)))
    (setq patterns (nreverse patterns))
    (if (null patterns)
        "o"
      (let ((groups '()) (run 1) (i 0) (n (length patterns)))
        (while (< i n)
          (let ((pat (nth i patterns)))
            (if (and (< (1+ i) n) (string= (nth (1+ i) patterns) pat))
                (setq run (1+ run))
              (push (if (> run 1) (format "%d%s" run pat) pat) groups)
              (setq run 1)))
          (setq i (1+ i)))
        (mapconcat #'identity (nreverse groups) "-")))))


;;;; Legend reading and checking

(defun writing-habit-name-parse-legend-cell (cell)
  "Return (CODE DESCRIPTION RISK) for the legend CELL, or nil.
A legend row carries the code and the description in its first cell, for
example \"A: DNPH1 docking :safe:\".  A trailing risk tag in either the
:safe: or the (safe) form is stripped from the description.  Two tags
name a class, safe and risky, and risky names the class the database
calls speculative, so RISK is \"safe\", \"speculative\", or nil.  A legacy
support tag is stripped and names nothing, because support is an
activity.  Callers that hold a table in memory, such as the table editor,
use this so one rule governs both the reader and the editor."
  (let ((cell (string-trim cell)))
    (when (let ((case-fold-search nil))
            (string-match writing-habit-name--legend-re cell))
      (let ((code (match-string 1 cell))
            (desc (string-trim (match-string 2 cell)))
            (risk nil))
        (let ((case-fold-search t))
          (when (string-match writing-habit-name--risk-re desc)
            (let ((tag (downcase (or (match-string 1 desc) (match-string 2 desc)))))
              (setq risk (cdr (assoc tag writing-habit-name-tag-to-risk))))
            (setq desc (string-trim
                        (replace-regexp-in-string writing-habit-name--risk-re "" desc)))))
        (list code desc risk)))))

(defun writing-habit-name-read-legend (table-path)
  "Return the legend of the weekly org table at TABLE-PATH.
The result is an alist of (CODE . (DESCRIPTION RISK)) in table order,
where RISK is \"safe\", \"speculative\", or nil, as parsed by
`writing-habit-name-parse-legend-cell'.  A code defined twice keeps its
first definition, which is what the plan importer sees, because the
scheduler looks a code up with `assoc'.  This matches `read_legend' in
the Python package."
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
            (let* ((inner (string-trim s "|+" "|+"))
                   (parsed (writing-habit-name-parse-legend-cell
                            (car (split-string inner "|")))))
              (when (and parsed (not (assoc (car parsed) legend)))
                (push (cons (car parsed) (cdr parsed)) legend)))))
        (forward-line 1)))
    (nreverse legend)))

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
