;;; writing-habit-name.el --- Decode writing-schedule file-name codes -*- lexical-binding: t; -*-

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

(require 'calendar)
(require 'seq)
(require 'subr-x)

(defconst writing-habit-name-days
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "Day names in order, used to label the decoded week.")

(defconst writing-habit-name-activities
  '((?g . "generative") (?e . "editing") (?s . "support"))
  "Map the lowercase activity letters to their full names.")

(defconst writing-habit-name--group-re
  "\\`\\([0-9]*\\)\\([A-Za-z][A-Za-z0-9]*\\)\\'"
  "Match one day-group: an optional day count then a pattern.
The pattern starts with a letter, so the digits before it are the day
count, and a digit after an activity letter is a project.")

(defconst writing-habit-name-code-re
  "\\(?:[A-Z][A-Z0-9]\\{0,3\\}\\|[0-9]\\)"
  "A project code: a capital and up to three capitals or digits, or one digit.
The digits add ten one-character codes, which a table file name can
carry.  Match with `case-fold-search' bound to nil.")

(defconst writing-habit-name--legend-re
  (concat "\\`\\(" writing-habit-name-code-re "\\)[ \t]*:[ \t]*\\(.*\\)\\'")
  "Match a legend cell: a code, a colon, then a description.")

(defconst writing-habit-name--time-re
  "[0-9]\\{1,2\\}:[ \t]*[0-9]\\{2\\}[ \t]*-+[ \t]*[0-9]\\{1,2\\}:[ \t]*[0-9]\\{2\\}"
  "Match a time range such as 05:00-06:30 or 5:00-06:30.")

(defun writing-habit-name--legend-match (cell)
  "Match CELL as a legend cell and return non-nil, setting the match data.
A label such as 5:00-06:30 is a time, so it never matches as the legend
row of code 5."
  (let ((case-fold-search nil))
    (and (string-match writing-habit-name--legend-re cell)
         (not (and (string-match-p "\\`[0-9]\\'" (match-string 1 cell))
                   (string-match-p writing-habit-name--time-re cell))))))

(defconst writing-habit-name--activity-re
  "\\(?:\\`\\|[ \t]\\)@\\(generative\\|editing\\|support\\)\\b"
  "Match a default activity tag in a legend description, such as @support.")

(defun writing-habit-name-split-legend-activity (desc)
  "Return (DESCRIPTION . ACTIVITY) for the legend description DESC.
The first tag such as @support is removed from DESCRIPTION, and ACTIVITY
is \"generative\", \"editing\", or \"support\", or nil when DESC has
no tag.  The scheduler reads the same tag, so a bare code in a cell takes
this activity when the cell carries no activity letter of its own."
  (let ((case-fold-search t)
        (desc (or desc "")))
    (if (string-match writing-habit-name--activity-re desc)
        (let* ((activity (downcase (match-string 1 desc)))
               (rest (string-trim (concat (substring desc 0 (match-beginning 0))
                                          (substring desc (match-end 0))))))
          (cons (replace-regexp-in-string "[ \t]\\{2,\\}" " " rest) activity))
      (cons desc nil))))

(defun writing-habit-name-legend-activity (cell)
  "Return the default activity named in the legend CELL, or nil."
  (let ((cell (string-trim (or cell ""))))
    (when (writing-habit-name--legend-match cell)
      (cdr (writing-habit-name-split-legend-activity
            (string-trim (match-string 2 cell)))))))

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
               ((or (and (<= ?A ch) (<= ch ?Z)) (and (<= ?0 ch) (<= ch ?9)))
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
                       (or (and (<= ?A (aref project 0)) (<= (aref project 0) ?Z))
                           (and (<= ?0 (aref project 0)) (<= (aref project 0) ?9))))
            (error "Project must be one uppercase letter or one digit, got %S"
                   project))
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
activity.  A default activity tag such as @support is stripped too; read
it with `writing-habit-name-legend-activity'.  Callers that hold a table
in memory, such as the table editor, use this so one rule governs both
the reader and the editor."
  (let ((cell (string-trim cell)))
    (when (writing-habit-name--legend-match cell)
      (let ((code (match-string 1 cell))
            (desc (car (writing-habit-name-split-legend-activity
                        (string-trim (match-string 2 cell)))))
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
scheduler looks a code up with `assoc'.  Only the first org table of
the file is read, which is the week itself, as the scheduler reads it;
the semester project lists that may follow are not its legend.  This
matches `read_legend' in the Python package."
  (let ((legend '()) (seen nil) (done nil))
    (with-temp-buffer
      (insert-file-contents table-path)
      (goto-char (point-min))
      (while (and (not (eobp)) (not done))
        (let ((s (string-trim
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))))
          (if (string-prefix-p "|" s)
              (setq seen t)
            (when seen (setq done t)))
          (when (and (not done) (string-prefix-p "|" s)
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

;;;; Project name and deadline
;;
;; A legend description holds the project name and, optionally, a deadline as
;; an ISO date at its end, as in "Z: 1008ToolsForQCrOfRNA 2026-10-05 :safe:".
;; The scheduler reads the description as free text, so the date travels with
;; it unchanged.  These functions mirror the ones of the same name in the
;; Python package, which the parity probes compare.

(defconst writing-habit-name--iso-tail-re
  "\\(?:,[ \t\n]*\\|[ \t\n]+\\)\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[ \t\n]*\\'"
  "An ISO date at the end of a description, in group 1.")

(defconst writing-habit-name--project-token-re
  "\\(?:\\`\\|[^0-9A-Za-z]\\)\\([0-9]\\{4\\}[A-Za-z][[:alnum:]_-]*\\)"
  "A project directory name such as 1008ToolsForQCrOfRNA, in group 1.")

(defconst writing-habit-name--months
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oc" . 10) ("nov" . 11) ("dec" . 12))
  "The month each name prefix stands for; oc covers a slip such as Ocotober.")

(defconst writing-habit-name--loose-date-re
  (concat "\\(?1:\\(?:submitted\\|sent\\)[ \t\n]+\\)?"
          "\\(?:\\(?:due\\|by\\|before\\)[ \t\n]*:?[ \t\n]*\\)?"
          "\\(?:\\(?2:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          "\\|\\(?3:jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oc\\|nov\\|dec\\)"
          "[a-z]*\\.?[ \t\n]+"
          "\\(?4:[0-9]\\{1,2\\}\\)\\(?:st\\|nd\\|rd\\|th\\)?"
          "\\(?7:,?[ \t\n]*\\(?5:[0-9]\\{4\\}\\)\\)?"
          "\\|\\(?8:[0-9]\\{1,2\\}\\)/\\(?9:[0-9]\\{1,2\\}\\)"
          "\\(?:/\\(?6:[0-9]\\{2,4\\}\\)\\)?\\)")
  "A loose date: ISO, a month name and day, or a numeric month and day.
Group 1 is a lead word such as submitted.  Group 2 is an ISO date, groups 3,
4, and 5 a month, day, and year, and groups 8, 9, and 6 a numeric month,
day, and year.  Group 7 is the optional year with its separator.")

(defun writing-habit-name--loose-dates (text)
  "Return every loose date in TEXT as a plist, in order.
Each plist has :start, :end, :text, :lead, :iso, :mon, :day, :year, :m,
:d, and :y.  A year directly followed by a letter or a digit, as in
\"Sep 16, 0382CCinJN\", is not a year, so the match ends at the day."
  (let ((case-fold-search t) (pos 0) (out '()))
    (while (string-match writing-habit-name--loose-date-re text pos)
      (let* ((start (match-beginning 0)) (end (match-end 0))
             (year (match-string 5 text)))
        (when (and year (< end (length text))
                   (string-match-p "[[:alnum:]]" (substring text end (1+ end))))
          (setq end (match-beginning 7) year nil))
        (push (list :start start :end end :text (substring text start end)
                    :lead (match-string 1 text) :iso (match-string 2 text)
                    :mon (match-string 3 text) :day (match-string 4 text) :year year
                    :m (match-string 8 text) :d (match-string 9 text) :y (match-string 6 text))
              out)
        (setq pos (max end (1+ start)))))
    (nreverse out)))

(defun writing-habit-name--date-abs (year month day)
  "Return the absolute day of YEAR, MONTH, and DAY, or nil when it is no date."
  (when (and (<= 1 month 12) (<= 1 day 31))
    (let ((abs (calendar-absolute-from-gregorian (list month day year))))
      (and (equal (calendar-gregorian-from-absolute abs) (list month day year)) abs))))

(defun writing-habit-name--iso (abs)
  "Return the ISO date of the absolute day ABS."
  (let ((g (calendar-gregorian-from-absolute abs)))
    (format "%04d-%02d-%02d" (nth 2 g) (nth 0 g) (nth 1 g))))

(defun writing-habit-name--resolve (date reference)
  "Return the absolute day DATE names, guessing its year from REFERENCE.
DATE is a plist from `writing-habit-name--loose-dates', and REFERENCE an
absolute day.  A month and day with no year take the reference year,
unless that puts them more than about two months before the reference,
when they take the next year."
  (if (plist-get date :iso)
      (let ((iso (plist-get date :iso)))
        (writing-habit-name--date-abs (string-to-number (substring iso 0 4))
                                      (string-to-number (substring iso 5 7))
                                      (string-to-number (substring iso 8 10))))
    (let* ((ref-year (nth 2 (calendar-gregorian-from-absolute reference)))
           month day year)
      (if (plist-get date :mon)
          (let ((key (downcase (plist-get date :mon))))
            (setq month (cdr (assoc (if (string-prefix-p "oc" key) "oc" (substring key 0 3))
                                    writing-habit-name--months))
                  day (string-to-number (plist-get date :day))
                  year (plist-get date :year)))
        (setq month (string-to-number (plist-get date :m))
              day (string-to-number (plist-get date :d))
              year (plist-get date :y))
        (when (and year (= (length year) 2)) (setq year (concat "20" year))))
      (if year
          (writing-habit-name--date-abs (string-to-number year) month day)
        (let ((guess (writing-habit-name--date-abs ref-year month day)))
          (cond ((null guess) nil)
                ((< guess (- reference 62))
                 (writing-habit-name--date-abs (1+ ref-year) month day))
                (t guess)))))))

(defun writing-habit-name-split-deadline (description)
  "Return (PROJECT . DEADLINE) for a legend DESCRIPTION.
DEADLINE is an ISO date at the end of DESCRIPTION, or nil.  Text in any
other shape stays in PROJECT, so nothing is lost."
  (let ((text (string-trim (or description ""))))
    (if (string-match writing-habit-name--iso-tail-re text)
        (cons (string-trim-right (substring text 0 (match-beginning 0)) "[ ,]+")
              (match-string 1 text))
      (cons (string-trim-right text "[ ,]+") nil))))

(defun writing-habit-name-join-deadline (project deadline)
  "Return the description that `writing-habit-name-split-deadline' splits.
PROJECT is the project name and DEADLINE an ISO date or nil."
  (let ((project (string-trim-right (string-trim (or project "")) "[ ,]+"))
        (deadline (string-trim (or deadline ""))))
    (if (string-empty-p deadline) project (string-trim (concat project " " deadline)))))

(defun writing-habit-name-valid-deadline-p (text)
  "Return non-nil when TEXT is empty or a real calendar date as YYYY-MM-DD."
  (let ((text (string-trim (or text ""))))
    (or (string-empty-p text)
        (and (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" text)
             (writing-habit-name--date-abs (string-to-number (substring text 0 4))
                                           (string-to-number (substring text 5 7))
                                           (string-to-number (substring text 8 10)))
             t))))

(defun writing-habit-name--project-tokens (text)
  "Return the project directory names in TEXT, in order."
  (let ((case-fold-search nil) (pos 0) (out '()))
    (while (string-match writing-habit-name--project-token-re text pos)
      (push (match-string 1 text) out)
      (setq pos (match-end 1)))
    (nreverse out)))

(defun writing-habit-name--replace-all (from to text)
  "Return TEXT with every literal FROM replaced by TO."
  (replace-regexp-in-string (regexp-quote from) to text t t))

(defun writing-habit-name-tidy-description (description &optional reference)
  "Propose (PROJECT DEADLINE DROPPED) for the free-text DESCRIPTION.
PROJECT is every project directory name in the text, such as
1008ToolsForQCrOfRNA, joined with \", \" when there are two; a description
that names none keeps its text before the first comma.  DEADLINE is the
last date in the text as YYYY-MM-DD, or nil; a date led by submitted or
sent records something done and is not used.  DROPPED is the text that
fits neither part.  REFERENCE is the absolute day that guesses a missing
year, today by default."
  (let* ((reference (or reference (calendar-absolute-from-gregorian
                                   (calendar-current-date))))
         (text (string-trim (or description "")))
         (already (writing-habit-name-split-deadline text)))
    (if (and (cdr already)
             (null (writing-habit-name--loose-dates (car already)))
             (or (let ((case-fold-search nil))
                   (string-match-p "\\`[0-9]\\{4\\}[A-Za-z][[:alnum:]_-]*\\'" (car already)))
                 (not (string-match-p "," (car already)))))
        (list (car already) (cdr already) "")
      (let ((deadline nil) (chosen nil) (rest text) project)
        (dolist (date (writing-habit-name--loose-dates text))
          (unless (plist-get date :lead)
            (let ((found (writing-habit-name--resolve date reference)))
              (when found
                (setq deadline (writing-habit-name--iso found) chosen date)))))
        (let ((projects (writing-habit-name--project-tokens text)))
          (if projects
              (progn
                (setq project (mapconcat #'identity (delete-dups (copy-sequence projects)) ", "))
                (dolist (token projects)
                  (setq rest (writing-habit-name--replace-all token " " rest))))
            (let* ((head (car (split-string text ",")))
                   (cut (car (writing-habit-name--loose-dates head))))
              (setq project (string-trim (if cut (substring head 0 (plist-get cut :start)) head)))
              (setq rest (substring rest (min (length rest) (length project)))))))
        (when chosen
          (setq rest (writing-habit-name--replace-all (plist-get chosen :text) " " rest)))
        (let ((dropped (replace-regexp-in-string
                        "[ \t\n]+" " "
                        (replace-regexp-in-string "\\(?:[ \t\n]*,[ \t\n]*\\)+" ", " rest))))
          (list project deadline (string-trim dropped "[ ,]+" "[ ,]+")))))))

(defun writing-habit-name-number-key (project)
  "Return a sort key for PROJECT: its four-digit number, unnumbered last.
The key is a list (RANK NUMBER), compared with `writing-habit-name-key<'."
  (if (string-match "\\`[ \t]*\\([0-9]\\{4\\}\\)" (or project ""))
      (list 0 (string-to-number (match-string 1 project)))
    (list 1 0)))

(defun writing-habit-name-key< (a b)
  "Return non-nil when the sort key A comes before the sort key B."
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cadr a) (cadr b)))))

(provide 'writing-habit-name)
;;; writing-habit-name.el ends here
