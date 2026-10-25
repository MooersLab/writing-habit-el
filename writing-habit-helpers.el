;;; writing-habit-helpers.el --- Capture, numbering, triage, and stuck projects -*- lexical-binding: t; -*-

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

;; The twin of the Python modules capture.py, numbering.py, triage.py, and
;; stuck.py.  Each helper computes its facts and proposals here, by rules
;; a writer can check, and the coach only words a note around them.
;;
;; Quick capture turns a thought in plain words into a coded grasscatcher
;; task.  Numbering help suggests a free four-digit number beside related
;; projects and flags a project forming inside another.  Roll-forward
;; triage suggests keeping, deferring, or dropping each carried-over
;; project.  The stuck-project detector names the projects whose
;; objectives keep going unmet or whose next step repeats.
;;
;; Commands:
;;   `writing-habit-helpers-capture'          file a thought on the grasscatcher
;;   `writing-habit-helpers-number-project'   suggest a number for a new project
;;   `writing-habit-helpers-triage-carried'   keep, defer, or drop carried projects
;;   `writing-habit-helpers-find-stuck'       report the stuck projects

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-coach)
(require 'writing-habit-db)
(require 'writing-habit-lists)
(require 'writing-habit-name)
(require 'writing-habit-table)

(defvar writing-habit-dispatch--last-db)
(defvar writing-habit-default-db)
(declare-function writing-habit-table-current "writing-habit-table-mode")
(declare-function writing-habit-table--apply "writing-habit-table-mode")

(defgroup writing-habit-helpers nil
  "Capture, numbering, triage, and stuck projects."
  :group 'tools
  :prefix "writing-habit-helpers-")

(defcustom writing-habit-helpers-projects-root "~"
  "The folder whose numbered subfolders are the writing projects."
  :type 'directory)

(defcustom writing-habit-helpers-semester-end nil
  "The last day of the current semester as YYYY-MM-DD, or nil.
With nil, triage takes the day sixteen weeks from today."
  :type '(choice (const :tag "Sixteen weeks from today" nil) string))

;;;; Small shared helpers

(defun writing-habit-helpers--fmt-number (value)
  "Return VALUE as Python prints a number, an integer without a decimal."
  (if (and (floatp value) (= value (ftruncate value)))
      (format "%d" (truncate value))
    (format "%s" value)))

(defun writing-habit-helpers--abs (iso)
  "Return the absolute day number of the ISO date ISO."
  (calendar-absolute-from-gregorian
   (list (string-to-number (substring iso 5 7))
         (string-to-number (substring iso 8 10))
         (string-to-number (substring iso 0 4)))))

(defun writing-habit-helpers--iso (abs)
  "Return the ISO date of the absolute day number ABS."
  (let ((date (calendar-gregorian-from-absolute abs)))
    (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date))))

(defun writing-habit-helpers--today ()
  "Return today as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d"))

(defun writing-habit-helpers--project-number (project)
  "Return the leading four-digit number of PROJECT, or nil."
  (when (string-match "\\`[ \t\n]*\\([0-9]\\{4\\}\\)" (or project ""))
    (match-string 1 project)))

(defun writing-habit-helpers--table-p (db name)
  "Return non-nil when DB has a table called NAME."
  (sqlite-select db "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?"
                 (list name)))

(defun writing-habit-helpers--series (items)
  "Join ITEMS as an English series."
  (cond ((null items) "")
        ((null (cdr items)) (car items))
        (t (concat (string-join (butlast items) ", ")
                   (if (cddr items) ", and " " and ")
                   (car (last items))))))

;;;; Quick capture

(defconst writing-habit-helpers--cookie-re
  "\\[#\\([A-Za-z][0-9]*\\|[0-9]+\\)\\]"
  "A priority cookie such as [#A1], with the priority in group 1.")

(defconst writing-habit-helpers--priority-words-re
  "\\b\\(?:with[ \t\n]+\\)?priority[ \t\n]+\\([A-Za-z][0-9]*\\|[0-9]+\\)\\b[,.;]?"
  "Words such as priority A, with the priority in group 1.")

(defconst writing-habit-helpers--lead-code-re
  "\\`[ \t\n]*\\([A-Z]\\{1,2\\}\\|[0-9]\\)[ \t\n]*:[ \t\n]*"
  "A legend code written at the start, such as A:.")

(defconst writing-habit-helpers--project-code-re
  "\\bproject[ \t\n]+\\([A-Z]\\{1,2\\}\\|[0-9]\\)\\b"
  "A code named as project A.")

(defconst writing-habit-helpers--openers-re
  (concat "\\`[ \t\n]*\\(?:\\(?:please[ \t\n]+\\)?remember[ \t\n]+to\\|"
          "remind[ \t\n]+me[ \t\n]+to\\|note[ \t\n]+to[ \t\n]+self[:,]?\\|"
          "i[ \t\n]+\\(?:need\\|have\\|want\\|ought\\)[ \t\n]+to\\|i[ \t\n]+should\\|"
          "don't[ \t\n]+forget[ \t\n]+to\\|do[ \t\n]+not[ \t\n]+forget[ \t\n]+to\\|"
          "todo[:,]?\\|to[ \t\n]+do[:,]?\\|task[:,]?\\)[ \t\n]+")
  "Openers such as remember to, which are not part of the task.")

(defconst writing-habit-helpers--capture-common
  '("the" "and" "for" "with" "paper" "note" "notes" "draft" "new"
    "data" "project" "review" "talk" "letter" "email")
  "Parts of project names too common to name a project on their own.")

(cl-defstruct (writing-habit-helpers-project
               (:constructor writing-habit-helpers-project-create)
               (:copier nil))
  "A coded project, with NAME the project name without its deadline."
  code name)

(cl-defstruct (writing-habit-helpers-captured
               (:constructor writing-habit-helpers-captured-create)
               (:copier nil))
  "A task ready for the grasscatcher and HOW its project was found.
CODE is nil when no project was found.  COOKIE is the priority, such as
A1, without the brackets."
  code task cookie (how ""))

(defun writing-habit-helpers-captured-entry-text (captured)
  "Return CAPTURED as grasscatcher text, with any cookie in front."
  (concat (if (writing-habit-helpers-captured-cookie captured)
              (format "[#%s] " (writing-habit-helpers-captured-cookie captured))
            "")
          (writing-habit-helpers-captured-task captured)))

(defun writing-habit-helpers--words (text)
  "Return each run of letters and digits in TEXT, in order."
  (let ((out '()) (start 0))
    (while (string-match "[A-Za-z0-9]+" text start)
      (push (match-string 0 text) out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun writing-habit-helpers--all-matches (regexp text &optional group)
  "Return the matches of REGEXP in TEXT, or of its GROUP."
  (let ((out '()) (start 0) (case-fold-search nil))
    (while (string-match regexp text start)
      (push (match-string (or group 0) text) out)
      (setq start (max (1+ (match-beginning 0)) (match-end 0))))
    (nreverse out)))

(defun writing-habit-helpers-capture-projects-from (legend lists)
  "Return the coded projects of LEGEND and LISTS, the legend first.
LEGEND is an alist of (CODE . DESCRIPTION) and LISTS a list of
\(CODE . DESCRIPTION) with CODE nil for an uncoded entry."
  (let ((out '()))
    (dolist (pair legend)
      (unless (assoc (car pair) out)
        (push (cons (car pair)
                    (writing-habit-helpers-project-create
                     :code (car pair)
                     :name (car (writing-habit-name-split-deadline (cdr pair)))))
              out)))
    (dolist (pair lists)
      (when (and (car pair) (not (assoc (car pair) out)))
        (push (cons (car pair)
                    (writing-habit-helpers-project-create
                     :code (car pair)
                     :name (car (writing-habit-name-split-deadline (cdr pair)))))
              out)))
    (mapcar #'cdr (nreverse out))))

(defun writing-habit-helpers--strip-number (name)
  "Return NAME without its leading four-digit number."
  (replace-regexp-in-string "\\`[ \t\n]*[0-9]\\{4\\}" "" name))

(defun writing-habit-helpers--capture-parts (name)
  "Split NAME, such as 1001grantR01, into its distinctive parts."
  (let ((rest (writing-habit-helpers--strip-number name))
        (out '()) (i 0) (case-fold-search nil))
    (while (< i (length rest))
      (let* ((tail (substring rest i))
             (piece
              (cond
               ((string-match "\\`[A-Z]+[0-9]+" tail) (match-string 0 tail))
               ((string-match "\\`[A-Z]?[a-z]+" tail) (match-string 0 tail))
               ((string-match "\\`[A-Z]\\{2,\\}" tail)
                ;; [A-Z]{2,}(?![a-z]) backs off until no lowercase follows.
                (let ((n (length (match-string 0 tail))))
                  (while (and (>= n 2) (< n (length tail))
                              (let ((c (aref tail n))) (and (>= c ?a) (<= c ?z))))
                    (setq n (1- n)))
                  (when (>= n 2) (substring tail 0 n))))
               ((string-match "\\`[0-9]+" tail) (match-string 0 tail)))))
        (if piece
            (progn (push piece out) (setq i (+ i (length piece))))
          (setq i (1+ i)))))
    (seq-filter (lambda (p) (and (>= (length p) 3)
                                 (not (member (downcase p)
                                              writing-habit-helpers--capture-common))))
                (nreverse out))))

(defun writing-habit-helpers-capture-evidence (text project)
  "Return (SCORE . REASON) for how plainly TEXT names PROJECT.
A legend code given as A: at the start or as project A scores 5, the
four-digit number 4, the whole name 3, and each distinctive part of the
name 1.  Zero means the words do not name the project."
  (let* ((case-fold-search nil)
         (code (writing-habit-helpers-project-code project))
         (name (writing-habit-helpers-project-name project))
         (lead (and (string-match writing-habit-helpers--lead-code-re text)
                    (match-string 1 text))))
    (if (or (equal lead code)
            (member code (writing-habit-helpers--all-matches
                          writing-habit-helpers--project-code-re text 1)))
        (cons 5 (format "its code %s" code))
      (let* ((words (mapcar #'downcase (writing-habit-helpers--words text)))
             (number (writing-habit-helpers--project-number name))
             (bare (downcase (writing-habit-helpers--strip-number name)))
             (squeezed (replace-regexp-in-string "[^a-z0-9]" "" (downcase text))))
        (cond
         ((and number
               (seq-some (lambda (w) (and (string-match-p "\\`[0-9]+\\'" w)
                                          (>= (length w) 2)
                                          (= (string-to-number w)
                                             (string-to-number number))))
                         words))
          (cons 4 (format "its number %s" number)))
         ((and (>= (length bare) 4)
               (not (member bare writing-habit-helpers--capture-common))
               (string-search bare squeezed))
          (cons 3 (format "its name %s" name)))
         (t
          (let ((hits (seq-filter (lambda (p) (member (downcase p) words))
                                  (writing-habit-helpers--capture-parts name))))
            (if hits
                (cons (length hits) (format "%s in its name" (string-join hits ", ")))
              (cons 0 "")))))))))

(defun writing-habit-helpers-capture-find-project (text projects)
  "Return (PROJECT . REASON) for the one project TEXT names best.
PROJECT is nil when none or a tie is found among PROJECTS."
  (let ((scored (seq-sort-by (lambda (item) (- (car (car item))))
                             #'<
                             (mapcar (lambda (p)
                                       (cons (writing-habit-helpers-capture-evidence text p) p))
                                     projects))))
    (cond
     ((or (null scored) (zerop (car (car (car scored)))))
      (cons nil "no project named in your words"))
     ((and (cdr scored) (= (car (car (cadr scored))) (car (car (car scored)))))
      (cons nil (format "your words fit both %s and %s"
                        (writing-habit-helpers-project-code (cdr (car scored)))
                        (writing-habit-helpers-project-code (cdr (cadr scored))))))
     (t (cons (cdr (car scored)) (cdr (car (car scored))))))))

(defun writing-habit-helpers--tidy (task)
  "Tidy TASK the way the Python port tidies it."
  (let* ((task (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " task)
                            "[ ,;:-]+" "[ ,;:-]+"))
         (task (string-trim-right task "\\.+")))
    (if (string-empty-p task)
        task
      (concat (upcase (substring task 0 1)) (substring task 1)))))

(defun writing-habit-helpers-capture-priority (text)
  "Return the priority TEXT states, as A or A1 or 2, or nil."
  (let ((value (let ((case-fold-search nil))
                 (and (string-match writing-habit-helpers--cookie-re text)
                      (match-string 1 text)))))
    (unless value
      (let ((case-fold-search t))
        (when (string-match writing-habit-helpers--priority-words-re text)
          (setq value (match-string 1 text)))))
    (when value
      (if (string-match-p "\\`[A-Za-z]" value) (upcase value) value))))

(defun writing-habit-helpers-capture-plain (text projects)
  "Read TEXT without a model, finding its project among PROJECTS."
  (let* ((found (writing-habit-helpers-capture-find-project text projects))
         (task text))
    (let ((case-fold-search nil))
      (setq task (replace-regexp-in-string writing-habit-helpers--cookie-re "" task t t)))
    (let ((case-fold-search t))
      (setq task (replace-regexp-in-string writing-habit-helpers--priority-words-re
                                           "" task t t)))
    (let ((case-fold-search nil))
      (when (string-match writing-habit-helpers--lead-code-re task)
        (setq task (substring task (match-end 0)))))
    (let ((case-fold-search t))
      (when (string-match writing-habit-helpers--openers-re task)
        (setq task (substring task (match-end 0)))))
    (writing-habit-helpers-captured-create
     :code (and (car found) (writing-habit-helpers-project-code (car found)))
     :task (writing-habit-helpers--tidy task)
     :cookie (writing-habit-helpers-capture-priority text)
     :how (cdr found))))

(defun writing-habit-helpers-capture-facts (text projects)
  "Write what a model may see, the PROJECTS and the captured TEXT."
  (string-join
   (append '("Projects:")
           (mapcar (lambda (p) (format "  %s: %s" (writing-habit-helpers-project-code p)
                                       (writing-habit-helpers-project-name p)))
                   projects)
           (list (concat "Captured words: " (string-join (split-string text) " "))))
   "\n"))

;;;; Project numbering help

(defconst writing-habit-helpers--numbering-common
  '("the" "and" "for" "with" "from" "into" "paper" "note" "notes"
    "draft" "new" "data" "project" "review" "talk" "letter" "email"
    "write" "read" "make" "find" "check" "update" "about" "this"
    "that" "send" "edit" "fix" "add" "tidy" "figure" "figures"
    "section" "table" "tables" "methods" "results" "intro" "first"
    "second" "third" "next" "last" "more" "reviewer" "reviewers"
    "article" "manuscript" "chapter")
  "Words too common in project names, or in tasks, to relate two projects.")

(defconst writing-habit-helpers-forming 3
  "Tasks that must share a word before a project counts as forming inside another.")

(defun writing-habit-helpers--upper-p (c)
  "Return non-nil when C is an ASCII capital letter."
  (and c (>= c ?A) (<= c ?Z)))

(defun writing-habit-helpers--lower-p (c)
  "Return non-nil when C is an ASCII small letter."
  (and c (>= c ?a) (<= c ?z)))

(defun writing-habit-helpers--digit-p (c)
  "Return non-nil when C is an ASCII digit."
  (and c (>= c ?0) (<= c ?9)))

(defun writing-habit-helpers--numbering-piece (s i)
  "Return the length of the piece of S beginning at I, or nil.
This follows the Python pattern
[A-Z]+\\d*(?=[A-Z][a-z]|\\b|_|\\d|$)|[A-Z]?[a-z]+\\d*|[A-Z]+\\d*|\\d+."
  (let* ((n (length s))
         (at (lambda (k) (and (< k n) (aref s k))))
         (run (lambda (k pred) (let ((j k)) (while (funcall pred (funcall at j))
                                                   (setq j (1+ j)))
                                     (- j k))))
         (ahead (lambda (p)
                  (let ((c (funcall at p)))
                    (or (null c)
                        (not (or (writing-habit-helpers--upper-p c)
                                 (writing-habit-helpers--lower-p c)
                                 (writing-habit-helpers--digit-p c) (= c ?_)))
                        (= c ?_)
                        (writing-habit-helpers--digit-p c)
                        (and (writing-habit-helpers--upper-p c)
                             (writing-habit-helpers--lower-p (funcall at (1+ p))))))))
         (upper (funcall run i #'writing-habit-helpers--upper-p)))
    (or
     ;; [A-Z]+\d* with the lookahead, trying the longest first.
     (when (> upper 0)
       (let ((digits (funcall run (+ i upper) #'writing-habit-helpers--digit-p))
             (found nil))
         (cl-loop for d from digits downto 0
                  when (funcall ahead (+ i upper d))
                  do (setq found (+ upper d)) and return nil)
         (unless found
           (cl-loop for k from (1- upper) downto 1
                    when (funcall ahead (+ i k))
                    do (setq found k) and return nil))
         found))
     ;; [A-Z]?[a-z]+\d*
     (let* ((lead (if (and (writing-habit-helpers--upper-p (funcall at i))
                           (writing-habit-helpers--lower-p (funcall at (1+ i))))
                      1 0))
            (lower (funcall run (+ i lead) #'writing-habit-helpers--lower-p)))
       (when (> lower 0)
         (+ lead lower (funcall run (+ i lead lower) #'writing-habit-helpers--digit-p))))
     ;; [A-Z]+\d*
     (when (> upper 0)
       (+ upper (funcall run (+ i upper) #'writing-habit-helpers--digit-p)))
     ;; \d+
     (let ((digits (funcall run i #'writing-habit-helpers--digit-p)))
       (when (> digits 0) digits)))))

(defun writing-habit-helpers-numbering-words (name)
  "Split project NAME into lowercase words.
RETKinaseCovalent gives ret, kinase, and covalent.  A piece of one or two
letters merges with the next, so PyMOL gives pymol as PYMOL does."
  (let* ((s (replace-regexp-in-string "\\`[ \t\n]*[0-9]\\{4\\}" "" (or name "")))
         (pieces '()) (i 0))
    (while (< i (length s))
      (let ((len (writing-habit-helpers--numbering-piece s i)))
        (if len
            (progn (push (downcase (substring s i (+ i len))) pieces)
                   (setq i (+ i len)))
          (setq i (1+ i)))))
    (setq pieces (nreverse pieces))
    (let ((merged '()))
      (while pieces
        (if (and (<= (length (car pieces)) 2) (cdr pieces))
            (progn (push (concat (car pieces) (cadr pieces)) merged)
                   (setq pieces (cddr pieces)))
          (push (car pieces) merged)
          (setq pieces (cdr pieces))))
      (nreverse merged))))

(defun writing-habit-helpers--stem (word)
  "Return the first five letters of WORD."
  (substring word 0 (min 5 (length word))))

(defun writing-habit-helpers-numbering-distinctive (name)
  "Return the words of NAME that can relate two projects."
  (seq-filter (lambda (w) (and (>= (length w) 3)
                               (not (member w writing-habit-helpers--numbering-common))
                               (not (string-match-p "\\`[0-9]+\\'" w))))
              (writing-habit-helpers-numbering-words name)))

(defun writing-habit-helpers-numbering-folder-projects (root)
  "Return an alist of (NUMBER . NAME) for the project folders under ROOT."
  (let ((out '()) (case-fold-search nil)
        (dir (expand-file-name root)))
    (when (file-directory-p dir)
      (dolist (child (sort (directory-files dir) #'string<))
        (when (and (string-match "\\`\\([0-9]\\{4\\}\\)\\([A-Za-z].*\\)\\'" child)
                   (file-directory-p (expand-file-name child dir))
                   (not (assoc (match-string 1 child) out)))
          (push (cons (match-string 1 child) child) out))))
    (nreverse out)))

(defun writing-habit-helpers-numbering-known-projects (&rest sources)
  "Return an alist of (NUMBER . NAME) from the project names in SOURCES."
  (let ((out '()))
    (dolist (source sources)
      (dolist (name source)
        (when (string-match "\\`[ \t\n]*\\([0-9]\\{4\\}\\)\\([^ \t\n]*\\)" (or name ""))
          (unless (assoc (match-string 1 name) out)
            (push (cons (match-string 1 name)
                        (concat (match-string 1 name) (match-string 2 name)))
                  out)))))
    (nreverse out)))

(cl-defstruct (writing-habit-helpers-suggestion
               (:constructor writing-habit-helpers-suggestion-create)
               (:copier nil))
  "Free NUMBERS for a new project NAME, best first.
RELATED is a list of (NUMBER NAME SHARED-WORDS) of the projects they sit near."
  name numbers related (reason ""))

(defun writing-habit-helpers-numbering-camel (name)
  "Write the working NAME as a project name, RET kinase PROTAC as RETKinasePROTAC."
  (mapconcat (lambda (p) (concat (upcase (substring p 0 1)) (substring p 1)))
             (writing-habit-helpers--words (or name "")) ""))

(defun writing-habit-helpers-suggestion-line (suggestion number)
  "Return the checkable line of SUGGESTION for NUMBER."
  (let ((near (mapconcat #'car (seq-take (writing-habit-helpers-suggestion-related
                                          suggestion)
                                         3)
                         ", ")))
    (format "Use %s%s%s" number
            (writing-habit-helpers-numbering-camel
             (writing-habit-helpers-suggestion-name suggestion))
            (if (string-empty-p near) "" (concat ", near " near)))))

(defun writing-habit-helpers--shares (word other tokens)
  "Return non-nil when WORD is in project name OTHER or its TOKENS."
  (let ((low (downcase other)))
    (or (member word tokens)
        (and (>= (length word) 4) (string-search word low))
        (string-prefix-p word (replace-regexp-in-string "\\`[0-9]\\{4\\}" "" low)))))

(defun writing-habit-helpers-numbering-related (name known &optional top)
  "Return (NUMBER NAME SHARED) of the projects in KNOWN most like NAME.
A shared word counts more the fewer projects hold it, and the first word
of NAME counts double.  Projects within three quarters of the best score
are returned, best first, at most TOP of them."
  (let ((mine (delete-dups (copy-sequence
                            (writing-habit-helpers-numbering-distinctive name))))
        (top (or top 6)))
    (when (and mine known)
      (let* ((tokens (mapcar (lambda (k) (cons (car k) (writing-habit-helpers-numbering-distinctive
                                                         (cdr k))))
                             known))
             (weight (mapcar
                      (lambda (w)
                        (let ((held (seq-count
                                     (lambda (k)
                                       (writing-habit-helpers--shares
                                        w (cdr k) (cdr (assoc (car k) tokens))))
                                     known)))
                          (cons w (log (/ (+ (length known) 1.0) (+ held 0.5))))))
                      mine))
             (scored '()))
        (setcdr (assoc (car mine) weight) (* 2 (cdr (assoc (car mine) weight))))
        (dolist (k known)
          (let ((shared (seq-filter (lambda (w) (writing-habit-helpers--shares
                                                 w (cdr k) (cdr (assoc (car k) tokens))))
                                    mine)))
            (when shared
              (push (list (apply #'+ (mapcar (lambda (w) (cdr (assoc w weight))) shared))
                          (car k) (cdr k) shared)
                    scored))))
        (when scored
          (setq scored (sort (nreverse scored)
                             (lambda (a b) (or (> (car a) (car b))
                                               (and (= (car a) (car b))
                                                    (string< (nth 1 a) (nth 1 b)))))))
          (let ((best (car (car scored))))
            (seq-take (mapcar #'cdr (seq-filter (lambda (s) (>= (car s) (* 0.75 best)))
                                                scored))
                      top)))))))

(defun writing-habit-helpers-numbering-suggest (name known &optional count active)
  "Suggest COUNT free numbers for a new project called NAME.
KNOWN is an alist of (NUMBER . NAME).  With related projects, the free
numbers inside their range come first, nearest the middle, then those
just above it, then just below.  Without any, the next free numbers after
the highest of ACTIVE, the numbers of this week's projects, or of KNOWN."
  (let* ((count (or count 3))
         (used (delete-dups (append (mapcar (lambda (k) (string-to-number (car k))) known)
                                    (mapcar #'string-to-number active))))
         (near (writing-habit-helpers-numbering-related name known))
         choices reason)
    (if near
        (let* ((numbers (sort (mapcar (lambda (r) (string-to-number (car r))) near) #'<))
               (low (car numbers)) (high (car (last numbers)))
               (middle (nth (/ (length numbers) 2) numbers))
               (inside (sort (seq-remove (lambda (n) (memq n used))
                                         (number-sequence low high))
                             (lambda (a b) (or (< (abs (- a middle)) (abs (- b middle)))
                                               (and (= (abs (- a middle)) (abs (- b middle)))
                                                    (< a b))))))
               (above (seq-remove (lambda (n) (memq n used))
                                  (number-sequence (1+ high) (1- (min (+ high 60) 10000)))))
               (below (seq-remove (lambda (n) (memq n used))
                                  (and (> (1- low) (max (- low 60) -1))
                                       (number-sequence (1- low) (1+ (max (- low 60) -1)) -1))))
               (shared (delete-dups (apply #'append (mapcar (lambda (r) (copy-sequence (nth 2 r)))
                                                            near)))))
          (setq choices (seq-take (append inside above below) count)
                reason (format "%d project%s %s with the new name" (length near)
                               (if (= (length near) 1) " shares" "s share")
                               (string-join shared ", "))))
      (let* ((pool (or (mapcar #'string-to-number active) (sort (copy-sequence used) #'<)))
             (start (if pool (1+ (apply #'max pool)) 1))
             (n start))
        (while (and (< (length choices) count) (< n 10000))
          (unless (memq n used) (push n choices))
          (setq n (1+ n)))
        (setq choices (nreverse choices)
              reason (if active
                         "no project shares a distinctive word with the new name, so it follows the highest number of this week's projects"
                       "no project shares a distinctive word with the new name"))))
    (writing-habit-helpers-suggestion-create
     :name name :numbers (mapcar (lambda (n) (format "%04d" n)) choices)
     :related near :reason reason)))

(cl-defstruct (writing-habit-helpers-forming
               (:constructor writing-habit-helpers-forming-create)
               (:copier nil))
  "A WORD several TASKS of project CODE share that its name PROJECT lacks."
  code project word tasks)

(defun writing-habit-helpers-forming-sentence (found)
  "Describe FOUND, a forming project."
  (format "%d tasks of %s %s mention “%s”, which its name does not: %s. It may be a project of its own."
          (length (writing-habit-helpers-forming-tasks found))
          (writing-habit-helpers-forming-code found)
          (writing-habit-helpers-forming-project found)
          (writing-habit-helpers-forming-word found)
          (mapconcat (lambda (t1) (format "“%s”" t1))
                     (seq-take (writing-habit-helpers-forming-tasks found) 3) "; ")))

(defun writing-habit-helpers-numbering-forming (tasks projects &optional least)
  "Find projects with LEAST or more tasks around a word the name lacks.
TASKS is an alist of (CODE . TEXTS) and PROJECTS of (CODE . NAME)."
  (let ((least (or least writing-habit-helpers-forming)) (out '()))
    (dolist (pair tasks)
      (let* ((code (car pair))
             (name (or (cdr (assoc code projects)) ""))
             (own (mapcar #'writing-habit-helpers--stem
                          (writing-habit-helpers-numbering-words name)))
             (counts '()))
        (dolist (text (delete-dups (copy-sequence (cdr pair))))
          (let ((seen '()))
            (dolist (w (writing-habit-helpers--all-matches
                        "[A-Za-z][A-Za-z0-9]\\{3,\\}" text))
              (let ((key (writing-habit-helpers--stem (downcase w))))
                (unless (or (member (downcase w) writing-habit-helpers--numbering-common)
                            (member key own) (member key seen))
                  (push key seen)
                  (let ((cell (assoc (downcase w) counts)))
                    (if cell
                        (setcdr cell (append (cdr cell) (list text)))
                      (setq counts (append counts (list (list (downcase w) text)))))))))))
        (let ((best (sort (seq-filter (lambda (c) (>= (length (cdr c)) least)) counts)
                          (lambda (a b) (or (> (length (cdr a)) (length (cdr b)))
                                            (and (= (length (cdr a)) (length (cdr b)))
                                                 (string< (car a) (car b))))))))
          (when best
            (push (writing-habit-helpers-forming-create
                   :code code :project name :word (car (car best)) :tasks (cdr (car best)))
                  out)))))
    (nreverse out)))

(defun writing-habit-helpers-numbering-facts (suggestion found)
  "Write SUGGESTION and the FOUND forming projects as facts for the model."
  (string-join
   (append
    (list (format "New project: %s" (writing-habit-helpers-suggestion-name suggestion))
          (format "Suggested numbers, best first: %s"
                  (string-join (writing-habit-helpers-suggestion-numbers suggestion) ", "))
          (format "Why: %s" (writing-habit-helpers-suggestion-reason suggestion)))
    (mapcar (lambda (r) (format "Related project: %s %s, sharing %s"
                                (nth 0 r) (nth 1 r) (string-join (nth 2 r) ", ")))
            (writing-habit-helpers-suggestion-related suggestion))
    (mapcar (lambda (f) (concat "Possible project forming: "
                                (writing-habit-helpers-forming-sentence f)))
            found))
   "\n"))

(defun writing-habit-helpers-numbering-plain (suggestion found)
  "Describe SUGGESTION and the FOUND forming projects without a model."
  (let ((out '())
        (numbers (writing-habit-helpers-suggestion-numbers suggestion))
        (reason (writing-habit-helpers-suggestion-reason suggestion)))
    (cond
     ((writing-habit-helpers-suggestion-related suggestion)
      (push (format "%s is free and sits with the related projects %s, because %s."
                    (car numbers)
                    (mapconcat (lambda (r) (nth 1 r))
                               (writing-habit-helpers-suggestion-related suggestion) ", ")
                    reason)
            out))
     (numbers
      (push (format "%s is the next free number, because %s." (car numbers) reason) out)))
    (string-join (append (nreverse out)
                         (mapcar #'writing-habit-helpers-forming-sentence found))
                 "\n\n")))

;;;; Roll-forward triage

(defconst writing-habit-helpers-recent-weeks 4
  "The weeks counted as recent.")

(defconst writing-habit-helpers-active-minutes 60
  "The minutes in the recent weeks that keep a project alive.")

(cl-defstruct (writing-habit-helpers-decision
               (:constructor writing-habit-helpers-decision-create)
               (:copier nil))
  "A suggested ACTION, keep, defer, or drop, for one carried PROJECT.
RECENT is the minutes of the last weeks, TOTAL those of the whole history,
LAST-WEEK the Monday of the last week worked, and TARGET the title of the
list a deferred project goes to."
  project action (reason "") deadline (recent 0) (total 0) last-week (target ""))

(defun writing-habit-helpers--verb (action)
  "Return the capitalized verb of ACTION."
  (pcase action ("keep" "Keep") ("defer" "Defer") ("drop" "Drop")))

(defun writing-habit-helpers-decision-line (decision)
  "Return the checkable line of DECISION."
  (format "%s %s%s"
          (writing-habit-helpers--verb (writing-habit-helpers-decision-action decision))
          (writing-habit-helpers-decision-project decision)
          (pcase (writing-habit-helpers-decision-action decision)
            ("keep" " on the current list")
            ("defer" (format " to %s"
                             (let ((target (writing-habit-helpers-decision-target decision)))
                               (if (string-empty-p (or target "")) "the next period" target))))
            ("drop" " from the lists"))))

(defun writing-habit-helpers--triage-key (project)
  "Name PROJECT by its four-digit number when it has one, else by its name."
  (or (writing-habit-helpers--project-number project) (string-trim (or project ""))))

(defun writing-habit-helpers-triage-minutes-by-project (db names-by-code since)
  "Return an alist of (KEY . ((WEEK . MINUTES) ...)) from DB since SINCE.
A session is filed under a legend code, and NAMES-BY-CODE, an alist of
\(CODE . PROJECT), resolves each code first.  Otherwise the description
the database holds for the code names the project."
  (let ((out '()))
    (dolist (row (sqlite-select
                  db (concat "SELECT s.week_start, p.code, p.description, SUM(s.actual_min) "
                             "FROM session s JOIN project p ON p.project_id = s.project_id "
                             "WHERE s.day >= ? GROUP BY s.week_start, p.code")
                  (list since)))
      (let* ((week (nth 0 row)) (code (nth 1 row))
             (name (or (cdr (assoc code names-by-code))
                       (car (writing-habit-name-split-deadline (or (nth 2 row) ""))))))
        (unless (or (null name) (string-empty-p name))
          (let* ((key (writing-habit-helpers--triage-key name))
                 (weeks (or (assoc key out) (car (push (list key) out))))
                 (cell (assoc week (cdr weeks))))
            (if cell
                (setcdr cell (+ (cdr cell) (or (nth 3 row) 0)))
              (setcdr weeks (append (cdr weeks) (list (cons week (or (nth 3 row) 0))))))))))
    (nreverse out)))

(defun writing-habit-helpers-triage-decide (project deadline weeks today period-end
                                                    &optional target)
  "Suggest keep, defer, or drop for PROJECT due DEADLINE.
WEEKS is an alist of (MONDAY . MINUTES), TODAY and PERIOD-END ISO dates,
and TARGET the title of the list a deferred project goes to."
  (let* ((today-abs (writing-habit-helpers--abs today))
         (monday (- today-abs (mod (+ (calendar-day-of-week
                                       (calendar-gregorian-from-absolute today-abs))
                                      6)
                                   7)))
         (cutoff (writing-habit-helpers--iso (- monday (* 7 writing-habit-helpers-recent-weeks))))
         (recent (apply #'+ (mapcar #'cdr (seq-filter (lambda (w) (not (string< (car w) cutoff)))
                                                      weeks))))
         (total (apply #'+ (mapcar #'cdr weeks)))
         (worked (sort (mapcar #'car (seq-filter (lambda (w) (> (cdr w) 0)) weeks)) #'string<))
         (last (car (last worked)))
         (d (writing-habit-helpers-decision-create
             :project project :action "keep" :deadline deadline :recent recent
             :total total :last-week last :target (or target ""))))
    (cond
     ((>= recent writing-habit-helpers-active-minutes)
      (setf (writing-habit-helpers-decision-reason d)
            (format "%d minutes were logged for it in the last %d weeks, so it is alive"
                    recent writing-habit-helpers-recent-weeks)))
     ((and deadline (not (string< deadline today)) (not (string< period-end deadline)))
      (setf (writing-habit-helpers-decision-reason d)
            (format "its deadline, %s, falls in this period" deadline)))
     ((and deadline (string< deadline today))
      (setf (writing-habit-helpers-decision-action d) "drop"
            (writing-habit-helpers-decision-reason d)
            (format "its deadline, %s, has passed with little time logged lately. Renegotiate the date instead if it still matters" deadline)))
     (deadline
      (setf (writing-habit-helpers-decision-action d) "defer"
            (writing-habit-helpers-decision-reason d)
            (format "its deadline, %s, falls after this period" deadline)))
     ((> total 0)
      (setf (writing-habit-helpers-decision-action d) "defer"
            (writing-habit-helpers-decision-reason d)
            (format "it was last worked the week of %s, and no deadline presses" last)))
     (t
      (setf (writing-habit-helpers-decision-action d) "drop"
            (writing-habit-helpers-decision-reason d)
            "no time was logged for it this period, and it has no deadline")))
    d))

(defun writing-habit-helpers-triage (entries history today period-end &optional target)
  "Suggest a decision for each carried list entry of ENTRIES, in list order.
HISTORY comes from `writing-habit-helpers-triage-minutes-by-project'.
TODAY and PERIOD-END are ISO dates, and TARGET names the list a
deferred project goes to."
  (mapcar (lambda (entry)
            (let* ((split (writing-habit-name-split-deadline
                           (writing-habit-lists-entry-description entry))))
              (writing-habit-helpers-triage-decide
               (car split) (cdr split)
               (cdr (assoc (writing-habit-helpers--triage-key (car split)) history))
               today period-end target)))
          entries))

(defun writing-habit-helpers-triage-facts (found period)
  "Write the decisions FOUND for PERIOD as the facts the model receives."
  (string-join
   (cons (format "New period: %s" period)
         (mapcar (lambda (d)
                   (format "Carried project %s: suggested %s, because %s. Minutes in the last %d weeks: %d. Last week worked: %s."
                           (writing-habit-helpers-decision-project d)
                           (writing-habit-helpers-decision-action d)
                           (writing-habit-helpers-decision-reason d)
                           writing-habit-helpers-recent-weeks
                           (writing-habit-helpers-decision-recent d)
                           (or (writing-habit-helpers-decision-last-week d) "none")))
                 found))
   "\n"))

(defun writing-habit-helpers-triage-plain (found)
  "Describe the decisions FOUND without a model."
  (if (null found)
      "Nothing is on the Carried over list, so nothing needs a decision."
    (let ((count (lambda (a) (seq-count (lambda (d) (equal (writing-habit-helpers-decision-action d) a))
                                        found))))
      (string-join
       (cons (format "%d carried project%s: keep %d, defer %d, drop %d. Dropping takes a project off the lists only. Its folder and history stay."
                     (length found) (if (= (length found) 1) "" "s")
                     (funcall count "keep") (funcall count "defer") (funcall count "drop"))
             (mapcar (lambda (d) (format "  %s %s, because %s."
                                         (writing-habit-helpers--verb
                                          (writing-habit-helpers-decision-action d))
                                         (writing-habit-helpers-decision-project d)
                                         (writing-habit-helpers-decision-reason d)))
                     found))
       "\n"))))

;;;; Stuck projects

(defconst writing-habit-helpers-stuck-recent 3
  "How many of a project's most recent blocks with objectives are read.")

(defconst writing-habit-helpers-stuck-low-share (/ 1.0 3)
  "Met objectives at or below this share of those committed signal a stuck project.")

(cl-defstruct (writing-habit-helpers-stuck
               (:constructor writing-habit-helpers-stuck-create)
               (:copier nil))
  "One project that shows a sign of being stuck.
KIND is repeated, low, or same_step, and TASK the work to break down."
  code project kind task blocks (times 0) (met 0) (committed 0))

(defun writing-habit-helpers-stuck-sentence (stuck)
  "Describe plainly the sign STUCK records."
  (let ((who (string-trim (format "%s %s" (writing-habit-helpers-stuck-code stuck)
                                  (writing-habit-helpers-stuck-project stuck)))))
    (pcase (writing-habit-helpers-stuck-kind stuck)
      ("repeated" (format "“%s” has gone unmet in %d of the last %d blocks of %s."
                          (writing-habit-helpers-stuck-task stuck)
                          (writing-habit-helpers-stuck-times stuck)
                          (writing-habit-helpers-stuck-blocks stuck) who))
      ("low" (format "%s met %d of %d objectives over its last %d blocks."
                     who (writing-habit-helpers-stuck-met stuck)
                     (writing-habit-helpers-stuck-committed stuck)
                     (writing-habit-helpers-stuck-blocks stuck)))
      (_ (format "The next step for %s has read “%s” in its last %d reviews."
                 who (writing-habit-helpers-stuck-task stuck)
                 (writing-habit-helpers-stuck-times stuck))))))

(defun writing-habit-helpers-stuck-advice (_stuck)
  "Return the advice for a stuck project."
  "Break it into smaller objectives that can each be finished in one block. Press Suggest to have the coach draft them.")

(defun writing-habit-helpers-stuck-normalize (text)
  "Return TEXT lowercased, with single spaces and only letters and digits."
  (string-trim
   (replace-regexp-in-string
    "[^a-z0-9 ]" ""
    (replace-regexp-in-string "[ \t\n\r\f\v]+" " " (downcase (or text ""))))))

(defun writing-habit-helpers--before-p (day start before)
  "Return non-nil when DAY and START come before BEFORE, a (DAY . START) pair."
  (or (null before)
      (string< day (car before))
      (and (string= day (car before)) (string< start (cdr before)))))

(defun writing-habit-helpers-stuck-check (db code &optional before today)
  "Return the first sign in DB that project CODE is stuck, or nil.
Only blocks that start before BEFORE, a (DAY . START) pair, and on or
before TODAY are read, so a block does not judge itself."
  (when (writing-habit-helpers--table-p db "block_objective")
    (let* ((row (car (sqlite-select db "SELECT description FROM project WHERE code = ?"
                                    (list code))))
           (project (or (car row) ""))
           (blocks '()))
      (dolist (r (sqlite-select
                  db (concat "SELECT day, start_time, text, done FROM block_objective "
                             "WHERE code = ? ORDER BY day, start_time, position, objective_id")
                  (list code)))
        (let ((day (nth 0 r)) (start (nth 1 r)))
          (when (and (writing-habit-helpers--before-p day start before)
                     (or (null today) (not (string< today day))))
            (let ((cell (assoc (cons day start) blocks)))
              (if cell
                  (setcdr cell (append (cdr cell) (list (cons (nth 2 r) (nth 3 r)))))
                (setq blocks (append blocks (list (list (cons day start)
                                                        (cons (nth 2 r) (nth 3 r)))))))))))
      (let* ((recent (last blocks writing-habit-helpers-stuck-recent))
             (found nil))
        (when recent
          (let ((unmet '()))
            (dolist (b recent)
              (dolist (o (cdr b))
                (when (zerop (cdr o))
                  (let* ((key (writing-habit-helpers-stuck-normalize (car o)))
                         (cell (assoc key unmet)))
                    (if cell
                        (setcdr cell (append (cdr cell) (list (car o))))
                      (setq unmet (append unmet (list (list key (car o))))))))))
            (let ((repeated (mapcar (lambda (u) (cons (length (cdr u)) (car (last (cdr u)))))
                                    (seq-filter (lambda (u) (>= (length (cdr u)) 2)) unmet))))
              (if repeated
                  (let ((top (car (sort repeated
                                        (lambda (a b) (or (> (car a) (car b))
                                                          (and (= (car a) (car b))
                                                               (string< (cdr b) (cdr a)))))))))
                    (setq found (writing-habit-helpers-stuck-create
                                 :code code :project project :kind "repeated"
                                 :task (cdr top) :blocks (length recent) :times (car top))))
                (let ((committed (apply #'+ (mapcar (lambda (b) (length (cdr b))) recent)))
                      (met (apply #'+ (mapcar (lambda (b) (apply #'+ (mapcar #'cdr (cdr b))))
                                              recent))))
                  (when (and (>= (length recent) writing-habit-helpers-stuck-recent)
                             (<= met (* writing-habit-helpers-stuck-low-share committed)))
                    (let ((last-unmet
                           (or (cl-loop for b in (reverse recent)
                                        thereis (car (seq-find (lambda (o) (zerop (cdr o)))
                                                               (cdr b))))
                               (car (cadr (car (last recent)))))))
                      (setq found (writing-habit-helpers-stuck-create
                                   :code code :project project :kind "low"
                                   :task last-unmet :blocks (length recent)
                                   :met met :committed committed)))))))))
        (or found
            (when (writing-habit-helpers--table-p db "session_review")
              (let ((steps (seq-filter
                            (lambda (r) (and (writing-habit-helpers--before-p
                                              (nth 0 r) (nth 1 r) before)
                                             (or (null today)
                                                 (not (string< today (nth 0 r))))))
                            (sqlite-select
                             db (concat "SELECT day, start_time, next_step FROM session_review "
                                        "WHERE code = ? AND next_step IS NOT NULL "
                                        "AND TRIM(next_step) != '' ORDER BY day, start_time")
                             (list code)))))
                (when (and (>= (length steps) 2)
                           (string= (writing-habit-helpers-stuck-normalize
                                     (nth 2 (car (last steps))))
                                    (writing-habit-helpers-stuck-normalize
                                     (nth 2 (car (last steps 2))))))
                  (writing-habit-helpers-stuck-create
                   :code code :project project :kind "same_step"
                   :task (string-trim (nth 2 (car (last steps))))
                   :blocks (length recent) :times 2)))))))))

(defun writing-habit-helpers-stuck-find (db codes &optional today)
  "Return the stuck projects in DB among CODES, in the order given.
Only blocks on or before TODAY are read."
  (delq nil (mapcar (lambda (code) (writing-habit-helpers-stuck-check db code nil today))
                    codes)))

(defun writing-habit-helpers-stuck-report (found)
  "Describe the stuck projects FOUND, or say there are none."
  (if (null found)
      "No project shows a sign of being stuck. Every recent block met enough of its objectives, and no next step repeats."
    (string-join
     (append (mapcar #'writing-habit-helpers-stuck-sentence found)
             (list ""
                   "Open the next block of each project in the hour view. Its review panel puts the stuck task in the objective box, and Suggest breaks it into smaller objectives."))
     "\n")))

;;;; Commands

(defun writing-habit-helpers--db-path ()
  "Return the habit database file, asking when none is known."
  (or (and (boundp 'writing-habit-dispatch--last-db) writing-habit-dispatch--last-db)
      (and (boundp 'writing-habit-default-db) writing-habit-default-db)
      (read-file-name "Habit database: " nil nil t)))

(defun writing-habit-helpers--legend (model)
  "Return the legend of MODEL as an alist of (CODE . DESCRIPTION)."
  (mapcar (lambda (row) (cons (nth 0 row) (nth 1 row)))
          (writing-habit-table-legend model)))

(defun writing-habit-helpers--list-names ()
  "Return the names of the project lists, without the completed list."
  (append (seq-remove (lambda (n) (equal n writing-habit-lists-completed))
                      (mapcar #'car writing-habit-lists))
          '("carried-over-projects")))

;;;###autoload
(defun writing-habit-helpers-capture (text)
  "File TEXT, a thought in plain words, on the grasscatcher of this table.
The project is found by a lookup of its code, number, or name among the
legend and the lists, and a priority is read only from a cookie or from
words such as priority A.  This port keeps the plain reading only and
asks no model."
  (interactive (list (read-string "Capture: ")))
  (let* ((model (writing-habit-table-current))
         (after (writing-habit-table-after model))
         (lists (apply #'append
                       (mapcar (lambda (name)
                                 (mapcar (lambda (e) (cons (writing-habit-lists-entry-code e)
                                                           (writing-habit-lists-entry-description e)))
                                         (writing-habit-lists-entries after name)))
                               (writing-habit-helpers--list-names))))
         (projects (writing-habit-helpers-capture-projects-from
                    (writing-habit-helpers--legend model) lists))
         (got (writing-habit-helpers-capture-plain text projects))
         (code (writing-habit-helpers-captured-code got))
         (project (and code (seq-find (lambda (p) (equal (writing-habit-helpers-project-code p) code))
                                      projects)))
         (number (and project (writing-habit-helpers--project-number
                               (writing-habit-helpers-project-name project))))
         (entry (writing-habit-lists-entry-create
                 :code code
                 :description (concat (if (and code number) (concat number " ") "")
                                      (writing-habit-helpers-captured-entry-text got)))))
    (unless (writing-habit-lists--span after "grasscatcher")
      (setq after (append after (list "" "#+NAME: grasscatcher"))))
    (writing-habit-table--set-after
     model (car (writing-habit-lists-insert after "grasscatcher" nil entry)))
    (writing-habit-table--apply model)
    (message "Captured %s" (writing-habit-lists-entry-text entry))
    entry))

;;;###autoload
(defun writing-habit-helpers-number-project (name)
  "Suggest a free number for the new project NAME and offer to list it.
The numbers in use come from the folders under
`writing-habit-helpers-projects-root' and from the legend and the lists.
Nothing is renamed or moved."
  (interactive (list (read-string "Working name of the new project: ")))
  (let* ((model (writing-habit-table-current))
         (after (writing-habit-table-after model))
         (legend (mapcar (lambda (p) (cons (car p) (car (writing-habit-table-split-due-date
                                                         (cdr p)))))
                         (writing-habit-helpers--legend model)))
         (listed (apply #'append
                        (mapcar (lambda (n) (mapcar #'writing-habit-lists-entry-project
                                                    (writing-habit-lists-entries after n)))
                                (mapcar #'car writing-habit-lists))))
         (known (writing-habit-helpers-numbering-folder-projects
                 writing-habit-helpers-projects-root))
         (active (delq nil (mapcar #'writing-habit-helpers--project-number
                                   (append (mapcar #'cdr legend)
                                           (mapcar #'writing-habit-lists-entry-project
                                                   (writing-habit-lists-entries
                                                    after writing-habit-lists-current))))))
         (tasks '()))
    (dolist (k (writing-habit-helpers-numbering-known-projects (mapcar #'cdr legend) listed))
      (unless (assoc (car k) known) (setq known (append known (list k)))))
    (dolist (e (writing-habit-lists-entries after "grasscatcher"))
      (let ((code (writing-habit-lists-entry-code e)))
        (when code
          (let ((cell (assoc code tasks)))
            (if cell
                (setcdr cell (append (cdr cell) (list (writing-habit-lists-entry-description e))))
              (setq tasks (append tasks (list (list code (writing-habit-lists-entry-description e))))))))))
    (let* ((suggestion (writing-habit-helpers-numbering-suggest name known 3 active))
           (found (writing-habit-helpers-numbering-forming tasks legend))
           (note (writing-habit-coach-note
                  "numbering" (writing-habit-helpers-numbering-facts suggestion found)
                  (writing-habit-helpers-numbering-plain suggestion found)))
           (chosen (funcall writing-habit-coach-chooser "Number for a new project"
                            (plist-get note :text)
                            (mapcar (lambda (n) (writing-habit-helpers-suggestion-line suggestion n))
                                    (writing-habit-helpers-suggestion-numbers suggestion)))))
      (when chosen
        (let ((project (concat (nth (car chosen) (writing-habit-helpers-suggestion-numbers suggestion))
                               (writing-habit-helpers-numbering-camel name))))
          (writing-habit-table--set-after
           model (car (writing-habit-lists-insert
                       after writing-habit-lists-current nil
                       (writing-habit-lists-entry-create :description project))))
          (writing-habit-table--apply model)
          (message "Added %s to the current semester list" project)))
      suggestion)))

;;;###autoload
(defun writing-habit-helpers-triage-carried ()
  "Suggest keeping, deferring, or dropping each carried-over project.
The suggestions come from the minutes logged in the habit database, the
last week worked, and the deadline.  Only the accepted ones are applied,
and dropping takes a project off the lists only."
  (interactive)
  (let* ((model (writing-habit-table-current))
         (after (writing-habit-table-after model))
         (carried (writing-habit-lists-entries after "carried-over-projects"))
         (today (writing-habit-helpers--today))
         (end (or writing-habit-helpers-semester-end
                  (writing-habit-helpers--iso (+ (writing-habit-helpers--abs today) (* 16 7)))))
         (history nil))
    (when carried
      (let ((names (delq nil (mapcar (lambda (e) (when (writing-habit-lists-entry-code e)
                                                   (cons (writing-habit-lists-entry-code e)
                                                         (writing-habit-lists-entry-project e))))
                                     carried)))
            (db (writing-habit-db-connect (writing-habit-helpers--db-path))))
        (dolist (p (writing-habit-helpers--legend model))
          (unless (assoc (car p) names)
            (setq names (append names (list (cons (car p) (car (writing-habit-table-split-due-date
                                                                (cdr p)))))))))
        (unwind-protect
            (setq history (writing-habit-helpers-triage-minutes-by-project
                           db names (writing-habit-helpers--iso
                                     (- (writing-habit-helpers--abs today) (* 26 7)))))
          (writing-habit-db-close db))))
    (let* ((found (writing-habit-helpers-triage carried history today end
                                                "Next semester projects"))
           (plain (writing-habit-helpers-triage-plain found)))
      (if (null found)
          (message "%s" plain)
        (let* ((note (writing-habit-coach-note
                      "triage" (writing-habit-helpers-triage-facts found "this period") plain))
               (chosen (funcall writing-habit-coach-chooser "Carried-over projects"
                                (plist-get note :text)
                                (mapcar #'writing-habit-helpers-decision-line found))))
          (dolist (i chosen)
            (let* ((d (nth i found))
                   (lines (writing-habit-table-after model))
                   (at (writing-habit-lists-find-project
                        lines "carried-over-projects" (writing-habit-helpers-decision-project d))))
              (when at
                (writing-habit-table--set-after
                 model
                 (car (pcase (writing-habit-helpers-decision-action d)
                        ("drop" (writing-habit-lists-remove lines "carried-over-projects" at))
                        ("keep" (writing-habit-lists-transfer lines "carried-over-projects" at
                                                              writing-habit-lists-current))
                        (_ (writing-habit-lists-transfer lines "carried-over-projects" at
                                                         "next-semester-projects")))))
                (message "%s" (writing-habit-helpers-decision-line d)))))
          (when chosen (writing-habit-table--apply model))))
      found)))

;;;###autoload
(defun writing-habit-helpers-find-stuck ()
  "Report the projects of this table that show a sign of being stuck."
  (interactive)
  (let* ((model (writing-habit-table-current))
         (codes (mapcar #'car (writing-habit-helpers--legend model)))
         (db (writing-habit-db-connect (writing-habit-helpers--db-path)))
         (found (unwind-protect
                    (writing-habit-helpers-stuck-find db codes (writing-habit-helpers--today))
                  (writing-habit-db-close db))))
    (with-current-buffer (get-buffer-create "*writing-habit stuck*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (writing-habit-helpers-stuck-report found) "\n"))
      (special-mode)
      (display-buffer (current-buffer)))
    found))

(provide 'writing-habit-helpers)
;;; writing-habit-helpers.el ends here
