;;; writing-habit-report.el --- Render the weekly comparison as an org buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT license.

;;; Commentary:

;; This is part of Phase 5 of the Emacs Lisp port of the Python
;; writing-habit package.  It is the analogue of compare/report.py's
;; render_week.  Where the Python report writes plain text to standard
;; output, this one writes an org document, so the writer can fold it,
;; export it, or paste it into a log with no conversion.  It reads the same
;; four views and the streak through writing-habit-compare, so the numbers
;; are exactly the ones the Python report shows.
;;
;; Public functions:
;;   `writing-habit-report-week-string'  return the org report as a string
;;   `writing-habit-report-week'         show the report in a buffer

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'writing-habit-compare)

(declare-function org-mode "org" ())
(declare-function org-read-date "org" (&rest args))


;;;; Formatting helpers

(defun writing-habit-report--fmt (value)
  "Format an adherence VALUE, or \"n/a\" when it is nil."
  (if (null value) "n/a" (format "%.2f" value)))

(defun writing-habit-report--ratio (actual planned)
  "Format ACTUAL over PLANNED to two places, or \"n/a\" when PLANNED is zero."
  (if (and planned (> planned 0))
      (format "%.2f" (/ (float actual) planned))
    "n/a"))

(defun writing-habit-report--pct (part whole)
  "Format PART as a whole-number percent of WHOLE, or \"n/a\" when WHOLE is zero."
  (if (and whole (> whole 0))
      (format "%d%%" (round (/ (* 100.0 part) whole)))
    "n/a"))

(defun writing-habit-report--table (header rows)
  "Return an org table string for HEADER and ROWS, each a list of strings.
Columns are padded so the table reads well as plain text and re-aligns in
org-mode."
  (let* ((ncol (length header))
         (widths (make-vector ncol 0)))
    (dolist (r (cons header rows))
      (dotimes (i ncol)
        (aset widths i (max (aref widths i) (length (or (nth i r) ""))))))
    (cl-flet ((fmt-row (cells)
                (concat "| "
                        (mapconcat
                         (lambda (i)
                           (let ((c (or (nth i cells) "")))
                             (concat c (make-string (- (aref widths i) (length c)) ?\s))))
                         (number-sequence 0 (1- ncol)) " | ")
                        " |")))
      (concat (fmt-row header) "\n"
              "|" (mapconcat (lambda (i) (make-string (+ 2 (aref widths i)) ?-))
                             (number-sequence 0 (1- ncol)) "+") "|\n"
              (mapconcat #'fmt-row rows "\n") "\n"))))

(defun writing-habit-report--num (row key)
  "Return the integer in ROW under KEY as a string."
  (number-to-string (cdr (assoc key row))))


;;;; Optional planned-versus-actual plot
;;
;; The Python report offers an optional matplotlib bar chart behind the
;; [plot] extra.  This port defines that same chart once, as a Python
;; script, and offers it two ways, each suited to how the report is used.
;; For the interactive org report the chart is embedded as an Org Babel
;; python source block, so C-c C-c on the block, or an export, renders it in
;; place with no work at render time.  For headless and batch use
;; `writing-habit-report-write-plot' runs the same script through Python in
;; Agg mode and writes the PNG directly, which is what the compare --plot
;; command uses.  Either way needs python3 with matplotlib.

(defcustom writing-habit-report-python "python3"
  "Python interpreter used to render the optional matplotlib plot."
  :type 'string
  :group 'writing-habit)

(defun writing-habit-report--py-strings (items)
  "Render ITEMS, a list of strings, as a Python list literal."
  (concat "[" (mapconcat (lambda (s) (format "%S" s)) items ", ") "]"))

(defun writing-habit-report--py-ints (items)
  "Render ITEMS, a list of integers, as a Python list literal."
  (concat "[" (mapconcat #'number-to-string items ", ") "]"))

(defun writing-habit-report--plot-python (proj week out-path)
  "Return a matplotlib script that plots PROJ for WEEK and saves it to OUT-PATH.
This mirrors compare/report.py's write_plot: a grouped bar chart of planned
against actual minutes per project, in Agg mode so it needs no display."
  (let ((codes (mapcar (lambda (r) (cdr (assoc "code" r))) proj))
        (planned (mapcar (lambda (r) (cdr (assoc "planned_min" r))) proj))
        (actual (mapcar (lambda (r) (cdr (assoc "actual_min" r))) proj)))
    (concat
     "import matplotlib\n"
     "matplotlib.use(\"Agg\")\n"
     "import matplotlib.pyplot as plt\n"
     "codes = " (writing-habit-report--py-strings codes) "\n"
     "planned = " (writing-habit-report--py-ints planned) "\n"
     "actual = " (writing-habit-report--py-ints actual) "\n"
     "x = range(len(codes))\n"
     "fig, ax = plt.subplots(figsize=(7, 4))\n"
     "ax.bar([i - 0.2 for i in x], planned, width=0.4, label=\"planned\")\n"
     "ax.bar([i + 0.2 for i in x], actual, width=0.4, label=\"actual\")\n"
     "ax.set_xticks(list(x))\n"
     "ax.set_xticklabels(codes)\n"
     "ax.set_ylabel(\"minutes\")\n"
     (format "ax.set_title(%S)\n" (format "Planned vs actual, week of %s" week))
     "ax.legend()\n"
     "fig.tight_layout()\n"
     (format "fig.savefig(%S, dpi=120)\n" out-path)
     "plt.close(fig)\n")))

(defun writing-habit-report--plot-src-block (proj week out-path)
  "Return an Org Babel python source block that renders the plot to OUT-PATH."
  (concat
   "#+CAPTION: Planned versus actual minutes per project.\n"
   (format "#+begin_src python :results file graphics :file %s :exports results\n"
           out-path)
   (writing-habit-report--plot-python proj week out-path)
   "#+end_src\n"))

(defun writing-habit-report-matplotlib-available-p ()
  "Return non-nil when the configured Python can import matplotlib."
  (and (executable-find writing-habit-report-python)
       (eq 0 (call-process writing-habit-report-python nil nil nil
                           "-c" "import matplotlib"))))

(defun writing-habit-report-write-plot (db week out-path)
  "Write a planned-versus-actual bar chart for WEEK from DB to OUT-PATH.
Run the configured Python with matplotlib in Agg mode as a subprocess, so
it needs python3 with matplotlib.  Return OUT-PATH, or signal an error when
Python fails."
  (let* ((proj (writing-habit-compare-week-project db week))
         (script (writing-habit-report--plot-python proj week out-path))
         (tmp (make-temp-file "wh-plot" nil ".py" script)))
    (unwind-protect
        (with-temp-buffer
          (let ((status (call-process writing-habit-report-python nil t nil tmp)))
            (unless (eq status 0)
              (error "Plot generation failed: %s" (string-trim (buffer-string))))))
      (delete-file tmp))
    out-path))


;;;; Rendering

(defun writing-habit-report-week-string (db week &optional plot-file)
  "Return an org-mode report of planned versus actual effort for WEEK from DB.
When PLOT-FILE is non-nil, append a section with an Org Babel python source
block that renders a planned-versus-actual bar chart to PLOT-FILE.  The
block is not run at render time; C-c C-c on it, or an export, produces the
figure."
  (let* ((proj (writing-habit-compare-week-project db week))
         (week-start (if proj (cdr (assoc "week_start" (car proj)))
                       (writing-habit-compare--monday week)))
         (planned-total (apply #'+ (mapcar (lambda (r) (cdr (assoc "planned_min" r))) proj)))
         (actual-total (apply #'+ (mapcar (lambda (r) (cdr (assoc "actual_min" r))) proj)))
         (out '()))
    (push (format "#+TITLE: Writing week beginning %s" week-start) out)
    (push "#+LaTeX_HEADER: \\usepackage[margin=0.5in]{geometry}" out)
    (push "#+LaTeX_HEADER: \\usepackage{booktabs}" out)
    (push "" out)
    (push (format "Planned %d min, actual %d min, overall adherence %s."
                  planned-total actual-total
                  (writing-habit-report--ratio actual-total planned-total))
          out)
    (when proj
      (push "" out)
      (push "* By project" out)
      (push "#+CAPTION: Planned versus actual minutes and adherence per project." out)
      (push "#+ATTR_LATEX: :booktabs t" out)
      (push (writing-habit-report--table
             '("Code" "Description" "Planned" "Actual" "Diff" "Adherence")
             (mapcar (lambda (r)
                       (list (cdr (assoc "code" r))
                             (or (cdr (assoc "description" r)) "")
                             (writing-habit-report--num r "planned_min")
                             (writing-habit-report--num r "actual_min")
                             (writing-habit-report--num r "diff_min")
                             (writing-habit-report--fmt (cdr (assoc "adherence" r)))))
                     proj))
            out))
    (let ((cat (writing-habit-compare-week-category db week)))
      (when cat
        (push "" out)
        (push "* By activity" out)
        (push "#+CAPTION: Planned versus actual minutes per activity, the Rule 2 balance." out)
        (push "#+ATTR_LATEX: :booktabs t" out)
        (push (writing-habit-report--table
               '("Activity" "Planned" "Actual")
               (mapcar (lambda (r)
                         (list (cdr (assoc "category" r))
                               (writing-habit-report--num r "planned_min")
                               (writing-habit-report--num r "actual_min")))
                       cat))
              out)))
    (let ((bar (writing-habit-compare-week-barbell db week)))
      (when bar
        (cl-flet ((sum-for (risk field)
                    (apply #'+ (mapcar (lambda (r) (cdr (assoc field r)))
                                       (seq-filter
                                        (lambda (r) (equal (cdr (assoc "risk_class" r)) risk))
                                        bar)))))
          (let ((p-plan (+ (sum-for "safe" "planned_min") (sum-for "speculative" "planned_min")))
                (p-act (+ (sum-for "safe" "actual_min") (sum-for "speculative" "actual_min")))
                (spec-plan (sum-for "speculative" "planned_min"))
                (spec-act (sum-for "speculative" "actual_min")))
            (push "" out)
            (push "* By barbell class" out)
            (push "#+CAPTION: Planned versus actual minutes per risk class, the Rule 6 drift." out)
            (push "#+ATTR_LATEX: :booktabs t" out)
            (push (writing-habit-report--table
                   '("Risk class" "Planned" "Actual")
                   (mapcar (lambda (r)
                             (list (or (cdr (assoc "risk_class" r)) "untagged")
                                   (writing-habit-report--num r "planned_min")
                                   (writing-habit-report--num r "actual_min")))
                           bar))
                  out)
            (push (format "Speculative share: planned %s, actual %s."
                          (writing-habit-report--pct spec-plan p-plan)
                          (writing-habit-report--pct spec-act p-act))
                  out)))))
    (push "" out)
    (push (format "* Current streak of consecutive writing days: %d"
                  (writing-habit-compare-current-streak db))
          out)
    (when (and plot-file proj)
      (push "" out)
      (push "* Plot" out)
      (push (writing-habit-report--plot-src-block proj week plot-file) out))
    (concat (mapconcat #'identity (nreverse out) "\n") "\n")))

;;;###autoload
(defun writing-habit-report-week (db-file week &optional plot-file)
  "Show the weekly comparison report for WEEK from the database at DB-FILE.
Interactively, prompt for the database file and any date in the target week.
With a prefix argument, also prompt for a PLOT-FILE and embed an Org Babel
python block that renders a planned-versus-actual chart to it."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (org-read-date nil nil nil "Any date in the target week: ")
         (when current-prefix-arg
           (read-file-name "Write plot to (png): " nil "planned-vs-actual.png"))))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (let ((text (writing-habit-report-week-string db week plot-file))
              (buf (get-buffer-create "*writing-habit report*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert text)
              (goto-char (point-min)))
            (when (fboundp 'org-mode) (org-mode)))
          (display-buffer buf)
          buf)
      (writing-habit-db-close db))))

(provide 'writing-habit-report)
;;; writing-habit-report.el ends here
