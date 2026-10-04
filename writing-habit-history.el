;;; writing-habit-history.el --- Cross-week adherence history -*- lexical-binding: t; -*-

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

;; This module ports `compare/history.py' from the Python package.  It
;; reads the cross-week views through writing-habit-compare and renders
;; the weekly adherence series three ways.
;;
;; `writing-habit-history-string' prints the same plain-text table as
;; `render_text' in Python, line for line, so the batch command and the
;; Python command print the same thing for the same database.
;;
;; `writing-habit-history-org' returns the same data as an org table, the
;; Emacs-native view that folds, exports, and plots with org-plot.
;;
;; `writing-habit-history-write-plots' writes the five-panel figure through
;; a generated matplotlib script, the same way the weekly report writes its
;; bar chart.  It needs python3 with matplotlib.
;;
;; Public functions:
;;   `writing-habit-history-collect'      weeks and the five series
;;   `writing-habit-history-string'       plain-text table
;;   `writing-habit-history-org'          org-mode table
;;   `writing-habit-history-write-plots'  five-panel PNG
;;   `writing-habit-history'              interactive command

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'writing-habit-db)
(require 'writing-habit-compare)
(require 'writing-habit-report)

(declare-function org-table-align "org-table" ())
(declare-function org-mode "org" ())

(defconst writing-habit-history-categories '("generative" "editing" "support")
  "The three activities, in the order the table and plots show them.")

(defconst writing-habit-history--series-keys
  '("overall" "project_mean" "generative" "editing" "support")
  "Keys of the series alist returned by `writing-habit-history-collect'.")

(defconst writing-habit-history--category-color
  '(("generative" . "#2a78d6") ("editing" . "#008300") ("support" . "#e87ba4"))
  "Activity colors from the dashboard palette.")

(defun writing-habit-history--by-week (rows value-key)
  "Return a hash table mapping the week of each of ROWS to its VALUE-KEY."
  (let ((h (make-hash-table :test #'equal)))
    (dolist (r rows)
      (puthash (cdr (assoc "week_start" r)) (cdr (assoc value-key r)) h))
    h))

(defun writing-habit-history-collect (db &optional start end)
  "Return (WEEKS . SERIES) for DB between START and END.
WEEKS is the sorted list of week-start dates that appear in any series.
SERIES is an alist from each key in `writing-habit-history--series-keys'
to a hash table that maps a week to its adherence, or has no entry when
the week has no value."
  (let* ((overall (writing-habit-compare-overall-series db start end))
         (projmean (writing-habit-compare-project-mean-series db start end))
         (cats (mapcar (lambda (c)
                         (cons c (writing-habit-compare-category-mean-series
                                  db c start end)))
                       writing-habit-history-categories))
         (weeks '()))
    (dolist (rows (append (list overall projmean) (mapcar #'cdr cats)))
      (dolist (r rows)
        (cl-pushnew (cdr (assoc "week_start" r)) weeks :test #'equal)))
    (cons (sort weeks #'string<)
          (append
           (list (cons "overall" (writing-habit-history--by-week overall "adherence"))
                 (cons "project_mean"
                       (writing-habit-history--by-week projmean "mean_adherence")))
           (mapcar (lambda (c)
                     (cons (car c)
                           (writing-habit-history--by-week (cdr c) "mean_adherence")))
                   cats)))))

(defun writing-habit-history--value (series key week)
  "Return the value of KEY in SERIES for WEEK, or nil."
  (gethash week (cdr (assoc key series))))

(defun writing-habit-history--fmt (value)
  "Format VALUE to two places, or the placeholder used for a missing week."
  (if value (format "%.2f" value) "  -  "))

(defun writing-habit-history--pad-left (s width)
  "Right-align S in WIDTH columns, as Python's > format does."
  (if (>= (length s) width) s (concat (make-string (- width (length s)) ?\s) s)))

(defun writing-habit-history--pad-right (s width)
  "Left-align S in WIDTH columns, as Python's < format does."
  (if (>= (length s) width) s (concat s (make-string (- width (length s)) ?\s))))

(defun writing-habit-history-string (db &optional start end)
  "Return a plain-text table of the weekly adherence series in DB.
START and END are any dates in the first and last week to include.  The
text matches `render_text' in the Python package."
  (let* ((data (writing-habit-history-collect db start end))
         (weeks (car data))
         (series (cdr data))
         (rule (make-string 72 ?=))
         (lines (list rule "Weekly adherence history")))
    (if (null weeks)
        (mapconcat #'identity (nreverse (cons "No weeks in range." lines)) "\n")
      (push (concat (writing-habit-history--pad-right "week" 12) " "
                    (writing-habit-history--pad-left "overall" 8) " "
                    (writing-habit-history--pad-left "proj-mean" 10) " "
                    (writing-habit-history--pad-left "gen" 7) " "
                    (writing-habit-history--pad-left "edit" 7) " "
                    (writing-habit-history--pad-left "support" 8))
            lines)
      (push (make-string 72 ?-) lines)
      (dolist (w weeks)
        (cl-flet ((cell (key width)
                    (writing-habit-history--pad-left
                     (writing-habit-history--fmt
                      (writing-habit-history--value series key w))
                     width)))
          (push (concat (writing-habit-history--pad-right w 12) " "
                        (cell "overall" 8) " "
                        (cell "project_mean" 10) " "
                        (cell "generative" 7) " "
                        (cell "editing" 7) " "
                        (cell "support" 8))
                lines)))
      (push "" lines)
      (push (concat "overall is summed actual over summed planned. The other "
                    "columns are the mean of the per-project adherence ratios. "
                    "A value of 1.00 is on plan.")
            lines)
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun writing-habit-history-org (db &optional start end)
  "Return an Org section holding the adherence history of DB.
START and END are any dates in the first and last week to include."
  (let* ((data (writing-habit-history-collect db start end))
         (weeks (car data))
         (series (cdr data)))
    (concat
     "* Weekly adherence history\n\n"
     (if (null weeks)
         "No weeks in range.\n"
       (concat
        "#+CAPTION: Weekly adherence, where 1.00 is on plan.\n"
        "#+ATTR_LATEX: :booktabs t\n"
        "#+PLOT: title:\"Weekly adherence\" ind:1 type:2d with:linespoints\n"
        (writing-habit-report--table
         '("week" "overall" "proj-mean" "gen" "edit" "support")
         (mapcar (lambda (w)
                   (cons w (mapcar (lambda (k)
                                     (let ((v (writing-habit-history--value series k w)))
                                       (if v (format "%.2f" v) "")))
                                   writing-habit-history--series-keys)))
                 weeks))
        "\nThe overall column is summed actual over summed planned minutes.  "
        "The other columns are the mean of the per-project adherence ratios.\n")))))

(defun writing-habit-history--py-values (series key weeks)
  "Return a Python list literal of KEY's values in SERIES over WEEKS."
  (concat "["
          (mapconcat (lambda (w)
                       (let ((v (writing-habit-history--value series key w)))
                         (if v (format "%s" v) "None")))
                     weeks ", ")
          "]"))

(defun writing-habit-history--plot-python (weeks series out-path)
  "Return a matplotlib script that draws the five panels for WEEKS and SERIES.
The figure is saved to OUT-PATH.  It mirrors `write_plots' in Python."
  (concat
   "import matplotlib\n"
   "matplotlib.use(\"Agg\")\n"
   "import matplotlib.pyplot as plt\n"
   "from matplotlib.gridspec import GridSpec\n"
   "weeks = " (writing-habit-report--py-strings weeks) "\n"
   (mapconcat (lambda (k)
                (format "s_%s = %s\n" k (writing-habit-history--py-values series k weeks)))
              writing-habit-history--series-keys "")
   "x = list(range(len(weeks)))\n"
   "labels = [w[5:] for w in weeks]\n"
   "fig = plt.figure(figsize=(12, 7))\n"
   "gs = GridSpec(2, 6, figure=fig, hspace=0.55, wspace=0.7)\n"
   "panels = [\n"
   "  (fig.add_subplot(gs[0, 0:3]), \"Overall adherence (summed minutes)\", s_overall, \"#0b0b0b\"),\n"
   "  (fig.add_subplot(gs[0, 3:6]), \"Mean of per-project adherence\", s_project_mean, \"#52514e\"),\n"
   (format "  (fig.add_subplot(gs[1, 0:2]), \"Generative (mean per project)\", s_generative, %S),\n"
           (cdr (assoc "generative" writing-habit-history--category-color)))
   (format "  (fig.add_subplot(gs[1, 2:4]), \"Editing (mean per project)\", s_editing, %S),\n"
           (cdr (assoc "editing" writing-habit-history--category-color)))
   (format "  (fig.add_subplot(gs[1, 4:6]), \"Support (mean per project)\", s_support, %S),\n"
           (cdr (assoc "support" writing-habit-history--category-color)))
   "]\n"
   "ymax = 1.2\n"
   "for _a, _t, data, _c in panels:\n"
   "    for v in data:\n"
   "        if v is not None:\n"
   "            ymax = max(ymax, v)\n"
   "for ax, title, data, color in panels:\n"
   "    xs = [i for i, y in zip(x, data) if y is not None]\n"
   "    ys = [y for y in data if y is not None]\n"
   "    ax.axhline(1.0, color=\"#898781\", linewidth=1.0, linestyle=\"--\", zorder=1)\n"
   "    ax.plot(xs, ys, marker=\"o\", color=color, linewidth=1.8, zorder=2)\n"
   "    ax.set_title(title, fontsize=10)\n"
   "    ax.set_ylim(0, ymax * 1.05)\n"
   "    ax.set_xticks(x)\n"
   "    ax.set_xticklabels(labels, rotation=45, ha=\"right\", fontsize=7)\n"
   "    ax.set_ylabel(\"adherence\", fontsize=8)\n"
   "    ax.grid(True, axis=\"y\", color=\"#e1e0d9\", linewidth=0.6)\n"
   "fig.suptitle(\"Weekly adherence history\", fontsize=13)\n"
   (format "fig.savefig(%S, dpi=120, bbox_inches=\"tight\")\n" out-path)
   "plt.close(fig)\n"))

(defun writing-habit-history-write-plots (db out-path &optional start end)
  "Write the five weekly adherence plots from DB to OUT-PATH.
START and END are any dates in the first and last week to include.
This runs `writing-habit-report-python' with matplotlib.  Return OUT-PATH,
or signal an error when there are no weeks or Python fails."
  (let* ((data (writing-habit-history-collect db start end))
         (weeks (car data)))
    (unless weeks (error "No weeks in range to plot"))
    (let ((tmp (make-temp-file "wh-history" nil ".py"
                               (writing-habit-history--plot-python
                                weeks (cdr data) out-path))))
      (unwind-protect
          (with-temp-buffer
            (unless (eq 0 (call-process writing-habit-report-python nil t nil tmp))
              (error "Plot generation failed: %s" (string-trim (buffer-string)))))
        (delete-file tmp)))
    out-path))

;;;###autoload
(defun writing-habit-history (db-file &optional start end)
  "Show the cross-week adherence history of DB-FILE in an org buffer.
START and END are optional dates in the first and last week to show.
Interactively, leave either prompt empty to leave that end open."
  (interactive
   (list (read-file-name "Database file: ")
         (let ((s (read-string "From week (empty for all): "))) (unless (string-empty-p s) s))
         (let ((s (read-string "To week (empty for all): "))) (unless (string-empty-p s) s))))
  (let* ((db (writing-habit-db-connect db-file))
         (text (unwind-protect (writing-habit-history-org db start end)
                 (writing-habit-db-close db))))
    (with-current-buffer (get-buffer-create "*writing-habit history*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (org-mode)
        (goto-char (point-min))
        (when (re-search-forward "^|" nil t) (org-table-align)))
      (display-buffer (current-buffer)))
    text))

(provide 'writing-habit-history)
;;; writing-habit-history.el ends here
