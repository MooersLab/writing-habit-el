;;; writing-habit-dashboard.el --- HTML dashboard for a writing week -*- lexical-binding: t; -*-

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

;; A self-contained HTML dashboard for one writing week, the twin of the
;; Python dashboard.py.  It has two panels.  The first redraws the week's
;; planned schedule as a time-by-day grid, colored by activity.  The second
;; compares planned against actual effort: per-project meters with adherence,
;; the activity balance, and the barbell split, plus the summary tiles and the
;; streak.  The output is one HTML file with inline CSS and no external
;; requests.
;;
;; The markup is built deterministically from the shared database, and the
;; static blocks below are the same text the Python module carries, so the two
;; produce byte-identical files from the same data.
;;
;; Colors come from the data-viz reference palette: the three activities take
;; blue, green, and magenta, and the planned and actual marks take blue and
;; orange, which sit far apart for colorblind readers.  Values appear as direct
;; labels in ink, never by color alone, and the schedule is a labeled table.
;;
;; Public functions:
;;   `writing-habit-dashboard-html'   return the dashboard HTML as a string
;;   `writing-habit-dashboard-write'  render and write it to a file
;;   `writing-habit-dashboard'        interactive command, writes and opens it

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'writing-habit-db)
(require 'writing-habit-compare)

(declare-function org-read-date "org" (&rest args))

(defconst writing-habit-dashboard--day-labels
  ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]
  "Day names indexed by offset from Monday.")

(defconst writing-habit-dashboard--activity-class
  '(("generative" . "gen") ("editing" . "edit") ("support" . "sup"))
  "Map an activity name to its CSS class.")

(defconst writing-habit-dashboard--style
  "  <style>
    .wh { color-scheme: light;
      --surface: #fcfcfb; --plane: #f9f9f7; --ink: #0b0b0b; --ink2: #52514e;
      --muted: #898781; --grid: #e1e0d9; --axis: #c3c2b7;
      --border: rgba(11,11,11,0.10);
      --gen: #2a78d6; --edit: #008300; --sup: #e87ba4;
      --planned: #2a78d6; --actual: #eb6834; }
    @media (prefers-color-scheme: dark) {
      .wh:not([data-theme=\"light\"]) { color-scheme: dark;
        --surface: #1a1a19; --plane: #0d0d0d; --ink: #ffffff; --ink2: #c3c2b7;
        --muted: #898781; --grid: #2c2c2a; --axis: #383835;
        --border: rgba(255,255,255,0.10);
        --gen: #3987e5; --edit: #008300; --sup: #d55181;
        --planned: #3987e5; --actual: #d95926; } }
    .wh[data-theme=\"dark\"] { color-scheme: dark;
      --surface: #1a1a19; --plane: #0d0d0d; --ink: #ffffff; --ink2: #c3c2b7;
      --muted: #898781; --grid: #2c2c2a; --axis: #383835;
      --border: rgba(255,255,255,0.10);
      --gen: #3987e5; --edit: #008300; --sup: #d55181;
      --planned: #3987e5; --actual: #d95926; }
    * { box-sizing: border-box; }
    body.wh { margin: 0; background: var(--plane); color: var(--ink); font-family: system-ui, -apple-system, \"Segoe UI\", sans-serif; line-height: 1.45; }
    .wrap { max-width: 960px; margin: 0 auto; padding: 24px 20px 48px; }
    header { display: flex; align-items: baseline; justify-content: space-between; gap: 12px; flex-wrap: wrap; }
    h1 { font-size: 22px; margin: 0; }
    h2 { font-size: 15px; margin: 32px 0 12px; letter-spacing: 0.02em; text-transform: uppercase; color: var(--ink2); }
    .sub { color: var(--ink2); margin: 2px 0 0; }
    .toggle { border: 1px solid var(--border); background: var(--surface); color: var(--ink2); border-radius: 8px; padding: 6px 10px; font: inherit; font-size: 13px; cursor: pointer; }
    .tiles { display: grid; grid-template-columns: repeat(4, 1fr); gap: 12px; margin-top: 20px; }
    .tile { background: var(--surface); border: 1px solid var(--border); border-radius: 12px; padding: 14px 16px; }
    .tile .v { font-size: 26px; font-weight: 650; }
    .tile .k { color: var(--muted); font-size: 12px; margin-top: 2px; }
    table { border-collapse: collapse; width: 100%; background: var(--surface); border: 1px solid var(--border); border-radius: 12px; overflow: hidden; margin-top: 4px; }
    caption { text-align: left; color: var(--muted); font-size: 12px; padding: 0 2px 6px; }
    th, td { padding: 7px 10px; text-align: left; font-size: 13px; border-bottom: 1px solid var(--grid); }
    thead th { color: var(--muted); font-weight: 600; }
    tbody tr:last-child td, tbody tr:last-child th { border-bottom: 0; }
    .num { text-align: right; font-variant-numeric: tabular-nums; }
    .time { color: var(--ink2); font-variant-numeric: tabular-nums; white-space: nowrap; }
    .cell { font-weight: 600; }
    .cell.gen { background: color-mix(in srgb, var(--gen) 15%, transparent); border-left: 3px solid var(--gen); }
    .cell.edit { background: color-mix(in srgb, var(--edit) 15%, transparent); border-left: 3px solid var(--edit); }
    .cell.sup { background: color-mix(in srgb, var(--sup) 15%, transparent); border-left: 3px solid var(--sup); }
    .meter { display: flex; align-items: center; gap: 8px; }
    .track { flex: 1; min-width: 120px; }
    .bar { height: 8px; border-radius: 4px; }
    .bar.planned { background: var(--planned); }
    .bar.actual { background: var(--actual); margin-top: 2px; }
    .swatch { display: inline-block; width: 10px; height: 10px; border-radius: 3px; vertical-align: middle; margin-right: 5px; }
    .legend { display: flex; gap: 16px; color: var(--ink2); font-size: 12px; margin: 8px 2px 0; flex-wrap: wrap; }
    .foot { color: var(--muted); font-size: 11px; margin-top: 28px; }
  </style>"
  "The full style block, held verbatim to match the Python twin.")

(defconst writing-habit-dashboard--script
  "  <script>
    document.getElementById('wh-theme').addEventListener('click', function () {
      var b = document.body;
      var dark = b.getAttribute('data-theme') === 'dark';
      b.setAttribute('data-theme', dark ? 'light' : 'dark');
      this.textContent = dark ? 'Dark theme' : 'Light theme';
    });
  </script>"
  "The theme-toggle script, held verbatim to match the Python twin.")


;;;; Small helpers

(defun writing-habit-dashboard--esc (text)
  "Escape the characters that matter inside HTML text and attributes in TEXT."
  (let ((s (if (null text) "" (format "%s" text))))
    (setq s (string-replace "&" "&amp;" s))
    (setq s (string-replace "<" "&lt;" s))
    (setq s (string-replace ">" "&gt;" s))
    (setq s (string-replace "\"" "&quot;" s))
    s))

(defun writing-habit-dashboard--pct (value whole)
  "Return VALUE as a whole-number percent of WHOLE, rounded half up."
  (if (<= whole 0) 0 (floor (+ 0.5 (/ (* 100.0 value) whole)))))

(defun writing-habit-dashboard--fmt2 (value)
  "Format an adherence VALUE, or \"n/a\" when nil."
  (if (null value) "n/a" (format "%.2f" value)))

(defun writing-habit-dashboard--ratio (actual planned)
  "Format ACTUAL over PLANNED to two places, or \"n/a\" when PLANNED is zero."
  (if (and planned (> planned 0)) (format "%.2f" (/ (float actual) planned)) "n/a"))

(defun writing-habit-dashboard--meter (planned actual scale)
  "Return a two-bar meter of PLANNED and ACTUAL, scaled to SCALE."
  (concat
   "<div class=\"meter\"><div class=\"track\">"
   (format "<div class=\"bar planned\" style=\"width:%d%%\"></div>"
           (writing-habit-dashboard--pct planned scale))
   (format "<div class=\"bar actual\" style=\"width:%d%%\"></div>"
           (writing-habit-dashboard--pct actual scale))
   "</div></div>"))

(defun writing-habit-dashboard--num (row key)
  "Return the integer in ROW under KEY as a string."
  (number-to-string (cdr (assoc key row))))

(defun writing-habit-dashboard--schedule-rows (db week-start)
  "Return the planned blocks for WEEK-START as (offset start end code activity)."
  (sqlite-select
   db (concat "SELECT CAST(julianday(b.day) - julianday(b.week_start) AS INTEGER) AS offset,"
              " b.start_time, b.end_time, p.code, c.name AS activity"
              " FROM plan_block b"
              " JOIN project p ON p.project_id = b.project_id"
              " JOIN category c ON c.category_id = b.category_id"
              " WHERE b.week_start = ?"
              " ORDER BY b.start_time, offset, p.code")
   (list week-start)))


;;;; Section builders

(defun writing-habit-dashboard--tiles (planned-total actual-total streak)
  "Return the summary tile lines for PLANNED-TOTAL, ACTUAL-TOTAL, and STREAK."
  (list
   "  <section class=\"tiles\">"
   (concat "    <div class=\"tile\"><div class=\"v\">" (number-to-string planned-total)
           "</div><div class=\"k\">planned minutes</div></div>")
   (concat "    <div class=\"tile\"><div class=\"v\">" (number-to-string actual-total)
           "</div><div class=\"k\">actual minutes</div></div>")
   (concat "    <div class=\"tile\"><div class=\"v\">"
           (writing-habit-dashboard--ratio actual-total planned-total)
           "</div><div class=\"k\">overall adherence</div></div>")
   (concat "    <div class=\"tile\"><div class=\"v\">" (number-to-string streak)
           "</div><div class=\"k\">day writing streak</div></div>")
   "  </section>"))

(defun writing-habit-dashboard--schedule (rows)
  "Return the schedule grid lines for the plan-block ROWS."
  (if (null rows)
      (list "  <h2>Schedule</h2>" "  <p class=\"sub\">No planned blocks this week.</p>")
    (let ((offsets '()) (slots '()) (cells (make-hash-table :test 'equal)))
      (dolist (r rows)
        (let ((off (nth 0 r)) (key (concat (nth 1 r) "-" (nth 2 r))))
          (unless (member off offsets) (push off offsets))
          (unless (member key slots) (setq slots (append slots (list key))))
          (let ((ck (list (cl-position key slots :test #'equal) off)))
            (unless (gethash ck cells) (puthash ck (list (nth 3 r) (nth 4 r)) cells)))))
      (setq offsets (sort offsets #'<))
      (let ((out (list "  <h2>Schedule</h2>" "  <table>"
                       "    <caption>Planned blocks, colored by activity.</caption>"))
            (head "    <thead><tr><th>Time</th>"))
        (dolist (off offsets)
          (setq head (concat head "<th>"
                             (writing-habit-dashboard--esc
                              (if (and (>= off 0) (< off 7))
                                  (aref writing-habit-dashboard--day-labels off)
                                off))
                             "</th>")))
        (setq out (append out (list (concat head "</tr></thead>") "    <tbody>")))
        (let ((idx 0))
          (dolist (key slots)
            (let ((rowstr (concat "      <tr><th class=\"time\">"
                                  (writing-habit-dashboard--esc key) "</th>")))
              (dolist (off offsets)
                (let ((cell (gethash (list idx off) cells)))
                  (if cell
                      (let ((cls (or (cdr (assoc (nth 1 cell)
                                                 writing-habit-dashboard--activity-class))
                                     "gen")))
                        (setq rowstr (concat rowstr "<td class=\"cell " cls "\">"
                                             (writing-habit-dashboard--esc (nth 0 cell))
                                             "</td>")))
                    (setq rowstr (concat rowstr "<td></td>")))))
              (setq out (append out (list (concat rowstr "</tr>"))))
              (setq idx (1+ idx)))))
        (append out (list "    </tbody>" "  </table>"
                          (concat "  <div class=\"legend\">"
                                  "<span><span class=\"swatch\" style=\"background:var(--gen)\"></span>generative</span>"
                                  "<span><span class=\"swatch\" style=\"background:var(--edit)\"></span>editing</span>"
                                  "<span><span class=\"swatch\" style=\"background:var(--sup)\"></span>support</span>"
                                  "</div>")))))))

(defun writing-habit-dashboard--projects (proj)
  "Return the per-project comparison lines for PROJ."
  (if (null proj)
      '()
    (let ((scale 0))
      (dolist (r proj)
        (setq scale (max scale (cdr (assoc "planned_min" r)) (cdr (assoc "actual_min" r)))))
      (let ((out (list "  <h2>Planned vs actual by project</h2>" "  <table>"
                       (concat "    <thead><tr><th>Code</th><th>Project</th>"
                               "<th class=\"num\">Planned</th><th class=\"num\">Actual</th>"
                               "<th>Progress</th><th class=\"num\">Adherence</th></tr></thead>")
                       "    <tbody>")))
        (dolist (r proj)
          (setq out (append out (list
            (concat "      <tr><td class=\"cell\">"
                    (writing-habit-dashboard--esc (cdr (assoc "code" r))) "</td><td>"
                    (writing-habit-dashboard--esc (or (cdr (assoc "description" r)) ""))
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "planned_min")
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "actual_min")
                    "</td><td>" (writing-habit-dashboard--meter
                                 (cdr (assoc "planned_min" r)) (cdr (assoc "actual_min" r)) scale)
                    "</td><td class=\"num\">"
                    (writing-habit-dashboard--fmt2 (cdr (assoc "adherence" r)))
                    "</td></tr>")))))
        (append out (list "    </tbody>" "  </table>"
                          (concat "  <div class=\"legend\">"
                                  "<span><span class=\"swatch\" style=\"background:var(--planned)\"></span>planned</span>"
                                  "<span><span class=\"swatch\" style=\"background:var(--actual)\"></span>actual</span>"
                                  "</div>")))))))

(defun writing-habit-dashboard--activities (cat)
  "Return the activity-balance lines for CAT."
  (if (null cat)
      '()
    (let ((scale 0))
      (dolist (r cat)
        (setq scale (max scale (cdr (assoc "planned_min" r)) (cdr (assoc "actual_min" r)))))
      (let ((out (list "  <h2>Activity balance</h2>" "  <table>"
                       (concat "    <thead><tr><th>Activity</th>"
                               "<th class=\"num\">Planned</th><th class=\"num\">Actual</th>"
                               "<th>Progress</th></tr></thead>")
                       "    <tbody>")))
        (dolist (r cat)
          (setq out (append out (list
            (concat "      <tr><td>" (writing-habit-dashboard--esc (cdr (assoc "category" r)))
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "planned_min")
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "actual_min")
                    "</td><td>" (writing-habit-dashboard--meter
                                 (cdr (assoc "planned_min" r)) (cdr (assoc "actual_min" r)) scale)
                    "</td></tr>")))))
        (append out (list "    </tbody>" "  </table>"))))))

(defun writing-habit-dashboard--barbell (bar)
  "Return the barbell-split lines for BAR."
  (if (null bar)
      '()
    (cl-flet ((sum-for (risk field)
                (apply #'+ (mapcar (lambda (r) (cdr (assoc field r)))
                                   (seq-filter
                                    (lambda (r) (equal (cdr (assoc "risk_class" r)) risk))
                                    bar)))))
      (let ((p-plan (+ (sum-for "safe" "planned_min") (sum-for "speculative" "planned_min")))
            (p-act (+ (sum-for "safe" "actual_min") (sum-for "speculative" "actual_min")))
            (spec-plan (sum-for "speculative" "planned_min"))
            (spec-act (sum-for "speculative" "actual_min"))
            (out (list "  <h2>Barbell split</h2>" "  <table>"
                       (concat "    <thead><tr><th>Risk class</th>"
                               "<th class=\"num\">Planned</th><th class=\"num\">Actual</th></tr></thead>")
                       "    <tbody>")))
        (dolist (r bar)
          (setq out (append out (list
            (concat "      <tr><td>"
                    (writing-habit-dashboard--esc (or (cdr (assoc "risk_class" r)) "untagged"))
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "planned_min")
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "actual_min")
                    "</td></tr>")))))
        (append out (list "    </tbody>" "  </table>"
                          (concat "  <p class=\"sub\">Speculative share: planned "
                                  (number-to-string (writing-habit-dashboard--pct spec-plan p-plan))
                                  "%, actual "
                                  (number-to-string (writing-habit-dashboard--pct spec-act p-act))
                                  "%.</p>")))))))


;;;; Assembly

(defun writing-habit-dashboard-html (db week)
  "Return a self-contained HTML dashboard string for the week containing WEEK."
  (let* ((week-start (writing-habit-compare--monday week))
         (proj (writing-habit-compare-week-project db week))
         (cat (writing-habit-compare-week-category db week))
         (bar (writing-habit-compare-week-barbell db week))
         (streak (writing-habit-compare-current-streak db))
         (planned-total (apply #'+ (mapcar (lambda (r) (cdr (assoc "planned_min" r))) proj)))
         (actual-total (apply #'+ (mapcar (lambda (r) (cdr (assoc "actual_min" r))) proj)))
         (lines (list
                 "<!DOCTYPE html>"
                 "<html lang=\"en\">"
                 "<head>"
                 "  <meta charset=\"utf-8\">"
                 "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                 (concat "  <title>Writing dashboard, week of "
                         (writing-habit-dashboard--esc week-start) "</title>")
                 writing-habit-dashboard--style
                 "</head>"
                 "<body class=\"wh\">"
                 "  <div class=\"wrap\">"
                 "  <header>"
                 (concat "    <div><h1>Writing dashboard</h1><p class=\"sub\">Week of "
                         (writing-habit-dashboard--esc week-start) "</p></div>")
                 "    <button id=\"wh-theme\" class=\"toggle\" type=\"button\">Dark theme</button>"
                 "  </header>")))
    (setq lines (append lines (writing-habit-dashboard--tiles planned-total actual-total streak)))
    (setq lines (append lines (writing-habit-dashboard--schedule
                               (writing-habit-dashboard--schedule-rows db week-start))))
    (setq lines (append lines (writing-habit-dashboard--projects proj)))
    (setq lines (append lines (writing-habit-dashboard--activities cat)))
    (setq lines (append lines (writing-habit-dashboard--barbell bar)))
    (setq lines (append lines (list
                               (concat "  <p class=\"foot\">Generated by writing-habit from the"
                                       " shared SQLite database. Planned versus actual,"
                                       " self-reported effort.</p>")
                               "  </div>"
                               writing-habit-dashboard--script
                               "</body>"
                               "</html>"
                               "")))
    (mapconcat #'identity lines "\n")))

(defun writing-habit-dashboard-write (db week out-path)
  "Render the dashboard for WEEK from DB, write it to OUT-PATH, return OUT-PATH."
  (with-temp-file out-path
    (insert (writing-habit-dashboard-html db week)))
  out-path)

;;;###autoload
(defun writing-habit-dashboard (db-file week out-file)
  "Write an HTML dashboard for WEEK from the database at DB-FILE to OUT-FILE.
Interactively, prompt for each, then open the dashboard in a browser."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (org-read-date nil nil nil "Any date in the target week: ")
         (read-file-name "Write dashboard to (html): " nil "writing-dashboard.html")))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (progn
          (writing-habit-dashboard-write db week out-file)
          (message "Wrote dashboard to %s" out-file)
          (when (called-interactively-p 'interactive)
            (browse-url (concat "file://" (expand-file-name out-file))))
          out-file)
      (writing-habit-db-close db))))

(provide 'writing-habit-dashboard)
;;; writing-habit-dashboard.el ends here
