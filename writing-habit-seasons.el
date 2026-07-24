;;; writing-habit-seasons.el --- Grouped-adherence dashboard -*- lexical-binding: t; -*-

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

;; The second dashboard, the twin of the Python seasons.py.  It groups the
;; weekly overall series by month, event context, and schedule code, and renders
;; a self-contained HTML page that reuses the weekly dashboard's style, script,
;; and helpers, so the two ports produce byte-identical files from the same data.
;;
;; Public functions:
;;   `writing-habit-seasons-html'   return the dashboard HTML as a string
;;   `writing-habit-seasons-write'  render and write it to a file
;;   `writing-habit-seasons'        interactive command, writes and opens it

;;; Code:

(require 'writing-habit-db)
(require 'writing-habit-compare)
(require 'writing-habit-dashboard)

(declare-function org-read-date "org" (&rest args))

(defun writing-habit-seasons--section (title subtitle label-head rows label-key empty)
  "Return the lines for one grouping table.
ROWS are view rows, LABEL-KEY names the group column, and EMPTY is the message
shown when there are no rows."
  (if (null rows)
      (list (concat "  <h2>" (writing-habit-dashboard--esc title) "</h2>")
            (concat "  <p class=\"sub\">" (writing-habit-dashboard--esc empty) "</p>"))
    (let ((scale 0))
      (dolist (r rows)
        (setq scale (max scale (cdr (assoc "planned_min" r)) (cdr (assoc "actual_min" r)))))
      (let ((out (list
                  (concat "  <h2>" (writing-habit-dashboard--esc title) "</h2>")
                  (concat "  <p class=\"sub\">" (writing-habit-dashboard--esc subtitle) "</p>")
                  "  <table>"
                  (concat "    <thead><tr><th>" (writing-habit-dashboard--esc label-head) "</th>"
                          "<th class=\"num\">Weeks</th><th class=\"num\">Planned</th>"
                          "<th class=\"num\">Actual</th><th>Progress</th>"
                          "<th class=\"num\">Adherence</th></tr></thead>")
                  "    <tbody>")))
        (dolist (r rows)
          (setq out (append out (list
            (concat "      <tr><td>" (writing-habit-dashboard--esc (cdr (assoc label-key r)))
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "weeks")
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "planned_min")
                    "</td><td class=\"num\">" (writing-habit-dashboard--num r "actual_min")
                    "</td><td>" (writing-habit-dashboard--meter
                                 (cdr (assoc "planned_min" r)) (cdr (assoc "actual_min" r)) scale)
                    "</td><td class=\"num\">"
                    (writing-habit-dashboard--fmt2 (cdr (assoc "adherence" r)))
                    "</td></tr>")))))
        (append out (list "    </tbody>" "  </table>"))))))

(defun writing-habit-seasons-html (db)
  "Return the self-contained grouped-adherence dashboard HTML from DB."
  (let* ((months (writing-habit-compare-month-overall db))
         (contexts (writing-habit-compare-context-overall db))
         (schedules (writing-habit-compare-schedule-overall db))
         (planned-total (apply #'+ (mapcar (lambda (r) (cdr (assoc "planned_min" r))) months)))
         (actual-total (apply #'+ (mapcar (lambda (r) (cdr (assoc "actual_min" r))) months)))
         (week-total (apply #'+ (mapcar (lambda (r) (cdr (assoc "weeks" r))) months)))
         (lines (list
                 "<!DOCTYPE html>"
                 "<html lang=\"en\">"
                 "<head>"
                 "  <meta charset=\"utf-8\">"
                 "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                 "  <title>Writing seasons: grouped adherence</title>"
                 writing-habit-dashboard--style
                 "</head>"
                 "<body class=\"wh\">"
                 "  <div class=\"wrap\">"
                 "  <header>"
                 (concat "    <div><h1>Writing seasons</h1><p class=\"sub\">Grouped adherence by month,"
                         " context, and schedule</p></div>")
                 "    <button id=\"wh-theme\" class=\"toggle\" type=\"button\">Dark theme</button>"
                 "  </header>"
                 "  <section class=\"tiles\">"
                 (concat "    <div class=\"tile\"><div class=\"v\">" (number-to-string week-total)
                         "</div><div class=\"k\">weeks recorded</div></div>")
                 (concat "    <div class=\"tile\"><div class=\"v\">" (number-to-string planned-total)
                         "</div><div class=\"k\">planned minutes</div></div>")
                 (concat "    <div class=\"tile\"><div class=\"v\">" (number-to-string actual-total)
                         "</div><div class=\"k\">actual minutes</div></div>")
                 (concat "    <div class=\"tile\"><div class=\"v\">"
                         (writing-habit-dashboard--ratio actual-total planned-total)
                         "</div><div class=\"k\">overall adherence</div></div>")
                 "  </section>")))
    (setq lines (append lines (writing-habit-seasons--section
                               "By month"
                               "Seasonal trend in adherence across the calendar."
                               "Month" months "month"
                               "No weeks recorded yet.")))
    (setq lines (append lines (writing-habit-seasons--section
                               "By event context"
                               "Adherence in weeks tagged with an event, for example a meeting, teaching, or data collection."
                               "Context" contexts "tag"
                               "No context tags recorded. Set one with: writing-habit context set --week DATE --tag teaching.")))
    (setq lines (append lines (writing-habit-seasons--section
                               "By schedule"
                               "Adherence grouped by the schedule file-name code, so plan shapes can be compared."
                               "Schedule" schedules "schedule_code"
                               "No schedule codes recorded. They are captured at plan import from the table file name.")))
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

(defun writing-habit-seasons-write (db out-path)
  "Render the seasons dashboard from DB, write it to OUT-PATH, return OUT-PATH."
  (with-temp-file out-path
    (insert (writing-habit-seasons-html db)))
  out-path)

;;;###autoload
(defun writing-habit-seasons (db-file out-file)
  "Write the grouped-adherence dashboard from DB-FILE to OUT-FILE.
Interactively, prompt for each, then open the dashboard in a browser."
  (interactive
   (list (read-file-name "Database file: " nil nil t)
         (read-file-name "Write seasons dashboard to (html): " nil "writing-seasons.html")))
  (let ((db (writing-habit-db-connect db-file)))
    (unwind-protect
        (progn
          (writing-habit-seasons-write db out-file)
          (message "Wrote seasons dashboard to %s" out-file)
          (when (called-interactively-p 'interactive)
            (browse-url (concat "file://" (expand-file-name out-file))))
          out-file)
      (writing-habit-db-close db))))

(provide 'writing-habit-seasons)
;;; writing-habit-seasons.el ends here
