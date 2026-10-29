;;; writing-habit-prospecting-tests.el --- ERT tests for risky prospecting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Risky prospecting is tracked work on a project that is on no list.
;; Track sync keeps it under the project's directory name and flags it,
;; the comparison views leave it out of planned versus actual, and the
;; dashboard and the report give it a section of its own.  These tests
;; mirror tests/test_prospecting.py in the Python package, and the
;; dashboard is checked byte for byte against the render the Python port
;; wrote to fixtures/prospect.html.

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-db)
(require 'writing-habit-track)
(require 'writing-habit-compare)
(require 'writing-habit-dashboard)
(require 'writing-habit-report)

(defconst writing-habit-prospecting-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst writing-habit-prospecting-tests--schema
  (expand-file-name "schema.sql"
                    (locate-dominating-file writing-habit-prospecting-tests--dir "schema.sql")))

(defconst writing-habit-prospecting-tests--week "2026-10-05")

(defconst writing-habit-prospecting-tests--legend
  (concat
   "| Time <l>    | M  | Tu |\n"
   "|-------------+----+----|\n"
   "| 05:30-07:00 | gZ |    |\n"
   "|-------------+----+----|\n"
   "| Z: 1008ToolsForQCrOfRNA, submitted October 5 :safe: | | |\n"
   "| X: 0436mpQCr December 1 :risky: | | |\n")
  "The legend of the Python prospecting tests.")

(defconst writing-habit-prospecting-tests--semester
  (concat "\n#+NAME: current-semester-projects\n"
          "| - 0103DACEabinitio 2026-12-15 :safe: |\n")
  "A current semester list with one project that is not in the legend.")

(defconst writing-habit-prospecting-tests--rows
  '((1 "2026-10-05" "05:30" "07:00" 1008 "1008ToolsForQCrOfRNA" "grant" "")
    (2 "2026-10-05" "08:00" "09:00" 1424 "1424AIseededPhasing" "idea" "G")
    (3 "2026-10-06" "08:00" "08:30" 1424 "1424AIseededPhasing" "" "")
    (4 "2026-10-06" "06:00" "06:30" 6005 "6005EveningCommute" "" "")
    (5 "2026-10-07" "09:00" "10:00" 103 "0103DACEabinitio" "" "")
    (6 "2026-10-07" "10:00" "10:20" 2026 "2026words" "journal" "")
    (7 "2026-10-08" "05:30" "06:30" 436 "0436mpQCr" "" "G"))
  "zTimeSpent rows: id, date, start, end, ProjectID, directory, note, activity.")

(defun writing-habit-prospecting-tests--mytime (path rows)
  "Write a mytime.db at PATH holding ROWS in zTimeSpent."
  (let ((src (sqlite-open path)))
    (unwind-protect
        (progn
          (sqlite-execute
           src (concat "CREATE TABLE zTimeSpent (id INTEGER PRIMARY KEY, DateDashed DATE,"
                       " Start TIME, \"End\" TIME, ProjectID INT, ProjectDirectory TEXT,"
                       " Description TEXT, Activity TEXT DEFAULT none)"))
          (dolist (r rows)
            (sqlite-execute src "INSERT INTO zTimeSpent VALUES (?,?,?,?,?,?,?,?)" r)))
      (sqlite-close src))))

(defmacro writing-habit-prospecting-tests--with (vars &rest body)
  "Run BODY with VARS (DB SOURCE TABLE) bound to a planned week."
  (declare (indent 1))
  (let ((db (nth 0 vars)) (source (nth 1 vars)) (table (nth 2 vars))
        (dir (make-symbol "dir")))
    `(let* ((,dir (make-temp-file "wh-prospect" t))
            (,table (expand-file-name "Oct5.org" ,dir))
            (,source (expand-file-name "mytime.db" ,dir))
            (,db (writing-habit-db-connect)))
       (unwind-protect
           (progn
             (writing-habit-db-init ,db writing-habit-prospecting-tests--schema)
             (with-temp-file ,table
               (insert writing-habit-prospecting-tests--legend
                       writing-habit-prospecting-tests--semester))
             (writing-habit-prospecting-tests--mytime
              ,source writing-habit-prospecting-tests--rows)
             (sqlite-execute ,db (concat "INSERT INTO plan_week(week_start, schedule_code,"
                                         " table_path) VALUES (?,?,?)")
                             (list writing-habit-prospecting-tests--week "Oct5" ,table))
             (let ((gen (writing-habit-db-get-category-id ,db "generative"))
                   (z (writing-habit-db-get-or-create-project
                       ,db "Z" "1008ToolsForQCrOfRNA" "safe"))
                   (x (writing-habit-db-get-or-create-project
                       ,db "X" "0436mpQCr" "speculative")))
               (sqlite-execute ,db (concat "INSERT INTO plan_block(day, start_time, end_time,"
                                           " project_id, category_id) VALUES"
                                           " ('2026-10-05', '05:30', '07:00', ?, ?)")
                               (list z gen))
               (sqlite-execute ,db (concat "INSERT INTO plan_block(day, start_time, end_time,"
                                           " project_id, category_id) VALUES"
                                           " ('2026-10-08', '05:30', '06:30', ?, ?)")
                               (list x gen)))
             ,@body)
         (writing-habit-db-close ,db)
         (delete-directory ,dir t)))))

(defun writing-habit-prospecting-tests--flags (db)
  "Return an alist of mytime id to (CODE PROSPECTING) in DB."
  (mapcar (lambda (r) (cons (car (last (split-string (nth 0 r) ":"))) (cdr r)))
          (sqlite-select
           db (concat "SELECT s.source_ref, p.code, s.prospecting FROM session s"
                      " JOIN project p USING(project_id) ORDER BY s.source_ref"))))

(ert-deftest writing-habit-prospecting/parse-ranges ()
  "Ranges parse as in the Python package."
  (should (equal (writing-habit-track-parse-ranges "0000-1999, 2100-5999")
                 '((0 . 1999) (2100 . 5999))))
  (should (equal (writing-habit-track-parse-ranges "3275") '((3275 . 3275))))
  (should (null (writing-habit-track-parse-ranges "")))
  (should (writing-habit-track-in-ranges "1424" '((0 . 1999))))
  (should-not (writing-habit-track-in-ranges "6005" '((0 . 1999))))
  (should-error (writing-habit-track-parse-ranges "abc")))

(ert-deftest writing-habit-prospecting/sync-labels-unlisted-writing ()
  "Work on a writing project on no list is kept and labeled risky."
  (skip-unless (sqlite-available-p))
  (writing-habit-prospecting-tests--with (db src _table)
    (let* ((report (writing-habit-track-sync-mytime db src))
           (flags (writing-habit-prospecting-tests--flags db)))
      (should (equal (cdr (assoc "2" flags)) '("1424AIseededPhasing" 1)))
      (should (equal (cdr (assoc "3" flags)) '("1424AIseededPhasing" 1)))
      (should (equal (cdr (assoc "1" flags)) '("Z" 0)))
      (should (equal (cadr (cdr (assoc "5" flags))) 0))
      (should-not (assoc "4" flags))
      (should-not (assoc "6" flags))
      (should (= (writing-habit-sync-prospecting report) 2))
      (should (= (writing-habit-sync-semester-unplanned report) 1))
      (should (equal (writing-habit-sync-prospecting-minutes report)
                     '(("1424AIseededPhasing" . 90))))
      (should (string-match-p
               "kept 2 on no list as risky prospecting"
               (writing-habit-track-sync-report-string report src "2026-10-05" nil nil))))))

(ert-deftest writing-habit-prospecting/views-leave-it-out ()
  "The plan comparisons skip prospecting, and its own view reports it."
  (skip-unless (sqlite-available-p))
  (writing-habit-prospecting-tests--with (db src _table)
    (writing-habit-track-sync-mytime db src)
    (let ((week writing-habit-prospecting-tests--week))
      (should-not (member "1424AIseededPhasing"
                          (mapcar (lambda (r) (cdr (assoc "code" r)))
                                  (writing-habit-compare-week-project db week))))
      (should (= (cdr (assoc "actual_min" (car (writing-habit-compare-overall-series db))))
                 (+ 90 60 60)))
      (should (equal (mapcar (lambda (r) (mapcar (lambda (k) (cdr (assoc k r)))
                                                 '("code" "sessions" "actual_min" "weeks_so_far")))
                             (writing-habit-compare-week-prospecting db week))
                     '(("1424AIseededPhasing" 2 90 1)))))))

(ert-deftest writing-habit-prospecting/label-clears-once-listed ()
  "A rerun after the project joins a list clears the flag."
  (skip-unless (sqlite-available-p))
  (writing-habit-prospecting-tests--with (db src table)
    (writing-habit-track-sync-mytime db src)
    (with-temp-file table
      (insert writing-habit-prospecting-tests--legend
              writing-habit-prospecting-tests--semester
              "| - 1424AIseededPhasing 2027-03-01 :risky: |\n"))
    (let ((again (writing-habit-track-sync-mytime db src)))
      (should (= (writing-habit-sync-updated again) 2))
      (should (= (writing-habit-sync-prospecting again) 0))
      (should (null (writing-habit-compare-week-prospecting
                     db writing-habit-prospecting-tests--week))))))

(ert-deftest writing-habit-prospecting/ranges-off-and-legend-scope ()
  "An empty range list or the legend scope labels nothing."
  (skip-unless (sqlite-available-p))
  (writing-habit-prospecting-tests--with (db src _table)
    (should (= 0 (writing-habit-sync-prospecting
                  (writing-habit-track-sync-mytime db src :prospect-ranges ""))))
    (should (= 0 (writing-habit-sync-prospecting
                  (writing-habit-track-sync-mytime db src :scope "legend"))))
    (should-not (assoc "2" (writing-habit-prospecting-tests--flags db)))))

(ert-deftest writing-habit-prospecting/scope-all-still-labels ()
  "Scope all keeps everything and still labels unlisted writing."
  (skip-unless (sqlite-available-p))
  (writing-habit-prospecting-tests--with (db src _table)
    (writing-habit-track-sync-mytime db src :scope "all")
    (let ((flags (writing-habit-prospecting-tests--flags db)))
      (should (= 1 (nth 2 (assoc "2" flags))))
      (should (= 0 (nth 2 (assoc "4" flags)))))))

(ert-deftest writing-habit-prospecting/report-section ()
  "The org report gains a risky-prospecting section."
  (skip-unless (sqlite-available-p))
  (writing-habit-prospecting-tests--with (db src _table)
    (writing-habit-track-sync-mytime db src)
    (let ((text (writing-habit-report-week-string db writing-habit-prospecting-tests--week)))
      (should (string-match-p "^\\* Risky prospecting$" text))
      (should (string-match-p "| 1424AIseededPhasing +| +2 +| +90 +| +1 +|" text))
      (should (string-match-p "90 minutes, 30% of the week's tracked writing" text))
      ;; The report rounds half to even, as its other percents do, so 62.5 reads 62.
      (should (string-match-p "the risky share of actual time is 62%" text)))))

(ert-deftest writing-habit-prospecting/command-line-range ()
  "The track sync command reads --prospect-range, and none turns the label off."
  (skip-unless (sqlite-available-p))
  (require 'writing-habit)
  (writing-habit-prospecting-tests--with (db src _table)
    (let ((path (make-temp-file "wh-prospect-cli" nil ".db")))
      (unwind-protect
          (let ((cli (writing-habit-db-connect path)))
            (writing-habit-db-init cli writing-habit-prospecting-tests--schema)
            (sqlite-execute cli (concat "INSERT INTO plan_week(week_start, schedule_code,"
                                        " table_path) SELECT ?, ?, ?")
                            (car (sqlite-select db (concat "SELECT week_start, schedule_code,"
                                                           " table_path FROM plan_week"))))
            (writing-habit-db-close cli)
            (should-not (string-match-p
                         "risky prospecting"
                         (writing-habit--dispatch (list "track" "sync" src "--db" path
                                                        "--prospect-range" "none"))))
            (should (string-match-p
                     "kept 2 on no list as risky prospecting"
                     (writing-habit--dispatch (list "track" "sync" src "--db" path)))))
        (delete-file path)))))

(ert-deftest writing-habit-prospecting/old-database-is-migrated ()
  "A database made before the label gains the column and rebuilt views."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-db-connect)))
    (unwind-protect
        (progn
          (sqlite-execute db (concat "CREATE TABLE project (project_id INTEGER PRIMARY KEY,"
                                     " code TEXT NOT NULL UNIQUE, description TEXT,"
                                     " risk_class TEXT, active INTEGER NOT NULL DEFAULT 1,"
                                     " created_at TEXT NOT NULL DEFAULT (datetime('now')))"))
          (sqlite-execute db (concat "CREATE TABLE session (session_id INTEGER PRIMARY KEY,"
                                     " day TEXT NOT NULL, start_time TEXT, end_time TEXT,"
                                     " actual_min INTEGER NOT NULL, project_id INTEGER NOT NULL,"
                                     " category_id INTEGER, source TEXT NOT NULL,"
                                     " source_ref TEXT, note TEXT,"
                                     " created_at TEXT NOT NULL DEFAULT (datetime('now')),"
                                     " week_start TEXT GENERATED ALWAYS AS (date(day, '-' ||"
                                     " ((CAST(strftime('%w', day) AS INTEGER) + 6) % 7)"
                                     " || ' days')) VIRTUAL)"))
          (sqlite-execute db (concat "CREATE VIEW v_week_overall AS SELECT week_start,"
                                     " 0 AS planned_min, SUM(actual_min) AS actual_min,"
                                     " NULL AS adherence FROM session GROUP BY week_start"))
          (sqlite-execute db "INSERT INTO project(code) VALUES ('A')")
          (sqlite-execute db (concat "INSERT INTO session(day, actual_min, project_id, source)"
                                     " VALUES ('2026-10-05', 30, 1, 'manual')"))
          (should-not (writing-habit-db-has-prospecting db))
          (should (null (writing-habit-compare-week-prospecting db "2026-10-05")))
          (writing-habit-db-init db writing-habit-prospecting-tests--schema)
          (should (writing-habit-db-has-prospecting db))
          (should (equal (sqlite-select db "SELECT prospecting FROM session") '((0))))
          (should (string-match-p "prospecting = 0"
                                  (caar (sqlite-select
                                         db (concat "SELECT sql FROM sqlite_master"
                                                    " WHERE name = 'v_week_overall'")))))
          (should-not (writing-habit-db-migrate db)))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-prospecting/matches-cross-port-fixture ()
  "The prospecting dashboard is byte-identical to the Python render."
  (skip-unless (sqlite-available-p))
  (let ((path (expand-file-name "fixtures/prospect.db" writing-habit-prospecting-tests--dir))
        (html (expand-file-name "fixtures/prospect.html" writing-habit-prospecting-tests--dir)))
    (skip-unless (file-exists-p path))
    (let ((db (writing-habit-db-connect path)))
      (unwind-protect
          (should (string= (writing-habit-dashboard-html db "2026-10-19" "2026-10-26")
                           (with-temp-buffer
                             (insert-file-contents html)
                             (buffer-string))))
        (writing-habit-db-close db)))))

(provide 'writing-habit-prospecting-tests)
;;; writing-habit-prospecting-tests.el ends here
