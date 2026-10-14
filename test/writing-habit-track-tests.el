;;; writing-habit-track-tests.el --- ERT tests for writing-habit-track -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ports the track behavior of the Python package to ERT and covers the
;; org-clock harvest.  Run from the repository root with:
;;
;;   emacs -batch -L elisp -l ert \
;;     -l elisp/test/writing-habit-track-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-db)
(require 'writing-habit-track)

(defconst writing-habit-track-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst writing-habit-track-tests--root
  (locate-dominating-file writing-habit-track-tests--dir "schema.sql")
  "Repository root, found by walking up to schema.sql, so tests work in any layout.")

(defconst writing-habit-track-tests--schema
  (expand-file-name "schema.sql" writing-habit-track-tests--root))

(defconst writing-habit-track-tests--csv
  (expand-file-name "examples/actuals.csv" writing-habit-track-tests--root))

(defconst writing-habit-track-tests--ics
  (expand-file-name "examples/actuals.ics" writing-habit-track-tests--root))

(defun writing-habit-track-tests--fresh ()
  "Return a fresh in-memory database with the schema loaded."
  (let ((db (writing-habit-db-connect)))
    (writing-habit-db-init db writing-habit-track-tests--schema)
    db))


;;;; CSV parser

(ert-deftest writing-habit-track-parse-csv-quoting ()
  "Quoted fields with commas and doubled quotes parse correctly."
  (let ((rows (writing-habit-track--parse-csv
               "a,b,c\n1,\"x,y\",\"he said \"\"hi\"\"\"\n")))
    (should (equal (nth 0 rows) '("a" "b" "c")))
    (should (equal (nth 1 rows) '("1" "x,y" "he said \"hi\"")))))

(ert-deftest writing-habit-track-parse-csv-no-trailing-newline ()
  "A final row without a trailing newline is still returned."
  (let ((rows (writing-habit-track--parse-csv "a,b\n1,2")))
    (should (= (length rows) 2))
    (should (equal (nth 1 rows) '("1" "2")))))


;;;; Manual entry

(ert-deftest writing-habit-track-add-minutes-and-times ()
  "Times give computed minutes; a minutes value is stored as is."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-track-tests--fresh)))
    (unwind-protect
        (progn
          (should (integerp (writing-habit-track-add
                             db :day "2026-01-19" :project "A"
                             :start "04:00" :end "05:30" :category "generative")))
          (let ((row (car (sqlite-select
                           db "SELECT actual_min, source FROM session WHERE start_time='04:00'"))))
            (should (= (nth 0 row) 90))
            (should (equal (nth 1 row) "manual")))
          ;; The returned id must point at the row just inserted, not at the
          ;; provenance row that logging writes afterward.
          (let ((id (writing-habit-track-add db :day "2026-01-19" :project "EM"
                                             :minutes 45 :category "support")))
            (should (= 45 (caar (sqlite-select
                                 db "SELECT actual_min FROM session WHERE session_id = ?"
                                 (list id)))))
            (should (= id (caar (sqlite-select db "SELECT MAX(session_id) FROM session"))))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-track-add-errors ()
  "Neither minutes nor times fails; an unknown category fails."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-track-tests--fresh)))
    (unwind-protect
        (progn
          (should-error (writing-habit-track-add db :day "2026-01-19" :project "A"))
          (should-error (writing-habit-track-add
                         db :day "2026-01-19" :project "A" :minutes 30 :category "bogus")))
      (writing-habit-db-close db))))


;;;; CSV import

(ert-deftest writing-habit-track-import-csv-counts ()
  "Every data row becomes a session, from times or from minutes."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-track-tests--fresh)))
    (unwind-protect
        (let ((n (writing-habit-track-import-csv db writing-habit-track-tests--csv)))
          (should (= n 7))
          ;; A start/end row gives computed minutes; source csv; source_ref path:line.
          (let ((row (car (sqlite-select
                           db (concat "SELECT actual_min, source, source_ref FROM session "
                                      "WHERE start_time='04:05' AND day='2026-01-19'")))))
            (should (= (nth 0 row) 85))
            (should (equal (nth 1 row) "csv"))
            (should (string-suffix-p ":2" (nth 2 row))))
          ;; The minutes-only row keeps null times.
          (should (= 30 (caar (sqlite-select
                               db (concat "SELECT actual_min FROM session "
                                          "WHERE start_time IS NULL AND project_id="
                                          "(SELECT project_id FROM project WHERE code='EM')"))))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-track-import-csv-requires-code ()
  "A data row with no project_code is an error."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-track-tests--fresh))
        (csv (make-temp-file
              "wh-bad" nil ".csv"
              (concat "date,start,end,minutes,project_code,category,note\n"
                      "2026-01-19,04:00,05:30,,,generative,missing code\n"))))
    (unwind-protect
        (should-error (writing-habit-track-import-csv db csv))
      (writing-habit-db-close db)
      (delete-file csv))))


;;;; Org-clock harvest

(ert-deftest writing-habit-track-harvest-clock-basic ()
  "Clocks with a project code become sessions; a code-less clock is skipped."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-track-tests--fresh))
        (org (make-temp-file
              "wh-clock" nil ".org"
              (concat
               "* Tasks\n"
               "** [A] DNPH1 draft :generative:\n"
               ":LOGBOOK:\n"
               "CLOCK: [2026-01-19 Mon 04:00]--[2026-01-19 Mon 05:30] =>  1:30\n"
               ":END:\n"
               "** [EM] inbox :support:\n"
               ":LOGBOOK:\n"
               "CLOCK: [2026-01-19 Mon 13:15]--[2026-01-19 Mon 13:45] =>  0:30\n"
               ":END:\n"
               "** meeting with no code\n"
               ":LOGBOOK:\n"
               "CLOCK: [2026-01-19 Mon 15:00]--[2026-01-19 Mon 16:00] =>  1:00\n"
               ":END:\n"))))
    (unwind-protect
        (progn
          (let ((n (writing-habit-track-harvest-clock db org)))
            (should (= n 2)))               ; the code-less clock is skipped
          (let ((row (car (sqlite-select
                           db (concat "SELECT p.code, c.name, s.actual_min, s.source, s.source_ref "
                                      "FROM session s JOIN project p ON p.project_id=s.project_id "
                                      "LEFT JOIN category c ON c.category_id=s.category_id "
                                      "WHERE p.code='A'")))))
            (should (equal (nth 0 row) "A"))
            (should (equal (nth 1 row) "generative"))
            (should (= (nth 2 row) 90))
            (should (equal (nth 3 row) "manual"))
            (should (string-prefix-p "orgclock:" (nth 4 row))))
          ;; Harvesting the same file again inserts nothing.
          (should (= 0 (writing-habit-track-harvest-clock db org))))
      (writing-habit-db-close db)
      (delete-file org))))

(ert-deftest writing-habit-track-harvest-clock-default-category ()
  "A clock with no activity tag takes the default category."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-track-tests--fresh))
        (org (make-temp-file
              "wh-clock2" nil ".org"
              (concat
               "* [B] DUSP1\n"
               ":LOGBOOK:\n"
               "CLOCK: [2026-01-20 Tue 06:00]--[2026-01-20 Tue 07:30] =>  1:30\n"
               ":END:\n"))))
    (unwind-protect
        (progn
          (writing-habit-track-harvest-clock db org "generative")
          (should (equal (caar (sqlite-select
                                db (concat "SELECT c.name FROM session s "
                                           "JOIN category c ON c.category_id=s.category_id "
                                           "WHERE s.project_id="
                                           "(SELECT project_id FROM project WHERE code='B')")))
                         "generative")))
      (writing-habit-db-close db)
      (delete-file org))))

;;;; ICS import

(ert-deftest writing-habit-track-import-ics-counts ()
  "Events with a bracketed code become sessions; a code-less event is skipped."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-track-tests--fresh)))
    (unwind-protect
        (let ((n (writing-habit-track-import-ics db writing-habit-track-tests--ics)))
          (should (= n 4))                ; five events, one without a code
          ;; The first A block: 04:05-05:30 is 85 minutes, source ics, uid ref.
          (let ((row (car (sqlite-select
                           db (concat "SELECT s.day, s.start_time, s.end_time, s.actual_min, "
                                      "c.name, s.source, s.source_ref FROM session s "
                                      "JOIN category c ON c.category_id=s.category_id "
                                      "WHERE s.start_time='04:05' AND s.day='2026-01-19'")))))
            (should (equal (nth 0 row) "2026-01-19"))
            (should (equal (nth 1 row) "04:05"))
            (should (equal (nth 2 row) "05:30"))
            (should (= (nth 3 row) 85))
            (should (equal (nth 4 row) "generative"))
            (should (equal (nth 5 row) "ics"))
            (should (equal (nth 6 row) "wh-2026-01-19-a1@example"))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-track-import-ics-day-is-literal ()
  "The stored day and time are the calendar's wall clock, with no zone shift."
  (skip-unless (sqlite-available-p))
  ;; Force a non-UTC zone to prove the literal wall clock is kept.
  (let ((db (writing-habit-track-tests--fresh))
        (process-environment (cons "TZ=America/Chicago" process-environment)))
    (unwind-protect
        (progn
          (writing-habit-track-import-ics db writing-habit-track-tests--ics)
          ;; The 04:00 Tuesday B block stays on 2026-01-20 at 04:00.
          (should (equal (caar (sqlite-select
                                db (concat "SELECT day FROM session "
                                           "WHERE start_time='04:00' AND source='ics'")))
                         "2026-01-20")))
      (writing-habit-db-close db))))


;;;; mytime.db sync

(defconst writing-habit-track-tests--legend
  (concat
   "| Time <l>    | M  | Tu |\n"
   "|-------------+----+----|\n"
   "| 05:30-07:00 | gZ |    |\n"
   "|-------------+----+----|\n"
   "| F: 4072UsersMeeting2026 Sep 16, 0382CCinJN :safe: | | |\n"
   "| H: 0104BaGquadShelxD November 15 :safe:          | | |\n"
   "| Z: 1008ToolsForQCrOfRNA, submitted October 5 :safe: | | |\n")
  "A week's legend, the same one the Python sync tests use.")

(defconst writing-habit-track-tests--rows
  '((1 "2026-10-05" "05:30" "07:00" 1008 "1008ToolsForQCrOfRNA" "grant text" "")
    (2 "2026-10-05" "08:00" "09:00" 104 "0104BaGquadShelxD" "figures" "E")
    (3 "2026-10-06" "3:45" "05:00" 382 "0382CCinJN" "" "sw")
    (4 "2026-10-06" "06:00" "06:30" 6005 "6005EveningCommute" "" "none")
    (5 "2026-10-06" "" "" "" "" "" "")
    (6 "2026-10-07" "21:00" "00:00" 104 "0104BaGquadShelxD" "late" "G")
    (7 "2026-09-28" "05:00" "06:00" 1008 "1008ToolsForQCrOfRNA" "before" "g"))
  "zTimeSpent rows: id, date, start, end, ProjectID, directory, note, activity.")

(defun writing-habit-track-tests--mytime (path rows)
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

(defmacro writing-habit-track-tests--with-sync (vars &rest body)
  "Run BODY with VARS (DB SOURCE TABLE DIR) bound to a planned week."
  (declare (indent 1))
  (let ((db (nth 0 vars)) (source (nth 1 vars)) (table (nth 2 vars)) (dir (nth 3 vars)))
    `(let* ((,dir (make-temp-file "wh-sync" t))
            (,table (expand-file-name "Oct5.org" ,dir))
            (,source (expand-file-name "mytime.db" ,dir))
            (,db (writing-habit-track-tests--fresh)))
       (unwind-protect
           (progn
             (with-temp-file ,table (insert writing-habit-track-tests--legend))
             (writing-habit-track-tests--mytime ,source writing-habit-track-tests--rows)
             (sqlite-execute ,db (concat "INSERT INTO plan_week(week_start, schedule_code,"
                                         " table_path) VALUES (?,?,?)")
                             (list "2026-10-05" "Oct5" ,table))
             (let ((pid (writing-habit-db-get-or-create-project
                         ,db "Z" "1008ToolsForQCrOfRNA" "safe")))
               (sqlite-execute ,db (concat "INSERT INTO plan_block(day, start_time, end_time,"
                                           " project_id, category_id) VALUES"
                                           " ('2026-10-05', '05:30', '07:00', ?, ?)")
                               (list pid (writing-habit-db-get-category-id ,db "generative"))))
             ,@body)
         (writing-habit-db-close ,db)
         (delete-directory ,dir t)))))

(defun writing-habit-track-tests--sessions (db)
  "Return an alist of mytime id to (CODE CATEGORY START MINUTES) in DB."
  (mapcar (lambda (r)
            (cons (car (last (split-string (nth 0 r) ":")))
                  (cdr r)))
          (sqlite-select
           db (concat "SELECT s.source_ref, p.code, c.name, s.start_time, s.actual_min"
                      " FROM session s JOIN project p USING(project_id)"
                      " LEFT JOIN category c USING(category_id) ORDER BY s.source_ref"))))

(ert-deftest writing-habit-track-sync/legend-numbers ()
  "Every project number in a legend description maps to its code."
  (writing-habit-track-tests--with-sync (_db _src table _dir)
    (should (equal (writing-habit-track-legend-numbers table)
                   '(("4072" . "F") ("0382" . "F") ("0104" . "H") ("1008" . "Z"))))))

(ert-deftest writing-habit-track-sync/maps-numbers-and-activities ()
  "Numbers become codes, tags become activities, and late work wraps midnight."
  (writing-habit-track-tests--with-sync (db src _table _dir)
    (let* ((report (writing-habit-track-sync-mytime db src))
           (rows (writing-habit-track-tests--sessions db)))
      (should (equal (sort (mapcar #'car rows) #'string<) '("1" "2" "3" "6")))
      (should (equal (cdr (assoc "1" rows)) '("Z" "generative" "05:30" 90)))
      (should (equal (cdr (assoc "2" rows)) '("H" "editing" "08:00" 60)))
      (should (equal (cdr (assoc "3" rows)) '("F" "support" "03:45" 75)))
      (should (= (nth 4 (assoc "6" rows)) 180))
      (should (= (writing-habit-sync-inserted report) 4))
      (should (= (writing-habit-sync-skipped-not-in-legend report) 1))
      (should (= (writing-habit-sync-skipped-no-time report) 1))
      (should (equal (writing-habit-sync-left-out-minutes report)
                     '(("6005EveningCommute" . 30)))))))

(ert-deftest writing-habit-track-sync/rerun-is-idempotent ()
  "A rerun changes nothing, and an edit in the tracker updates its row."
  (writing-habit-track-tests--with-sync (db src _table _dir)
    (writing-habit-track-sync-mytime db src)
    (let ((again (writing-habit-track-sync-mytime db src)))
      (should (equal (list (writing-habit-sync-inserted again)
                           (writing-habit-sync-updated again)
                           (writing-habit-sync-unchanged again))
                     '(0 0 4))))
    (let ((s (sqlite-open src)))
      (sqlite-execute s "UPDATE zTimeSpent SET \"End\" = '09:30' WHERE id = 2")
      (sqlite-close s))
    (should (= (writing-habit-sync-updated
                (writing-habit-track-sync-mytime db src))
               1))
    (should (= (nth 4 (assoc "2" (writing-habit-track-tests--sessions db))) 90))
    (should (= (caar (sqlite-select db "SELECT COUNT(*) FROM session")) 4))))

(ert-deftest writing-habit-track-sync/scope-all ()
  "Scope all keeps a project the legend does not name, under its directory."
  (writing-habit-track-tests--with-sync (db src _table _dir)
    (writing-habit-track-sync-mytime db src :scope "all")
    (should (equal (nth 1 (assoc "4" (writing-habit-track-tests--sessions db)))
                   "6005EveningCommute"))))

(ert-deftest writing-habit-track-sync/semester-list ()
  "Scope semester keeps a project on the current list as unplanned work."
  (writing-habit-track-tests--with-sync (db src table _dir)
    (with-temp-buffer
      (insert-file-contents table)
      (goto-char (point-max))
      (insert "\n#+NAME: current-semester-projects\n| -: 6005EveningCommute |\n")
      (write-region (point-min) (point-max) table))
    (let ((report (writing-habit-track-sync-mytime db src)))
      (should (= (writing-habit-sync-semester-unplanned report) 1))
      (should (equal (nth 1 (assoc "4" (writing-habit-track-tests--sessions db)))
                     "6005EveningCommute")))
    (should-not (assoc "4" (progn (sqlite-execute db "DELETE FROM session")
                                  (writing-habit-track-sync-mytime db src :scope "legend")
                                  (writing-habit-track-tests--sessions db))))))

(ert-deftest writing-habit-track-sync/dry-run ()
  "A dry run counts the rows and writes nothing."
  (writing-habit-track-tests--with-sync (db src _table _dir)
    (let ((report (writing-habit-track-sync-mytime db src :dry-run t)))
      (should (= (writing-habit-sync-inserted report) 4))
      (should (= (caar (sqlite-select db "SELECT COUNT(*) FROM session")) 0))
      (should (= (caar (sqlite-select db "SELECT COUNT(*) FROM project")) 1)))))

(ert-deftest writing-habit-track-sync/since-and-table-fallback ()
  "A week with no plan borrows the legend of the named table."
  (writing-habit-track-tests--with-sync (db src table _dir)
    (let ((report (writing-habit-track-sync-mytime
                   db src :since "2026-09-28" :until "2026-10-05" :table table)))
      (should (equal (sort (mapcar #'car (writing-habit-track-tests--sessions db)) #'string<)
                     '("1" "2" "7")))
      (should (= (writing-habit-sync-inserted report) 3))
      (should (equal (writing-habit-sync-weeks-without-plan report)
                     '("2026-09-28"))))))

(ert-deftest writing-habit-track-sync/missing-source ()
  "A missing mytime.db is a clear error."
  (writing-habit-track-tests--with-sync (db _src _table dir)
    (should-error (writing-habit-track-sync-mytime db (expand-file-name "nope.db" dir)))))

(ert-deftest writing-habit-track-sync/report-text ()
  "The report names the counts and the projects left out."
  (writing-habit-track-tests--with-sync (db src _table _dir)
    (let ((text (writing-habit-track-sync-report-string
                 (writing-habit-track-sync-mytime db src) src "2026-10-05" nil nil)))
      (should (string-match-p "inserted 4, updated 0, unchanged 0" text))
      (should (string-match-p "    6005EveningCommute +30$" text)))))

(provide 'writing-habit-track-tests)
;;; writing-habit-track-tests.el ends here
