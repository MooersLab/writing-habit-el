;;; writing-habit-track-tests.el --- ERT tests for writing-habit-track -*- lexical-binding: t; -*-

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

(provide 'writing-habit-track-tests)
;;; writing-habit-track-tests.el ends here
