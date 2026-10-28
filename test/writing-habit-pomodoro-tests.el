;;; writing-habit-pomodoro-tests.el --- ERT tests for the Pomodoro timer -*- lexical-binding: t; -*-

;;; Commentary:

;; The cases of the Python tests test_pomodoro.py for the cycle, the clock,
;; the day's count, and the daily record, with the same inputs and the same
;; expected text.  No test plays a sound or touches the real mytime.db.

;;; Code:

(require 'ert)
(require 'writing-habit-pomodoro)

(defmacro writing-habit-pomodoro-tests--with (vars &rest body)
  "Run BODY with fresh settings, a scratch folder, and stand-ins for the world.
VARS is (DIR LINES DAY CLOCK): the folder, the logged lines, the day, and
the clock in seconds, each a cons cell whose car a test may set."
  (declare (indent 1))
  (let ((dir (nth 0 vars)) (lines (nth 1 vars)) (day (nth 2 vars)) (now (nth 3 vars)))
    `(let* ((,dir (make-temp-file "wh-pom" t))
            (,lines (list nil))
            (,day (list "2026-10-21"))
            (,now (list 0.0))
            (writing-habit-pomodoro-minutes 25) (writing-habit-pomodoro-short-break 5)
            (writing-habit-pomodoro-long-break 15) (writing-habit-pomodoro-per-cycle 4)
            (writing-habit-pomodoro-auto-cycle t) (writing-habit-pomodoro-auto-next t)
            (writing-habit-pomodoro-tick t) (writing-habit-pomodoro-tick-in-breaks nil)
            (writing-habit-pomodoro-buzzer t)
            (writing-habit-pomodoro-record-db t)
            (writing-habit-pomodoro-db-dir ,dir)
            (writing-habit-pomodoro-db-file "mytime.db")
            (writing-habit-pomodoro-db-table "zzPomCount")
            (writing-habit-pomodoro-state-file (expand-file-name "state.eld" ,dir))
            (writing-habit-pomodoro-clock (writing-habit-pomodoro-clock-create))
            (writing-habit-pomodoro-block-function (lambda () nil))
            (writing-habit-pomodoro-now-function (lambda () (car ,now)))
            (writing-habit-pomodoro-today-function (lambda () (car ,day)))
            (writing-habit-pomodoro-wall-function (lambda () (concat (car ,day) "T23:59:00")))
            (writing-habit-pomodoro-play-function (lambda (name _v) (push (format "play %s" name) (car ,lines))))
            (writing-habit-pomodoro-log-function (lambda (text) (push text (car ,lines)))))
       (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) nil)))
         (unwind-protect (progn ,@body)
           (delete-directory ,dir t))))))

(defun writing-habit-pomodoro-tests--logged (lines)
  "Return the logged LINES oldest first, without the sounds."
  (seq-remove (lambda (l) (string-prefix-p "play " l)) (reverse (car lines))))

;;;; The cycle

(ert-deftest writing-habit-pomodoro/the-block-sets-the-pomodoros-per-cycle ()
  (dolist (case '((120 . 4) (115 . 4) (114 . 3) (90 . 3) (60 . 2) (55 . 2) (45 . 1) (30 . 1) (15 . 1)))
    (should (= (writing-habit-pomodoro-fit-in-block (car case) 25 5) (cdr case)))))

(ert-deftest writing-habit-pomodoro/no-block-gives-the-default ()
  (should (= (writing-habit-pomodoro-fit-in-block nil 25 5) 4))
  (should (= (writing-habit-pomodoro-fit-in-block 120 50 10) 2))
  (should (= (writing-habit-pomodoro-block-minutes '("23:00" . "01:00")) 120)))

(ert-deftest writing-habit-pomodoro/a-fixed-number-overrides-the-block ()
  (let ((writing-habit-pomodoro-auto-cycle nil) (writing-habit-pomodoro-per-cycle 6))
    (should (= (writing-habit-pomodoro-cycle-length '("05:00" . "07:00")) 6)))
  (let ((writing-habit-pomodoro-auto-cycle t))
    (should (= (writing-habit-pomodoro-cycle-length '("05:00" . "06:30")) 3))))

(ert-deftest writing-habit-pomodoro/a-cycle-alternates-and-ends-with-the-long-break ()
  (let ((writing-habit-pomodoro-minutes 25) (writing-habit-pomodoro-short-break 5)
        (writing-habit-pomodoro-long-break 15))
    (should (equal (mapcar (lambda (p) (cons (writing-habit-pomodoro-phase-kind p)
                                             (writing-habit-pomodoro-phase-minutes p)))
                           (writing-habit-pomodoro-build-cycle 3))
                   '((pomodoro . 25) (short . 5) (pomodoro . 25) (short . 5) (pomodoro . 25) (long . 15))))))

;;;; The clock

(ert-deftest writing-habit-pomodoro/the-clock-runs-a-cycle-by-itself ()
  (writing-habit-pomodoro-tests--with (_dir _lines _day _now)
    (let ((clock (writing-habit-pomodoro-clock-create)))
      (writing-habit-pomodoro-clock-start clock 0 '("05:00" . "06:00"))
      (should (= (writing-habit-pomodoro-clock-count clock) 2))
      (should (equal (writing-habit-pomodoro-clock-text clock 0) "Pomodoro 1/2  25:00"))
      (should (equal (writing-habit-pomodoro-clock-text clock 61.5) "Pomodoro 1/2  23:59"))
      (should-not (writing-habit-pomodoro-clock-poll clock 1499))
      (should (eq (writing-habit-pomodoro-phase-kind (writing-habit-pomodoro-clock-poll clock 1500)) 'pomodoro))
      (should (equal (writing-habit-pomodoro-clock-text clock 1500) "Short break  05:00"))
      ;; A late read does not stretch the cycle: the break began at 1500.
      (should (eq (writing-habit-pomodoro-phase-kind (writing-habit-pomodoro-clock-poll clock 1830)) 'short))
      (should (equal (writing-habit-pomodoro-clock-text clock 1830) "Pomodoro 2/2  24:30"))
      (should (eq (writing-habit-pomodoro-phase-kind (writing-habit-pomodoro-clock-poll clock 3300)) 'pomodoro))
      (should (eq (writing-habit-pomodoro-phase-kind (writing-habit-pomodoro-clock-phase clock)) 'long))
      (should (eq (writing-habit-pomodoro-phase-kind (writing-habit-pomodoro-clock-poll clock 4200)) 'long))
      (should (eq (writing-habit-pomodoro-clock-state clock) 'done))
      (should (= (writing-habit-pomodoro-clock-completed clock) 2))
      (should (equal (writing-habit-pomodoro-clock-text clock 4200) "Cycle done, 2 Pomodoros")))))

(ert-deftest writing-habit-pomodoro/pause-resume-and-stop ()
  (writing-habit-pomodoro-tests--with (_dir _lines _day _now)
    (let ((clock (writing-habit-pomodoro-clock-create)))
      (writing-habit-pomodoro-clock-start clock 0)
      (writing-habit-pomodoro-clock-pause clock 600)
      (should (eq (writing-habit-pomodoro-clock-state clock) 'paused))
      (should (equal (writing-habit-pomodoro-clock-text clock 5000) "Pomodoro 1/4  15:00 paused"))
      (should-not (writing-habit-pomodoro-clock-poll clock 5000))
      (writing-habit-pomodoro-clock-start clock 5000)
      (should (= (writing-habit-pomodoro-clock-remaining clock 5100) 800))
      (writing-habit-pomodoro-clock-stop clock)
      (should (equal (writing-habit-pomodoro-clock-text clock 0) "Pomodoro")))))

(ert-deftest writing-habit-pomodoro/without-automatic-starts-the-next-phase-waits ()
  (writing-habit-pomodoro-tests--with (_dir _lines _day _now)
    (let ((writing-habit-pomodoro-auto-next nil)
          (clock (writing-habit-pomodoro-clock-create)))
      (writing-habit-pomodoro-clock-start clock 0)
      (writing-habit-pomodoro-clock-poll clock 1500)
      (should (eq (writing-habit-pomodoro-clock-state clock) 'waiting))
      (should (equal (writing-habit-pomodoro-clock-text clock 9999) "Short break ready. Press Start"))
      (writing-habit-pomodoro-clock-start clock 2000)
      (should (equal (writing-habit-pomodoro-clock-text clock 2000) "Short break  05:00")))))

;;;; The timer

(ert-deftest writing-habit-pomodoro/the-timer-ticks-buzzes-and-counts ()
  (writing-habit-pomodoro-tests--with (_dir lines _day now)
    (writing-habit-pomodoro-start)
    (should (equal (writing-habit-pomodoro-tests--logged lines) '("Pomodoro cycle of 4 started")))
    (setcar now 10.0)
    (writing-habit-pomodoro-update)
    (should (equal (car (car lines)) "play tick"))
    (setcar now 1500.0)
    (should (equal (writing-habit-pomodoro-update) "Short break  05:00"))
    (should (member "play buzzer" (car lines)))
    (should (equal (car (last (writing-habit-pomodoro-tests--logged lines)))
                   "Pomodoro 1/4 ended. Short break started"))
    (should (= (writing-habit-pomodoro-today-count "2026-10-21") 1))
    (should (string-match-p "\\`\\[Short break  05:00  Today: 1\\]\\'"
                            (substring-no-properties (writing-habit-pomodoro-mode-line))))
    (should (eq (get-text-property 1 'face (writing-habit-pomodoro-mode-line)) 'writing-habit-pomodoro-break))
    ;; No tick in a break unless asked.
    (setq lines (list nil))
    (setcar now 1510.0)
    (writing-habit-pomodoro-update)
    (should-not (member "play tick" (car lines)))
    (writing-habit-pomodoro-start)                 ; pause
    (should (eq (writing-habit-pomodoro-clock-state writing-habit-pomodoro-clock) 'paused))
    (writing-habit-pomodoro-stop)
    (should (equal (car (last (writing-habit-pomodoro-tests--logged lines)))
                   "Pomodoro stopped after 1 Pomodoros"))
    (writing-habit-pomodoro-mode -1)))

(ert-deftest writing-habit-pomodoro/the-sounds-can-be-turned-off ()
  (writing-habit-pomodoro-tests--with (_dir lines _day now)
    (let ((writing-habit-pomodoro-tick nil) (writing-habit-pomodoro-buzzer nil))
      (writing-habit-pomodoro-start)
      (setcar now 1500.0)
      (writing-habit-pomodoro-update)
      (should-not (seq-find (lambda (l) (string-prefix-p "play " l)) (car lines))))
    (writing-habit-pomodoro-mode -1)))

(ert-deftest writing-habit-pomodoro/a-new-cycle-starts-when-the-long-break-ends-in-a-block ()
  (writing-habit-pomodoro-tests--with (_dir lines _day now)
    (let ((writing-habit-pomodoro-block-function (lambda () '("05:00" . "05:30"))))
      (writing-habit-pomodoro-start)
      (should (= (writing-habit-pomodoro-clock-count writing-habit-pomodoro-clock) 1))
      (setcar now 1500.0) (writing-habit-pomodoro-update)
      (setcar now 2400.0) (writing-habit-pomodoro-update)
      (should (equal (car (last (writing-habit-pomodoro-tests--logged lines)))
                     "Pomodoro cycle done: 1 Pomodoros. Next cycle of 1 started"))
      (should (eq (writing-habit-pomodoro-clock-state writing-habit-pomodoro-clock) 'running)))
    (writing-habit-pomodoro-mode -1)))

(ert-deftest writing-habit-pomodoro/the-count-starts-over-the-next-day ()
  (writing-habit-pomodoro-tests--with (_dir _lines day _now)
    (should (= (writing-habit-pomodoro-add-to-today "2026-10-21") 1))
    (should (= (writing-habit-pomodoro-add-to-today "2026-10-21") 2))
    (should (= (writing-habit-pomodoro-today-count "2026-10-22") 0))
    (setcar day "2026-10-22")
    (with-temp-file writing-habit-pomodoro-state-file (insert "garbage ("))
    (should (= (writing-habit-pomodoro-today-count "2026-10-22") 0))))

;;;; The daily record

(defun writing-habit-pomodoro-tests--path (dir)
  "Return the mytime.db of DIR."
  (expand-file-name "mytime.db" dir))

(ert-deftest writing-habit-pomodoro/record-adds-one-row-per-day-and-updates-it ()
  (writing-habit-pomodoro-tests--with (dir _lines _day _now)
    (let ((path (writing-habit-pomodoro-tests--path dir)))
      (should (eq (writing-habit-pomodoro-record path "zzPomCount" "2026-10-21" 6 "2026-10-21T23:59:00") 'added))
      (should (eq (writing-habit-pomodoro-record path "zzPomCount" "2026-10-21" 7) 'updated))
      (writing-habit-pomodoro-record path "zzPomCount" "2026-10-22" 0)
      (should (equal (writing-habit-pomodoro-rows path "zzPomCount") '(("2026-10-21" . 7) ("2026-10-22" . 0)))))))

(ert-deftest writing-habit-pomodoro/record-refuses-a-bad-table-or-folder ()
  (writing-habit-pomodoro-tests--with (dir _lines _day _now)
    (should-error (writing-habit-pomodoro-record (expand-file-name "m.db" dir) "zz; DROP TABLE x" "2026-10-21" 1))
    (should-error (writing-habit-pomodoro-record (expand-file-name "nowhere/m.db" dir) "zzPomCount" "2026-10-21" 1))
    (should-not (file-exists-p (expand-file-name "nowhere" dir)))
    (should (equal (writing-habit-pomodoro-database-path "~/x" "") (expand-file-name "~/x/mytime.db")))))

(ert-deftest writing-habit-pomodoro/the-time-tracker-table-is-never-touched ()
  (writing-habit-pomodoro-tests--with (dir _lines _day _now)
    (let* ((path (writing-habit-pomodoro-tests--path dir))
           (db (sqlite-open path)))
      (sqlite-execute db "CREATE TABLE \"zTimeSpent\" (\"id\" INTEGER NOT NULL, \"DateDashed\" Date, \"Start\" TIME, \"End\" TIME, \"TimeHr\" TIME GENERATED ALWAYS AS (ROUND((julianday(\"End\") - julianday(\"Start\")) * 24, 2)) VIRTUAL, \"ProjectID\" INT, PRIMARY KEY(\"id\"))")
      (sqlite-execute db "INSERT INTO zTimeSpent (id, DateDashed, Start, \"End\", ProjectID) VALUES (1, '2026-10-21', '05:30', '07:00', 142)")
      (let ((before (caar (sqlite-select db "SELECT sql FROM sqlite_master WHERE name = 'zTimeSpent'"))))
        (sqlite-close db)
        (writing-habit-pomodoro-record path "zzPomCount" "2026-10-21" 6)
        (dolist (name '("zTimeSpent" "ztimespent" "ZTIMESPENT"))
          (should (string-match-p "time tracker's own table"
                                  (cadr (should-error (writing-habit-pomodoro-record path name "2026-10-21" 6))))))
        (let ((db (sqlite-open path)))
          (should (equal (caar (sqlite-select db "SELECT sql FROM sqlite_master WHERE name = 'zTimeSpent'")) before))
          (should (equal (caar (sqlite-select db "SELECT TimeHr FROM zTimeSpent")) 1.5))
          (sqlite-close db))))))

(ert-deftest writing-habit-pomodoro/a-hand-made-table-with-an-id-column-is-used ()
  (writing-habit-pomodoro-tests--with (dir _lines _day _now)
    (let* ((path (writing-habit-pomodoro-tests--path dir))
           (db (sqlite-open path)))
      (sqlite-execute db "CREATE TABLE \"zzPomCount\" (\"ID\" INTEGER, \"date\" DATE, \"pom_count\" INTEGER, \"recorded_at\" INTEGER, PRIMARY KEY(\"ID\"))")
      (sqlite-execute db "INSERT INTO zzPomCount (date, pom_count, recorded_at) VALUES ('2026-10-21', 1, '2026-10-21T23:59:00')")
      (sqlite-close db)
      (should (eq (writing-habit-pomodoro-record path "zzPomCount" "2026-10-22" 7) 'added))
      (should (eq (writing-habit-pomodoro-record path "zzPomCount" "2026-10-22" 8) 'updated))
      (should (equal (writing-habit-pomodoro-rows path "zzPomCount") '(("2026-10-21" . 1) ("2026-10-22" . 8)))))))

(ert-deftest writing-habit-pomodoro/a-table-of-another-shape-is-refused-and-left-alone ()
  (writing-habit-pomodoro-tests--with (dir _lines _day _now)
    (let* ((path (writing-habit-pomodoro-tests--path dir))
           (db (sqlite-open path)))
      (sqlite-execute db "CREATE TABLE zProjects (id INTEGER PRIMARY KEY, name TEXT)")
      (sqlite-close db)
      (should (string-match-p "not a Pomodoro count table"
                              (cadr (should-error (writing-habit-pomodoro-record path "zProjects" "2026-10-21" 3)))))
      (let ((db (sqlite-open path)))
        (should (equal (mapcar (lambda (r) (nth 1 r)) (sqlite-select db "PRAGMA table_info(zProjects)"))
                       '("id" "name")))
        (sqlite-close db)))))

(ert-deftest writing-habit-pomodoro/the-next-record-time-is-tonight-or-tomorrow-night ()
  (should (= (writing-habit-pomodoro-next-record-time (decode-time (encode-time '(0 0 19 21 10 2026 nil -1 nil)))) (+ (* 4 3600) (* 59 60))))
  (should (= (writing-habit-pomodoro-next-record-time (decode-time (encode-time '(30 59 23 21 10 2026 nil -1 nil)))) (- 86400 30))))

(defun writing-habit-pomodoro-tests--finish-one (now)
  "Run one Pomodoro to its end on the clock NOW, then stop."
  (writing-habit-pomodoro-start)
  (setcar now (+ (car now) 1500.0))
  (writing-habit-pomodoro-update)
  (writing-habit-pomodoro-stop))

(ert-deftest writing-habit-pomodoro/eleven-fifty-nine-writes-the-day ()
  (writing-habit-pomodoro-tests--with (dir lines _day now)
    (writing-habit-pomodoro-tests--finish-one now)
    (writing-habit-pomodoro-tests--finish-one now)
    (setq writing-habit-pomodoro--record-target "2026-10-21")
    (writing-habit-pomodoro-record-tonight)
    (should (equal (writing-habit-pomodoro-rows (writing-habit-pomodoro-tests--path dir) "zzPomCount")
                   '(("2026-10-21" . 2))))
    (should (string-prefix-p "Recorded 2 Pomodoros for 2026-10-21 in zzPomCount"
                             (car (last (writing-habit-pomodoro-tests--logged lines)))))
    (should-not (writing-habit-pomodoro-catch-up))
    (writing-habit-pomodoro-mode -1)))

(ert-deftest writing-habit-pomodoro/a-missed-night-is-written-the-next-day ()
  (writing-habit-pomodoro-tests--with (dir _lines day now)
    (writing-habit-pomodoro-tests--finish-one now)
    (setcar day "2026-10-22")               ; Emacs was closed at 23:59
    (should (eq (writing-habit-pomodoro-catch-up) 'added))
    (should (equal (writing-habit-pomodoro-rows (writing-habit-pomodoro-tests--path dir) "zzPomCount")
                   '(("2026-10-21" . 1))))
    (writing-habit-pomodoro-mode -1)))

(ert-deftest writing-habit-pomodoro/a-late-pomodoro-updates-the-row-before-the-new-day-starts ()
  (writing-habit-pomodoro-tests--with (dir _lines day now)
    (writing-habit-pomodoro-tests--finish-one now)
    (setq writing-habit-pomodoro--record-target "2026-10-21")
    (writing-habit-pomodoro-record-tonight)
    (writing-habit-pomodoro-tests--finish-one now)        ; ends at 23:59:40
    (setcar day "2026-10-22")
    (writing-habit-pomodoro-tests--finish-one now)        ; the first of the new day
    (should (equal (writing-habit-pomodoro-rows (writing-habit-pomodoro-tests--path dir) "zzPomCount")
                   '(("2026-10-21" . 2))))
    (should (= (writing-habit-pomodoro-today-count "2026-10-22") 1))
    (writing-habit-pomodoro-mode -1)))

(ert-deftest writing-habit-pomodoro/a-late-timer-after-sleep-does-not-overwrite ()
  (writing-habit-pomodoro-tests--with (dir _lines day now)
    (writing-habit-pomodoro-tests--finish-one now)
    (setq writing-habit-pomodoro--record-target "2026-10-21")
    (setcar day "2026-10-22")               ; woke the next morning
    (writing-habit-pomodoro-record-tonight)
    (should (equal (writing-habit-pomodoro-rows (writing-habit-pomodoro-tests--path dir) "zzPomCount")
                   '(("2026-10-21" . 1))))
    (writing-habit-pomodoro-mode -1)))

(ert-deftest writing-habit-pomodoro/recording-can-be-turned-off-and-reports-errors ()
  (writing-habit-pomodoro-tests--with (dir lines _day _now)
    (let ((writing-habit-pomodoro-record-db nil))
      (should-not (writing-habit-pomodoro-record-day "2026-10-21" 3)))
    (let ((writing-habit-pomodoro-db-dir (expand-file-name "nowhere" dir)))
      (should-not (writing-habit-pomodoro-record-day "2026-10-21" 3))
      (should (string-prefix-p "Could not record the Pomodoro count for 2026-10-21: No folder"
                               (car (last (writing-habit-pomodoro-tests--logged lines))))))
    (should (eq (writing-habit-pomodoro-record-today) 'added))))

;;;; Sounds

(ert-deftest writing-habit-pomodoro/the-sounds-are-written-once ()
  (let* ((dir (make-temp-file "wh-sounds" t))
         (paths (writing-habit-pomodoro-sound-files dir)))
    (unwind-protect
        (progn
          (dolist (p paths)
            (should (file-exists-p (cdr p)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally (cdr p))
              (should (string-prefix-p "RIFF" (buffer-string)))
              (should (string-search "WAVEfmt " (buffer-string)))))
          (let ((time (file-attribute-modification-time (file-attributes (cdr (assq 'tick paths))))))
            (writing-habit-pomodoro-sound-files dir)
            (should (equal time (file-attribute-modification-time
                                 (file-attributes (cdr (assq 'tick paths))))))))
      (delete-directory dir t))))

(ert-deftest writing-habit-pomodoro/a-missing-custom-file-falls-back ()
  (let ((writing-habit-pomodoro-buzzer-file "/nowhere/alarm.aiff"))
    (let ((choice (writing-habit-pomodoro-choose-sound 'buzzer)))
      (should (string-suffix-p "buzzer.wav" (car choice)))
      (should (equal (cdr choice) "/nowhere/alarm.aiff not found, so the built-in buzzer plays")))))

(provide 'writing-habit-pomodoro-tests)
;;; writing-habit-pomodoro-tests.el ends here
