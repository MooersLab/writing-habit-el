;;; writing-habit-compare-tests.el --- ERT tests for compare and report -*- lexical-binding: t; -*-

;;; Commentary:

;; Ports the compare behavior to ERT and checks the org report.  The fixture
;; is built by direct inserts, so these tests need neither writing-schedule
;; nor any importer.  Run from the repository root with:
;;
;;   emacs -batch -L elisp -l ert \
;;     -l elisp/test/writing-habit-compare-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-db)
(require 'writing-habit-track)
(require 'writing-habit-compare)
(require 'writing-habit-report)

(defconst writing-habit-compare-tests--root
  (locate-dominating-file
   (file-name-directory (or load-file-name buffer-file-name)) "schema.sql")
  "Repository root, found by walking up to schema.sql, so tests work in any layout.")

(defconst writing-habit-compare-tests--schema
  (expand-file-name "schema.sql" writing-habit-compare-tests--root))

(defun writing-habit-compare-tests--fixture ()
  "Return a fresh in-memory database loaded with a controlled scenario.
Plan: A two 90-minute generative blocks, W one.  Actual: A 80 then 90, W 30,
EM 20, on four consecutive days beginning 2026-01-19."
  (let ((db (writing-habit-db-connect)))
    (writing-habit-db-init db writing-habit-compare-tests--schema)
    (let ((a (writing-habit-db-get-or-create-project db "A" "DNPH1 docking" "safe"))
          (w (writing-habit-db-get-or-create-project db "W" "2026words" "speculative"))
          (em (writing-habit-db-get-or-create-project db "EM" "email" "support"))
          (gen (writing-habit-db-get-category-id db "generative")))
      ;; Plan blocks.
      (dolist (row (list (list "2026-01-19" "04:00" "05:30" a gen)
                         (list "2026-01-20" "04:00" "05:30" a gen)
                         (list "2026-01-21" "04:00" "05:30" w gen)))
        (apply (lambda (day s e pid cid)
                 (sqlite-execute
                  db (concat "INSERT INTO plan_block(day,start_time,end_time,project_id,category_id)"
                             " VALUES (?,?,?,?,?)")
                  (list day s e pid cid)))
               row))
      ;; Actual sessions.
      (writing-habit-track-add db :day "2026-01-19" :project "A" :minutes 80 :category "generative")
      (writing-habit-track-add db :day "2026-01-20" :project "A" :minutes 90 :category "generative")
      (writing-habit-track-add db :day "2026-01-21" :project "W" :minutes 30 :category "generative")
      (writing-habit-track-add db :day "2026-01-22" :project "EM" :minutes 20 :category "support"))
    db))

(defun writing-habit-compare-tests--val (row key)
  (cdr (assoc key row)))


(ert-deftest writing-habit-compare-monday ()
  "Any day of the week snaps back to its Monday."
  (should (equal (writing-habit-compare--monday "2026-01-19") "2026-01-19"))  ; Mon
  (should (equal (writing-habit-compare--monday "2026-01-21") "2026-01-19"))  ; Wed
  (should (equal (writing-habit-compare--monday "2026-01-25") "2026-01-19"))  ; Sun
  (should (equal (writing-habit-compare--monday "2026-01-26") "2026-01-26"))) ; next Mon

(ert-deftest writing-habit-compare-week-project ()
  "Per-project planned, actual, diff, and adherence match the plan and actuals."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-compare-tests--fixture)))
    (unwind-protect
        (let* ((rows (writing-habit-compare-week-project db "2026-01-19"))
               (by-code (mapcar (lambda (r) (cons (writing-habit-compare-tests--val r "code") r)) rows)))
          (should (equal (mapcar #'car by-code) '("A" "EM" "W")))   ; ordered by code
          (let ((a (cdr (assoc "A" by-code))))
            (should (= (writing-habit-compare-tests--val a "planned_min") 180))
            (should (= (writing-habit-compare-tests--val a "actual_min") 170))
            (should (= (writing-habit-compare-tests--val a "diff_min") -10))
            (should (< (abs (- (writing-habit-compare-tests--val a "adherence") 0.94)) 0.001)))
          (let ((em (cdr (assoc "EM" by-code))))
            (should (= (writing-habit-compare-tests--val em "planned_min") 0))
            (should (= (writing-habit-compare-tests--val em "actual_min") 20))
            (should (null (writing-habit-compare-tests--val em "adherence")))))  ; planned 0 -> NULL
      (writing-habit-db-close db))))

(ert-deftest writing-habit-compare-streak ()
  "Consecutive worked days count up; a gap resets the run."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-compare-tests--fixture)))
    (unwind-protect
        (progn
          (should (= (writing-habit-compare-current-streak db) 4))  ; 19,20,21,22
          ;; A gap at 23, then 24, drops the streak to that single latest day.
          (writing-habit-track-add db :day "2026-01-24" :project "A" :minutes 30 :category "generative")
          (should (= (writing-habit-compare-current-streak db) 1))
          ;; Filling the gap at 23 reconnects the whole run.
          (writing-habit-track-add db :day "2026-01-23" :project "A" :minutes 30 :category "generative")
          (should (= (writing-habit-compare-current-streak db) 6)))  ; 19..24
      (writing-habit-db-close db))))

(ert-deftest writing-habit-compare-streak-empty ()
  "No worked days is a streak of zero."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-db-connect)))
    (writing-habit-db-init db writing-habit-compare-tests--schema)
    (unwind-protect
        (should (= (writing-habit-compare-current-streak db) 0))
      (writing-habit-db-close db))))


;;;; Report rendering

(ert-deftest writing-habit-report-week-string-content ()
  "The org report shows the right totals, adherence, share, and streak."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-compare-tests--fixture)))
    (unwind-protect
        (let ((text (writing-habit-report-week-string db "2026-01-19")))
          (should (string-match-p "#\\+TITLE: Writing week beginning 2026-01-19" text))
          ;; planned 270, actual 220, overall 220/270 = 0.81
          (should (string-match-p "Planned 270 min, actual 220 min, overall adherence 0.81" text))
          (should (string-match-p "^\\* By project" text))
          (should (string-match-p "0.94" text))          ; A adherence
          (should (string-match-p "n/a" text))            ; EM adherence (planned 0)
          (should (string-match-p "^\\* By barbell class" text))
          ;; speculative share: planned 90/270 = 33%, actual 30/200 = 15%
          (should (string-match-p "Speculative share: planned 33%, actual 15%" text))
          (should (string-match-p "streak of consecutive writing days: 4" text))
          ;; the org tables carry the user's booktabs attribute
          (should (string-match-p "#\\+ATTR_LATEX: :booktabs t" text)))
      (writing-habit-db-close db))))

;;;; Optional plot

(ert-deftest writing-habit-report-plot-python-script ()
  "The generated matplotlib script carries the data and saves to the file."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-compare-tests--fixture)))
    (unwind-protect
        (let* ((proj (writing-habit-compare-week-project db "2026-01-19"))
               (script (writing-habit-report--plot-python proj "2026-01-19" "/tmp/out.png")))
          (should (string-match-p "matplotlib.use(\"Agg\")" script))
          (should (string-match-p "planned = \\[180, 0, 90\\]" script))   ; A, EM, W
          (should (string-match-p "actual = \\[170, 20, 30\\]" script))
          (should (string-match-p "fig.savefig(\"/tmp/out.png\", dpi=120)" script)))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-report-plot-src-block-embedded ()
  "Passing a plot file appends an Org Babel python block to the report."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-compare-tests--fixture)))
    (unwind-protect
        (let ((text (writing-habit-report-week-string db "2026-01-19" "chart.png")))
          (should (string-match-p "^\\* Plot" text))
          (should (string-match-p
                   "#\\+begin_src python :results file graphics :file chart.png" text))
          ;; No plot file, no block.
          (should-not (string-match-p "begin_src"
                                      (writing-habit-report-week-string db "2026-01-19"))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-report-write-plot-makes-png ()
  "The direct writer produces a real PNG file when matplotlib is available."
  (skip-unless (sqlite-available-p))
  (skip-unless (writing-habit-report-matplotlib-available-p))
  (let ((db (writing-habit-compare-tests--fixture))
        (png (make-temp-file "wh-plot" nil ".png")))
    (unwind-protect
        (progn
          (writing-habit-report-write-plot db "2026-01-19" png)
          (should (file-exists-p png))
          (should (> (file-attribute-size (file-attributes png)) 1000))
          ;; PNG magic number.
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally png nil 0 8)
            (should (equal (buffer-substring 2 5) "PNG"))))
      (writing-habit-db-close db)
      (delete-file png))))

(provide 'writing-habit-compare-tests)
;;; writing-habit-compare-tests.el ends here
