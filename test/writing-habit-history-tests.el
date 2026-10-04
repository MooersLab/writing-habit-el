;;; writing-habit-history-tests.el --- ERT tests for the adherence history -*- lexical-binding: t; -*-

;;; Commentary:

;; Ports tests/test_history.py from the Python package.  The fixture
;; history.db was written by the Python test fixture, and history.txt,
;; history-from.txt, and cross-port-history.txt are what the Python
;; command printed for it, so these tests hold the two ports to the same
;; output byte for byte.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit)
(require 'writing-habit-history)

(defconst writing-habit-history-tests--fixtures
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory of the shared fixtures.")

(defun writing-habit-history-tests--file (name)
  "Return the path of the fixture NAME."
  (expand-file-name name writing-habit-history-tests--fixtures))

(defun writing-habit-history-tests--slurp (name)
  "Return the contents of the fixture NAME."
  (with-temp-buffer
    (insert-file-contents (writing-habit-history-tests--file name))
    (buffer-string)))

(defmacro writing-habit-history-tests--with-db (var file &rest body)
  "Bind VAR to a connection to a temporary copy of fixture FILE around BODY."
  (declare (indent 2))
  (let ((tmp (make-symbol "tmp")))
    `(let ((,tmp (make-temp-file "wh-hist" nil ".db")))
       (copy-file (writing-habit-history-tests--file ,file) ,tmp t)
       (let ((,var (writing-habit-db-connect ,tmp)))
         (unwind-protect (progn ,@body)
           (writing-habit-db-close ,var)
           (delete-file ,tmp))))))

(defun writing-habit-history-tests--by-week (rows key)
  "Return an alist from week to the KEY value of ROWS."
  (mapcar (lambda (r) (cons (cdr (assoc "week_start" r)) (cdr (assoc key r)))) rows))

(defun writing-habit-history-tests--approx (got want)
  "Return non-nil when the alists GOT and WANT agree to within 0.005."
  (and (= (length got) (length want))
       (cl-every (lambda (w)
                   (let ((g (cdr (assoc (car w) got))))
                     (and g (< (abs (- g (cdr w))) 0.005))))
                 want)))

(ert-deftest writing-habit-history/overall-series ()
  "Overall adherence is summed actual over summed planned."
  (writing-habit-history-tests--with-db db "history.db"
    (should (writing-habit-history-tests--approx
             (writing-habit-history-tests--by-week
              (writing-habit-compare-overall-series db) "adherence")
             '(("2026-01-05" . 0.54) ("2026-01-12" . 0.86) ("2026-01-19" . 0.48))))))

(ert-deftest writing-habit-history/project-mean-series ()
  "The project mean weights every project equally."
  (writing-habit-history-tests--with-db db "history.db"
    (should (writing-habit-history-tests--approx
             (writing-habit-history-tests--by-week
              (writing-habit-compare-project-mean-series db) "mean_adherence")
             '(("2026-01-05" . 0.45) ("2026-01-12" . 0.85) ("2026-01-19" . 0.45))))))

(ert-deftest writing-habit-history/overall-differs-from-project-mean ()
  "The two headline definitions diverge when project sizes differ."
  (writing-habit-history-tests--with-db db "history.db"
    (should-not (equal (cdr (assoc "2026-01-05"
                                   (writing-habit-history-tests--by-week
                                    (writing-habit-compare-overall-series db)
                                    "adherence")))
                       (cdr (assoc "2026-01-05"
                                   (writing-habit-history-tests--by-week
                                    (writing-habit-compare-project-mean-series db)
                                    "mean_adherence")))))))

(ert-deftest writing-habit-history/category-mean-series ()
  "Each activity has its own mean per-project adherence."
  (writing-habit-history-tests--with-db db "history.db"
    (cl-flet ((series (cat) (writing-habit-history-tests--by-week
                             (writing-habit-compare-category-mean-series db cat)
                             "mean_adherence")))
      (should (writing-habit-history-tests--approx
               (series "generative")
               '(("2026-01-05" . 0.40) ("2026-01-12" . 0.80) ("2026-01-19" . 0.33))))
      (should (writing-habit-history-tests--approx
               (series "editing")
               '(("2026-01-05" . 1.00) ("2026-01-12" . 0.90) ("2026-01-19" . 0.60))))
      (should (writing-habit-history-tests--approx
               (series "support")
               '(("2026-01-05" . 0.50) ("2026-01-12" . 1.00) ("2026-01-19" . 0.80)))))))

(ert-deftest writing-habit-history/range-filter ()
  "START and END snap to Mondays and bound the weeks."
  (writing-habit-history-tests--with-db db "history.db"
    (should (equal (mapcar #'car (writing-habit-history-tests--by-week
                                  (writing-habit-compare-overall-series
                                   db "2026-01-12" "2026-01-19")
                                  "adherence"))
                   '("2026-01-12" "2026-01-19")))
    (should (equal (mapcar #'car (writing-habit-history-tests--by-week
                                  (writing-habit-compare-category-mean-series
                                   db "support" "2026-01-14")
                                  "mean_adherence"))
                   '("2026-01-12" "2026-01-19")))))

(ert-deftest writing-habit-history/collect-weeks-sorted ()
  "Collect returns the sorted union of weeks and five series."
  (writing-habit-history-tests--with-db db "history.db"
    (let ((data (writing-habit-history-collect db)))
      (should (equal (car data) '("2026-01-05" "2026-01-12" "2026-01-19")))
      (should (equal (mapcar #'car (cdr data))
                     '("overall" "project_mean" "generative" "editing" "support"))))))

(ert-deftest writing-habit-history/text-matches-python ()
  "The plain-text table matches the Python output byte for byte."
  (writing-habit-history-tests--with-db db "history.db"
    (should (equal (concat (writing-habit-history-string db) "\n")
                   (writing-habit-history-tests--slurp "history.txt")))
    (should (equal (concat (writing-habit-history-string db "2026-01-13") "\n")
                   (writing-habit-history-tests--slurp "history-from.txt")))))

(ert-deftest writing-habit-history/cross-port-text ()
  "The cross-port database renders the same history text as Python."
  (writing-habit-history-tests--with-db db "cross-port.db"
    (should (equal (concat (writing-habit-history-string db) "\n")
                   (writing-habit-history-tests--slurp "cross-port-history.txt")))))

(ert-deftest writing-habit-history/empty-range ()
  "A range with no weeks says so."
  (writing-habit-history-tests--with-db db "history.db"
    (should (equal (writing-habit-history-string db "2027-01-01")
                   (concat "Weekly adherence history\n" (make-string 72 ?=)
                           "\nNo weeks in range.")))))

(ert-deftest writing-habit-history/org-table ()
  "The org view holds one row per week with blank cells for missing values."
  (writing-habit-history-tests--with-db db "history.db"
    (let ((org (writing-habit-history-org db)))
      (should (string-match-p "^\\* Weekly adherence history" org))
      (should (string-match-p "^| 2026-01-12 | 0.86 " org))
      (should (string-match-p "#\\+ATTR_LATEX: :booktabs t" org)))))

(ert-deftest writing-habit-history/plot-script ()
  "The plot script names every series and writes to the requested path."
  (writing-habit-history-tests--with-db db "history.db"
    (let* ((data (writing-habit-history-collect db))
           (py (writing-habit-history--plot-python (car data) (cdr data) "/tmp/x.png")))
      (should (string-match-p "s_overall = \\[0.54, 0.86, 0.48\\]" py))
      (should (string-match-p "fig.savefig(\"/tmp/x.png\"" py)))))

(ert-deftest writing-habit-history/write-plots ()
  "With matplotlib present, the five-panel figure is written."
  (skip-unless (writing-habit-report-matplotlib-available-p))
  (writing-habit-history-tests--with-db db "history.db"
    (let ((out (make-temp-file "wh-trend" nil ".png")))
      (unwind-protect
          (progn (writing-habit-history-write-plots db out)
                 (should (> (file-attribute-size (file-attributes out)) 0)))
        (delete-file out)))))

(ert-deftest writing-habit-history/batch-dispatch ()
  "The history subcommand runs through the batch dispatcher."
  (let ((tmp (make-temp-file "wh-hist" nil ".db")))
    (unwind-protect
        (progn
          (copy-file (writing-habit-history-tests--file "history.db") tmp t)
          (should (equal (concat (writing-habit--dispatch
                                  (list "history" "--from" "2026-01-13" "--db" tmp))
                                 "\n")
                         (writing-habit-history-tests--slurp "history-from.txt"))))
      (delete-file tmp))))

;;;; Legend cell parsing, shared by the reader and the editor

(ert-deftest writing-habit-history/parse-legend-cell ()
  "A legend cell gives code, description, and risk class."
  (should (equal (writing-habit-name-parse-legend-cell "A: DNPH1 docking :safe:")
                 '("A" "DNPH1 docking" "safe")))
  (should (equal (writing-habit-name-parse-legend-cell " W: 2026words (risky) ")
                 '("W" "2026words" "speculative")))
  (should (equal (writing-habit-name-parse-legend-cell "EM: email :support:")
                 '("EM" "email" nil)))
  (should (equal (writing-habit-name-parse-legend-cell "C:") '("C" "" nil)))
  (should-not (writing-habit-name-parse-legend-cell "Generative:"))
  (should-not (writing-habit-name-parse-legend-cell "05:00-06:00")))

(ert-deftest writing-habit-history/read-legend-keeps-first ()
  "A code defined twice keeps its first definition, as in Python."
  (let ((f (make-temp-file "wh-leg" nil ".org"
                           (concat "| Time | M |\n|-\n| A: first :safe: | |\n"
                                   "| B: bee |  |\n| A: second :risky: | |\n"))))
    (unwind-protect
        (should (equal (writing-habit-name-read-legend f)
                       '(("A" "first" "safe") ("B" "bee" nil))))
      (delete-file f))))

(provide 'writing-habit-history-tests)
;;; writing-habit-history-tests.el ends here
