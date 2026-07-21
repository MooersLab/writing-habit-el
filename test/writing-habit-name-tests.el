;;; writing-habit-name-tests.el --- ERT tests for writing-habit-name -*- lexical-binding: t; -*-

;;; Commentary:

;; A one-to-one port of tests/test_name.py to ERT.  Run from the repository
;; root with:
;;
;;   emacs -batch -L elisp -l ert \
;;     -l elisp/test/writing-habit-name-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Make the module loadable whether or not -L elisp was passed.
(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-name)

(defconst writing-habit-name-tests--root
  (locate-dominating-file
   (file-name-directory (or load-file-name buffer-file-name)) "schema.sql")
  "Repository root, found by walking up to schema.sql, so tests work in any layout.")

(defconst writing-habit-name-tests--table
  (expand-file-name "examples/my-week.org" writing-habit-name-tests--root)
  "The shared example weekly table, reused from the Python tests.")


(ert-deftest writing-habit-name-decode-full-example ()
  "Four identical days then a two-block Friday."
  (let* ((week (writing-habit-name-decode "4gAAeAsA-gWW"))
         (mon (cdr (assoc "Mon" week)))
         (thu (cdr (assoc "Thu" week)))
         (fri (cdr (assoc "Fri" week))))
    (should (equal mon '(("generative" . "A") ("generative" . "A")
                         ("editing" . "A") ("support" . "A"))))
    (should (equal mon thu))
    (should (equal fri '(("generative" . "W") ("generative" . "W"))))))

(ert-deftest writing-habit-name-decode-reduced-and-open ()
  "An open day yields no blocks, and a reduced day yields one."
  (let ((week (writing-habit-name-decode "2gAAeA-o-2gW")))
    (should (equal (cdr (assoc "Wed" week)) '()))
    (should (equal (cdr (assoc "Thu" week)) '(("generative" . "W"))))))

(ert-deftest writing-habit-name-summary-counts ()
  "Totals split correctly by activity and by project."
  (pcase-let ((`(,total ,act ,proj)
               (writing-habit-name-summary
                (writing-habit-name-decode "4gAAeAsA-gWW"))))
    (should (= total 18))
    (should (= (cdr (assoc "generative" act)) 10))
    (should (= (cdr (assoc "editing" act)) 4))
    (should (= (cdr (assoc "support" act)) 4))
    (should (= (cdr (assoc "A" proj)) 16))
    (should (= (cdr (assoc "W" proj)) 2))))

(ert-deftest writing-habit-name-bad-codes ()
  "A block before an activity, and a digit inside a pattern, both fail."
  (should-error (writing-habit-name-decode "gAA-A"))
  (should-error (writing-habit-name-decode "g1A")))

(ert-deftest writing-habit-name-legend-check-exact-and-alias ()
  "Exact codes resolve, and a single-letter alias resolves to its code."
  (let ((legend (writing-habit-name-read-legend writing-habit-name-tests--table)))
    (should (equal (nth 1 (cdr (assoc "A" legend))) "safe"))
    (should (equal (nth 1 (cdr (assoc "W" legend))) "speculative"))
    (pcase-let ((`(,rows ,problems)
                 (writing-habit-name-check-against-legend
                  (writing-habit-name-decode "4gAAeAsA-gWW") legend)))
      (let ((status (mapcar (lambda (r) (cons (nth 0 r) (nth 4 r))) rows)))
        (should (equal (cdr (assoc "A" status)) "exact"))
        (should (equal (cdr (assoc "W" status)) "exact")))
      (should (equal problems '())))
    ;; EM is a two-letter legend code; the single-letter alias E resolves to it.
    (pcase-let ((`(,rows2 ,problems2)
                 (writing-habit-name-check-against-legend
                  (writing-habit-name-decode "gAsE") legend)))
      (let ((r (seq-find (lambda (row) (equal (nth 0 row) "E")) rows2)))
        (should (equal (nth 1 r) "EM"))
        (should (equal (nth 4 r) "alias")))
      (should (equal problems2 '())))))

(ert-deftest writing-habit-name-legend-check-unknown ()
  "A letter with no matching legend code is reported as a problem."
  (let ((legend (writing-habit-name-read-legend writing-habit-name-tests--table)))
    (pcase-let ((`(,_rows ,problems)
                 (writing-habit-name-check-against-legend
                  (writing-habit-name-decode "gZ") legend)))
      (should (equal problems '("Z"))))))

(provide 'writing-habit-name-tests)
;;; writing-habit-name-tests.el ends here
