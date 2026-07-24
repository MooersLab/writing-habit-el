;;; writing-habit-seasons-tests.el --- ERT tests for the seasons dashboard -*- lexical-binding: t; -*-

;;; Commentary:

;; Byte-identical guard against the Python-built seasons golden, rendered from
;; the shared 20-week fixtures/seasons.db.  Run from the repository root with:
;;
;;   emacs -batch -L . -l ert \
;;     -l test/writing-habit-seasons-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-db)
(require 'writing-habit-context)
(require 'writing-habit-seasons)

(defconst writing-habit-seasons-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst writing-habit-seasons-tests--db
  (expand-file-name "fixtures/seasons.db" writing-habit-seasons-tests--dir))

(defconst writing-habit-seasons-tests--html
  (expand-file-name "fixtures/seasons.html" writing-habit-seasons-tests--dir)
  "The seasons HTML the Python port renders for the fixture database.")

(ert-deftest writing-habit-seasons-matches-python-fixture ()
  "The Elisp seasons dashboard is byte-identical to the Python render."
  (skip-unless (sqlite-available-p))
  (skip-unless (file-exists-p writing-habit-seasons-tests--db))
  (skip-unless (file-exists-p writing-habit-seasons-tests--html))
  (let ((db (writing-habit-db-connect writing-habit-seasons-tests--db))
        (expected (with-temp-buffer
                    (insert-file-contents writing-habit-seasons-tests--html)
                    (buffer-string))))
    (unwind-protect
        (should (equal (writing-habit-seasons-html db) expected))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-seasons-structure ()
  "The seasons page carries the three grouping sections and the tiles."
  (skip-unless (sqlite-available-p))
  (skip-unless (file-exists-p writing-habit-seasons-tests--db))
  (let ((db (writing-habit-db-connect writing-habit-seasons-tests--db)))
    (unwind-protect
        (let ((html (writing-habit-seasons-html db)))
          (should (string-prefix-p "<!DOCTYPE html>" html))
          (should (string-suffix-p "</html>\n" html))
          (should (string-match-p "<title>Writing seasons: grouped adherence</title>" html))
          (should (string-match-p "<h2>By month</h2>" html))
          (should (string-match-p "<h2>By event context</h2>" html))
          (should (string-match-p "<h2>By schedule</h2>" html))
          (should (string-match-p "weeks recorded" html)))
      (writing-habit-db-close db))))

(provide 'writing-habit-seasons-tests)
;;; writing-habit-seasons-tests.el ends here
