;;; writing-habit-dashboard-tests.el --- ERT tests for the HTML dashboard -*- lexical-binding: t; -*-

;;; Commentary:

;; Checks the dashboard HTML, including the byte-identical guard against a
;; Python-built fixture rendering committed under fixtures/.  Run from the
;; repository root with:
;;
;;   emacs -batch -L elisp -l ert \
;;     -l elisp/test/writing-habit-dashboard-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'writing-habit-db)
(require 'writing-habit-track)
(require 'writing-habit-dashboard)

(defconst writing-habit-dashboard-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst writing-habit-dashboard-tests--root
  (locate-dominating-file writing-habit-dashboard-tests--dir "schema.sql")
  "Repository root, found by walking up to schema.sql, so tests work in any layout.")

(defconst writing-habit-dashboard-tests--schema
  (expand-file-name "schema.sql" writing-habit-dashboard-tests--root))

(defconst writing-habit-dashboard-tests--xport-db
  (expand-file-name "fixtures/cross-port.db" writing-habit-dashboard-tests--dir))

(defconst writing-habit-dashboard-tests--xport-html
  (expand-file-name "fixtures/dashboard.html" writing-habit-dashboard-tests--dir)
  "The dashboard HTML the Python port renders for the fixture database.")

(defun writing-habit-dashboard-tests--fixture ()
  "Return a small in-memory database with a plan and some actuals."
  (let ((db (writing-habit-db-connect)))
    (writing-habit-db-init db writing-habit-dashboard-tests--schema)
    (let ((a (writing-habit-db-get-or-create-project db "A" "DNPH1 docking" "safe"))
          (w (writing-habit-db-get-or-create-project db "W" "2026words" "speculative"))
          (gen (writing-habit-db-get-category-id db "generative")))
      (sqlite-execute db (concat "INSERT INTO plan_block(day,start_time,end_time,project_id,category_id)"
                                 " VALUES ('2026-01-19','04:00','05:30',?,?)")
                      (list a gen))
      (sqlite-execute db (concat "INSERT INTO plan_block(day,start_time,end_time,project_id,category_id)"
                                 " VALUES ('2026-01-21','04:00','05:30',?,?)")
                      (list w gen))
      (writing-habit-track-add db :day "2026-01-19" :project "A" :minutes 80 :category "generative")
      (writing-habit-track-add db :day "2026-01-20" :project "A" :minutes 90 :category "generative"))
    db))


(ert-deftest writing-habit-dashboard-html-structure ()
  "The dashboard carries the two panels, the tiles, and the palette variables."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-dashboard-tests--fixture)))
    (unwind-protect
        (let ((html (writing-habit-dashboard-html db "2026-01-19")))
          (should (string-prefix-p "<!DOCTYPE html>" html))
          (should (string-suffix-p "</html>\n" html))
          (should (string-match-p "<title>Writing dashboard, week of 2026-01-19</title>" html))
          (should (string-match-p "<h2>Schedule</h2>" html))
          (should (string-match-p "Planned vs actual by project" html))
          (should (string-match-p "day writing streak" html))
          ;; the palette variable and the activity cell classes are present
          (should (string-match-p "--gen: #2a78d6;" html))
          (should (string-match-p "class=\"cell gen\">A</td>" html))
          ;; a meter bar carries an integer width
          (should (string-match-p "class=\"bar planned\" style=\"width:[0-9]+%\"" html)))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-dashboard-escapes-text ()
  "Angle brackets and ampersands in a description are escaped."
  (skip-unless (sqlite-available-p))
  (let ((db (writing-habit-db-connect)))
    (writing-habit-db-init db writing-habit-dashboard-tests--schema)
    (unwind-protect
        (let ((gen (writing-habit-db-get-category-id db "generative")))
          (writing-habit-db-get-or-create-project db "A" "a<b> & \"c\"" "safe")
          (writing-habit-track-add db :day "2026-01-19" :project "A" :minutes 30 :category "generative")
          (let ((html (writing-habit-dashboard-html db "2026-01-19")))
            (should (string-match-p "a&lt;b&gt; &amp; &quot;c&quot;" html))
            (should-not (string-match-p "a<b> & \"c\"" html))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-dashboard-matches-python-fixture ()
  "The Elisp dashboard is byte-identical to the Python port's committed render."
  (skip-unless (sqlite-available-p))
  (skip-unless (file-exists-p writing-habit-dashboard-tests--xport-db))
  (skip-unless (file-exists-p writing-habit-dashboard-tests--xport-html))
  (let ((db (writing-habit-db-connect writing-habit-dashboard-tests--xport-db))
        (expected (with-temp-buffer
                    (insert-file-contents writing-habit-dashboard-tests--xport-html)
                    (buffer-string))))
    (unwind-protect
        (should (equal (writing-habit-dashboard-html db "2026-01-19") expected))
      (writing-habit-db-close db))))

(provide 'writing-habit-dashboard-tests)
;;; writing-habit-dashboard-tests.el ends here
