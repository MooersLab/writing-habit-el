;;; writing-habit-hours-tests.el --- ERT tests for the hour dashboard -*- lexical-binding: t; -*-

;;; Commentary:

;; The hour-by-day dashboard of a week.

;;; Code:

(require 'ert)
(require 'writing-habit-hours)

(defconst writing-habit-hours-tests--week
  (concat "|        Time | M  | Tu | W  |\n"
          "|-------------+----+----+----|\n"
          "| 05:00-06:30 | gA | eB | gA |\n"
          "| 06:45-07:15 |    | sE |    |\n"
          "| 14:00-15:30 |    | gB |    |\n"
          "|-------------+----+----+----|\n"
          "| A: 1006AIrxOpt :safe: |  |  |  |\n"
          "| B: 0201dusp1 :safe: |  |  |  |\n"
          "| E: email |  |  |  |\n")
  "A small week.")

(ert-deftest writing-habit-hours/cells ()
  (skip-unless (featurep 'writing-schedule))
  (let* ((table (writing-habit-table-from-text writing-habit-hours-tests--week))
         (grid (writing-habit-hours-cells (writing-habit-table-blocks table))))
    (should (equal (seq-take grid 2) '(5 15)))
    (should (equal (mapcar #'cdr (gethash '(5 . 0) (nth 2 grid))) '(t)))
    (should (equal (mapcar #'cdr (gethash '(6 . 0) (nth 2 grid))) '(nil)))
    (should (= (length (gethash '(6 . 1) (nth 2 grid))) 2))   ; eB continues and sE starts
    (should (null (gethash '(7 . 0) (nth 2 grid))))))

(ert-deftest writing-habit-hours/string ()
  (skip-unless (featurep 'writing-schedule))
  (let* ((table (writing-habit-table-from-text writing-habit-hours-tests--week))
         (text (writing-habit-hours-string table))
         (lines (split-string text "\n")))
    (should (string-match-p "\\`05:00 gA 05:00-06:30 +eB 05:00-06:30 +gA 05:00-06:30 *\\'" (nth 4 lines)))
    (should (string-match-p "\\`06:00 ↓ A +↓ B sE 06:45-07… +↓ A *\\'" (nth 5 lines)))
    (should (string-match-p "\\`07:00 · +↓ E +· *\\'" (nth 6 lines)))
    (should (string-match-p "\\`08:00 · +· +· *\\'" (nth 7 lines)))
    (should (string-match-p "\\`14:00 · +gB 14:00-15:30 +· *\\'" (nth 13 lines)))
    (should (string-match-p "\\`Total 90 min +210 min +90 min *\\'" (nth 15 lines)))
    (let ((pos (string-search "gA 05:00" text)))
      (should (writing-habit-table-block-p (get-text-property pos 'writing-habit-hours-block text))))))

(ert-deftest writing-habit-hours/command-and-visit ()
  (skip-unless (featurep 'writing-schedule))
  (require 'writing-habit-table-mode)
  (with-temp-buffer
    (insert writing-habit-hours-tests--week)
    (let ((week (current-buffer)))
      (save-window-excursion
        (writing-habit-hours)
        (with-current-buffer "*writing-habit hours*"
          (should (eq writing-habit-hours--week-buffer week))
          (goto-char (point-min))
          (search-forward "gB 14:00")
          (writing-habit-hours-visit)
          (should (eq (current-buffer) week))
          (should (looking-at "gB"))
          (should (= (line-number-at-pos) 5)))))
    (kill-buffer "*writing-habit hours*")))

(provide 'writing-habit-hours-tests)
;;; writing-habit-hours-tests.el ends here
