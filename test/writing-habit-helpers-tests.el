;;; writing-habit-helpers-tests.el --- ERT tests for the helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; The cases of the Python tests test_capture.py, test_numbering.py,
;; test_triage.py, and test_stuck.py, with the same inputs and the same
;; expected strings, so the two ports cannot drift.  No test reaches a
;; model server.

;;; Code:

(require 'ert)
(require 'writing-habit-table-mode)
(require 'writing-habit-helpers)

(defvar writing-habit-default-db)
(defvar writing-habit-dispatch--last-db)

;;;; Quick capture

(defconst writing-habit-helpers-tests--legend
  '(("A" . "1001grantR01 2026-11-15") ("B" . "2002reviewArticle 2026-12-01")
    ("C" . "3003methodsNote") ("E" . "9001email")))

(defconst writing-habit-helpers-tests--lists
  '(("F" . "5005bookChapter 2026-12-20") (nil . "6006renewalR01 2027-03-05")))

(defun writing-habit-helpers-tests--projects ()
  "Return the projects of the capture fixture."
  (writing-habit-helpers-capture-projects-from writing-habit-helpers-tests--legend
                                               writing-habit-helpers-tests--lists))

(ert-deftest writing-habit-helpers/capture-projects-from-legend-and-lists ()
  (should (equal (mapcar (lambda (p) (cons (writing-habit-helpers-project-code p)
                                           (writing-habit-helpers-project-name p)))
                         (writing-habit-helpers-tests--projects))
                 '(("A" . "1001grantR01") ("B" . "2002reviewArticle")
                   ("C" . "3003methodsNote") ("E" . "9001email")
                   ("F" . "5005bookChapter")))))

(ert-deftest writing-habit-helpers/capture-part-of-the-name ()
  (let ((got (writing-habit-helpers-capture-plain
              "remember to email the reviewer about figure 3 for the R01, priority A"
              (writing-habit-helpers-tests--projects))))
    (should (equal (writing-habit-helpers-captured-code got) "A"))
    (should (equal (writing-habit-helpers-captured-task got)
                   "Email the reviewer about figure 3 for the R01"))
    (should (equal (writing-habit-helpers-captured-cookie got) "A"))
    (should (equal (writing-habit-helpers-captured-how got) "R01 in its name"))
    (should (string-prefix-p "[#A] Email the reviewer"
                             (writing-habit-helpers-captured-entry-text got)))))

(ert-deftest writing-habit-helpers/capture-number-code-and-name ()
  (let ((projects (writing-habit-helpers-tests--projects)))
    (should (equal (writing-habit-helpers-captured-code
                    (writing-habit-helpers-capture-plain "check the stats in 3003" projects))
                   "C"))
    (let ((got (writing-habit-helpers-capture-plain "C: tidy the methods" projects)))
      (should (equal (writing-habit-helpers-captured-task got) "Tidy the methods"))
      (should (equal (writing-habit-helpers-captured-how got) "its code C")))
    (should (equal (writing-habit-helpers-captured-code
                    (writing-habit-helpers-capture-plain "outline for bookChapter" projects))
                   "F"))
    (should (equal (writing-habit-helpers-captured-code
                    (writing-habit-helpers-capture-plain "ask Sam about project B" projects))
                   "B"))))

(ert-deftest writing-habit-helpers/capture-none-or-a-tie ()
  (should-not (writing-habit-helpers-captured-code
               (writing-habit-helpers-capture-plain "buy printer toner"
                                                    (writing-habit-helpers-tests--projects))))
  (let ((got (writing-habit-helpers-capture-plain
              "cite the R01 budget rules"
              (list (writing-habit-helpers-project-create :code "A" :name "1001grantR01")
                    (writing-habit-helpers-project-create :code "G" :name "6006renewalR01")))))
    (should-not (writing-habit-helpers-captured-code got))
    (should (string-search "both A and G" (writing-habit-helpers-captured-how got)))))

(ert-deftest writing-habit-helpers/capture-cookie ()
  (let ((got (writing-habit-helpers-capture-plain "[#B2] reread the comments on the grant R01"
                                                  (writing-habit-helpers-tests--projects))))
    (should (equal (writing-habit-helpers-captured-cookie got) "B2"))
    (should (equal (writing-habit-helpers-captured-task got)
                   "Reread the comments on the grant R01")))
  (should-not (writing-habit-helpers-capture-priority "nothing urgent here")))

;;;; Project numbering help

(defconst writing-habit-helpers-tests--known
  (writing-habit-helpers-numbering-known-projects
   (split-string "0131RETKinase2018BrP 0132RETKinase2019JBC 0139RETprotac 0140RETactivationLoopDeletions 0141RETkinaseMutants 0142RETKinaseCovalentInhibitors 0149RETKinaseDocking 0200DUSP1review 0201DUSP1radiation 0378QCrPluginPyMOL 0379pymolSceneManager 0385PyMOLWhisper 0403PyMOLRStudio 0404PYMOLColab 1001grantR01")))

(ert-deftest writing-habit-helpers/numbering-words ()
  (should (equal (writing-habit-helpers-numbering-words "0142RETKinaseCovalentInhibitors")
                 '("ret" "kinase" "covalent" "inhibitors")))
  (should (equal (writing-habit-helpers-numbering-words "0378QCrPluginPyMOL")
                 '("qcr" "plugin" "pymol")))
  (should (equal (writing-habit-helpers-numbering-camel "RET kinase PROTAC degraders")
                 "RETKinasePROTACDegraders")))

(ert-deftest writing-habit-helpers/numbering-rare-word ()
  (let ((s (writing-habit-helpers-numbering-suggest "RET kinase PROTAC degraders"
                                                    writing-habit-helpers-tests--known)))
    (should (equal (mapcar #'car (writing-habit-helpers-suggestion-related s)) '("0139")))
    (should (equal (writing-habit-helpers-suggestion-numbers s) '("0143" "0144" "0145")))
    (should (equal (writing-habit-helpers-suggestion-reason s)
                   "1 project shares ret, protac with the new name"))))

(ert-deftest writing-habit-helpers/numbering-inside-the-range ()
  (let* ((s (writing-habit-helpers-numbering-suggest "RET kinase review 2026"
                                                     writing-habit-helpers-tests--known))
         (related (mapcar #'car (writing-habit-helpers-suggestion-related s))))
    (dolist (n '("0131" "0141" "0149")) (should (member n related)))
    (should (member (car (writing-habit-helpers-suggestion-numbers s))
                    '("0143" "0144" "0145" "0146" "0147" "0148")))
    (dolist (n (writing-habit-helpers-suggestion-numbers s))
      (should-not (assoc n writing-habit-helpers-tests--known)))))

(ert-deftest writing-habit-helpers/numbering-pymol-spelling ()
  (let ((s (writing-habit-helpers-numbering-suggest "PyMOL in Positron"
                                                    writing-habit-helpers-tests--known)))
    (should (equal (sort (mapcar #'car (writing-habit-helpers-suggestion-related s)) #'string<)
                   '("0378" "0379" "0385" "0403" "0404")))))

(ert-deftest writing-habit-helpers/numbering-follows-active ()
  (let ((s (writing-habit-helpers-numbering-suggest "Seeded direct methods"
                                                    writing-habit-helpers-tests--known
                                                    3 '("0142" "1001"))))
    (should-not (writing-habit-helpers-suggestion-related s))
    (should (equal (writing-habit-helpers-suggestion-numbers s) '("1002" "1003" "1004")))
    (should (string-search "follows the highest number of this week's projects"
                           (writing-habit-helpers-suggestion-reason s)))))

(ert-deftest writing-habit-helpers/numbering-folders ()
  (let ((root (make-temp-file "wh-folders" t)))
    (unwind-protect
        (progn
          (dolist (name '("0142RETKinase" "0200DUSP1review" "notes" "2026words"))
            (make-directory (expand-file-name name root)))
          (with-temp-file (expand-file-name "0300file.txt" root) (insert "x"))
          (should (equal (writing-habit-helpers-numbering-folder-projects root)
                         '(("0142" . "0142RETKinase") ("0200" . "0200DUSP1review")
                           ("2026" . "2026words"))))
          (should-not (writing-habit-helpers-numbering-folder-projects
                       (expand-file-name "missing" root))))
      (delete-directory root t))))

(ert-deftest writing-habit-helpers/numbering-forming ()
  (let ((found (writing-habit-helpers-numbering-forming
                '(("A" "Outline the PROTAC linker study" "Order PROTAC compounds"
                   "Email Sam about PROTAC assays" "Fix figure 2")
                  ("B" "Draft aim 2" "Draft aim 3"))
                '(("A" . "0142RETKinaseCovalentInhibitors") ("B" . "1001grantR01")))))
    (should (equal (mapcar (lambda (f) (list (writing-habit-helpers-forming-code f)
                                             (writing-habit-helpers-forming-word f)
                                             (length (writing-habit-helpers-forming-tasks f))))
                           found)
                   '(("A" "protac" 3))))
    (should (string-prefix-p
             "3 tasks of A 0142RETKinaseCovalentInhibitors mention “protac”"
             (writing-habit-helpers-forming-sentence (car found)))))
  (should-not (writing-habit-helpers-numbering-forming
               '(("A" "Read the RET paper" "Check the RET table" "Edit the RET methods"))
               '(("A" . "0142RETKinase")))))

(ert-deftest writing-habit-helpers/numbering-plain-and-line ()
  (let ((s (writing-habit-helpers-numbering-suggest "RET kinase PROTAC degraders"
                                                    writing-habit-helpers-tests--known)))
    (should (string-prefix-p "0143 is free and sits with the related projects 0139RETprotac"
                             (writing-habit-helpers-numbering-plain s nil)))
    (should (equal (writing-habit-helpers-suggestion-line s "0143")
                   "Use 0143RETKinasePROTACDegraders, near 0139"))
    (let ((writing-habit-coach-use-model nil))
      (should (equal (plist-get (writing-habit-coach-note
                                 "numbering" (writing-habit-helpers-numbering-facts s nil)
                                 (writing-habit-helpers-numbering-plain s nil))
                                :text)
                     (writing-habit-helpers-numbering-plain s nil))))))

;;;; Roll-forward triage

(defun writing-habit-helpers-tests--entries (&rest cells)
  "Parse CELLS as list entries."
  (mapcar #'writing-habit-lists-parse-entry cells))

(ert-deftest writing-habit-helpers/triage-recent-work-keeps ()
  (let ((d (writing-habit-helpers-triage-decide
            "1006AIrxOpt" nil '(("2026-10-05" . 45) ("2026-09-28" . 30))
            "2026-10-14" "2026-12-18")))
    (should (equal (writing-habit-helpers-decision-action d) "keep"))
    (should (= (writing-habit-helpers-decision-recent d) 75))
    (should (equal (writing-habit-helpers-decision-reason d)
                   "75 minutes were logged for it in the last 4 weeks, so it is alive"))))

(ert-deftest writing-habit-helpers/triage-deadlines ()
  (let ((d (writing-habit-helpers-triage-decide "1452PHFSboXt" "2026-11-02" nil
                                                "2026-10-14" "2026-12-18")))
    (should (equal (writing-habit-helpers-decision-action d) "keep"))
    (should (string-search "falls in this period" (writing-habit-helpers-decision-reason d))))
  (let ((d (writing-habit-helpers-triage-decide "1002RNAediting" "2027-03-01"
                                                '(("2026-09-07" . 20))
                                                "2026-10-14" "2026-12-18"
                                                "Spring 2027 projects")))
    (should (equal (writing-habit-helpers-decision-action d) "defer"))
    (should (equal (writing-habit-helpers-decision-line d)
                   "Defer 1002RNAediting to Spring 2027 projects"))))

(ert-deftest writing-habit-helpers/triage-old-work-defers ()
  (let ((d (writing-habit-helpers-triage-decide "2003reviewArticle" nil
                                                '(("2026-08-31" . 90))
                                                "2026-10-14" "2026-12-18")))
    (should (equal (writing-habit-helpers-decision-action d) "defer"))
    (should (equal (writing-habit-helpers-decision-last-week d) "2026-08-31"))
    (should (string-search "last worked the week of 2026-08-31"
                           (writing-habit-helpers-decision-reason d)))))

(ert-deftest writing-habit-helpers/triage-drops ()
  (let ((gone (writing-habit-helpers-triage-decide "0900oldTalk" "2026-09-01" nil
                                                   "2026-10-14" "2026-12-18"))
        (idle (writing-habit-helpers-triage-decide "0901idea" nil nil
                                                   "2026-10-14" "2026-12-18"))
        (late (writing-habit-helpers-triage-decide "0902late" "2026-09-01"
                                                   '(("2026-10-12" . 120))
                                                   "2026-10-14" "2026-12-18")))
    (should (equal (writing-habit-helpers-decision-action gone) "drop"))
    (should (string-search "Renegotiate" (writing-habit-helpers-decision-reason gone)))
    (should (equal (writing-habit-helpers-decision-action idle) "drop"))
    (should (equal (writing-habit-helpers-decision-line idle) "Drop 0901idea from the lists"))
    (should (equal (writing-habit-helpers-decision-action late) "keep"))))

(ert-deftest writing-habit-helpers/triage-minutes-follow-the-number ()
  (let ((db (writing-habit-db-connect nil)))
    (unwind-protect
        (progn
          (writing-habit-db-init db)
          (let ((a (writing-habit-db-get-or-create-project db "A" "1006AIrxOpt" "safe"))
                (q (writing-habit-db-get-or-create-project db "Q" "0900oldTalk" "safe")))
            (dolist (row `(("2026-10-06" ,a 50) ("2026-10-07" ,a 40)
                           ("2026-09-02" ,q 30) ("2026-01-05" ,a 99)))
              (sqlite-execute db "INSERT INTO session(day, actual_min, project_id, source)
 VALUES (?, ?, ?, 'manual')" (list (nth 0 row) (nth 2 row) (nth 1 row)))))
          (let ((found (writing-habit-helpers-triage-minutes-by-project
                        db '(("A" . "1006AIrxOpt extra")) "2026-06-01")))
            (should (equal (cdr (assoc "1006" found)) '(("2026-10-05" . 90))))
            (should (equal (cdr (assoc "0900" found)) '(("2026-08-31" . 30))))
            (should (= (length found) 2)))
          (let ((found (writing-habit-helpers-triage-minutes-by-project
                        db '(("Q" . "1006AIrxOpt")) "2026-06-01")))
            (should (equal (sort (copy-sequence (cdr (assoc "1006" found)))
                                 (lambda (x y) (string< (car x) (car y))))
                           '(("2026-08-31" . 30) ("2026-10-05" . 90))))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-helpers/triage-entries-and-plain ()
  (let* ((rows (writing-habit-helpers-tests--entries
                "- 1006AIrxOpt :safe:" "- 1002RNAediting 2027-03-01 :safe:" "- 0901idea"))
         (found (writing-habit-helpers-triage rows '(("1006" ("2026-10-05" . 90)))
                                              "2026-10-14" "2026-12-18"
                                              "Spring 2027 projects"))
         (text (writing-habit-helpers-triage-plain found)))
    (should (equal (mapcar #'writing-habit-helpers-decision-action found)
                   '("keep" "defer" "drop")))
    (should (string-prefix-p "3 carried projects: keep 1, defer 1, drop 1." text))
    (should (string-search "  Drop 0901idea, because no time was logged" text))
    (should (string-prefix-p "Nothing is on the Carried over list"
                             (writing-habit-helpers-triage-plain nil)))))

;;;; Stuck projects

(defun writing-habit-helpers-tests--stuck-db ()
  "Return an in-memory database with project A."
  (let ((db (writing-habit-db-connect nil)))
    (writing-habit-db-init db)
    (writing-habit-db-get-or-create-project db "A" "1001grantR01" "safe")
    db))

(defun writing-habit-helpers-tests--block (db day objectives &optional code start)
  "Add the OBJECTIVES, (TEXT . DONE) pairs, of a block on DAY to DB."
  (let ((position 0))
    (dolist (o objectives)
      (sqlite-execute db "INSERT INTO block_objective(day, start_time, code, position, text, done)
 VALUES (?, ?, ?, ?, ?, ?)"
                      (list day (or start "05:00") (or code "A") position (car o)
                            (if (cdr o) 1 0)))
      (setq position (1+ position)))))

(ert-deftest writing-habit-helpers/stuck-repeated ()
  (let ((db (writing-habit-helpers-tests--stuck-db)))
    (unwind-protect
        (progn
          (writing-habit-helpers-tests--block db "2026-10-05" '(("Draft aim 2") ("Find citations" . t)))
          (writing-habit-helpers-tests--block db "2026-10-06" '(("draft aim 2.")))
          (writing-habit-helpers-tests--block db "2026-10-07" '(("Fix figure 1" . t)))
          (let ((found (writing-habit-helpers-stuck-check db "A")))
            (should (equal (list (writing-habit-helpers-stuck-kind found)
                                 (writing-habit-helpers-stuck-task found)
                                 (writing-habit-helpers-stuck-times found)
                                 (writing-habit-helpers-stuck-blocks found))
                           '("repeated" "draft aim 2." 2 3)))
            (should (equal (writing-habit-helpers-stuck-sentence found)
                           "“draft aim 2.” has gone unmet in 2 of the last 3 blocks of A 1001grantR01."))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-helpers/stuck-low-share ()
  (let ((db (writing-habit-helpers-tests--stuck-db)))
    (unwind-protect
        (progn
          (writing-habit-helpers-tests--block db "2026-10-05" '(("Outline" . t) ("Draft intro") ("Edit")))
          (writing-habit-helpers-tests--block db "2026-10-06" '(("Draft methods")))
          (writing-habit-helpers-tests--block db "2026-10-07" '(("Draft results")))
          (let ((found (writing-habit-helpers-stuck-check db "A")))
            (should (equal (list (writing-habit-helpers-stuck-kind found)
                                 (writing-habit-helpers-stuck-met found)
                                 (writing-habit-helpers-stuck-committed found)
                                 (writing-habit-helpers-stuck-task found))
                           '("low" 1 5 "Draft results")))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-helpers/stuck-same-step ()
  (let ((db (writing-habit-helpers-tests--stuck-db)))
    (unwind-protect
        (progn
          (dolist (day '("2026-10-05" "2026-10-06"))
            (sqlite-execute db "INSERT INTO session_review(day, start_time, code, next_step)
 VALUES (?, '05:00', 'A', 'Start the budget justification')" (list day)))
          (let ((found (writing-habit-helpers-stuck-check db "A")))
            (should (equal (writing-habit-helpers-stuck-kind found) "same_step"))
            (should (equal (writing-habit-helpers-stuck-task found)
                           "Start the budget justification"))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-helpers/stuck-not-stuck ()
  (let ((db (writing-habit-helpers-tests--stuck-db)) (n 0))
    (unwind-protect
        (progn
          (dolist (day '("2026-10-05" "2026-10-06" "2026-10-07"))
            (writing-habit-helpers-tests--block
             db day `(("Write one paragraph" . t) (,(format "Check refs of part %d" n))))
            (setq n (1+ n)))
          (should-not (writing-habit-helpers-stuck-check db "A"))
          (should (string-search "No project shows a sign of being stuck"
                                 (writing-habit-helpers-stuck-report nil))))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-helpers/stuck-does-not-judge-itself ()
  (let ((db (writing-habit-helpers-tests--stuck-db)))
    (unwind-protect
        (progn
          (writing-habit-helpers-tests--block db "2026-10-05" '(("Draft aim 2")))
          (writing-habit-helpers-tests--block db "2026-10-06" '(("Draft aim 2")))
          (should-not (writing-habit-helpers-stuck-check db "A" '("2026-10-06" . "05:00")))
          (should-not (writing-habit-helpers-stuck-check db "A" nil "2026-10-05"))
          (should (equal (writing-habit-helpers-stuck-kind
                          (writing-habit-helpers-stuck-check db "A" '("2026-10-07" . "05:00")))
                         "repeated")))
      (writing-habit-db-close db))))

(ert-deftest writing-habit-helpers/stuck-find-and-report ()
  (let ((db (writing-habit-helpers-tests--stuck-db)))
    (unwind-protect
        (progn
          (writing-habit-helpers-tests--block db "2026-10-05" '(("Draft aim 2")))
          (writing-habit-helpers-tests--block db "2026-10-06" '(("Draft aim 2")))
          (let ((found (writing-habit-helpers-stuck-find db '("A" "B"))))
            (should (equal (mapcar #'writing-habit-helpers-stuck-code found) '("A")))
            (should (string-search "Suggest breaks it into smaller objectives"
                                   (writing-habit-helpers-stuck-report found)))))
      (writing-habit-db-close db))))

;;;; Commands on a table buffer

(defconst writing-habit-helpers-tests--week
  (concat "#+TITLE: helpers\n"
          "|        Time | M  | Tu |\n"
          "|-------------+----+----|\n"
          "| 05:00-06:30 | gA | gB |\n"
          "|-------------+----+----|\n"
          "| A: 1001grantR01 2026-10-30 :safe: |  |  |\n"
          "| B: 0142RETKinase :safe: |  |  |\n"
          "\n"
          "#+NAME: current-semester-projects\n"
          "| - 1452PHFSboXt 2026-11-02 :safe: |\n"
          "\n"
          "#+NAME: carried-over-projects\n"
          "| - 1006AIrxOpt |\n"
          "| - 0901idea |\n")
  "A small week with a carried-over list.")

(defmacro writing-habit-helpers-tests--with-week (&rest body)
  "Run BODY in a buffer holding the test week."
  (declare (indent 0))
  `(with-temp-buffer
     (require 'writing-habit-table-mode)
     (insert writing-habit-helpers-tests--week)
     (cl-letf (((symbol-function 'writing-habit-table--refresh) #'ignore))
       ,@body)))

(ert-deftest writing-habit-helpers/command-capture ()
  (skip-unless (featurep 'writing-schedule))
  (writing-habit-helpers-tests--with-week
    (writing-habit-helpers-capture "remember to email Sam about the RETKinase table, priority A")
    (should (string-search "| B: 0142 [#A] Email Sam about the RETKinase table |"
                           (buffer-string)))
    (should (string-search "#+NAME: grasscatcher" (buffer-string)))))

(ert-deftest writing-habit-helpers/command-triage ()
  (skip-unless (featurep 'writing-schedule))
  (let ((path (make-temp-file "wh-helpers" nil ".db")))
    (unwind-protect
        (let ((db (writing-habit-db-connect path)))
          (writing-habit-db-init db)
          (let ((a (writing-habit-db-get-or-create-project db "Z" "1006AIrxOpt" "safe"))
                (monday (writing-habit-helpers--iso
                         (- (writing-habit-helpers--abs (writing-habit-helpers--today)) 2))))
            (sqlite-execute db "INSERT INTO session(day, actual_min, project_id, source)
 VALUES (?, 90, ?, 'manual')" (list monday a)))
          (writing-habit-db-close db)
          (writing-habit-helpers-tests--with-week
            (let* ((writing-habit-default-db path)
                  (writing-habit-dispatch--last-db nil)
                  (writing-habit-coach-use-model nil)
                  (asked nil)
                  (writing-habit-coach-chooser
                   (lambda (_title _note items) (setq asked items) '(0 1))))
              (writing-habit-helpers-triage-carried)
              (should (equal asked '("Keep 1006AIrxOpt on the current list"
                                     "Drop 0901idea from the lists")))
              (should-not (string-search "0901idea" (buffer-string)))
              (let ((model (writing-habit-table-current)))
                (should (equal (mapcar #'writing-habit-lists-entry-project
                                       (writing-habit-lists-entries
                                        (writing-habit-table-after model)
                                        "current-semester-projects"))
                               '("1452PHFSboXt" "1006AIrxOpt")))))))
      (delete-file path))))

(ert-deftest writing-habit-helpers/command-number-project ()
  (skip-unless (featurep 'writing-schedule))
  (let ((root (make-temp-file "wh-root" t)))
    (unwind-protect
        (progn
          (dolist (name '("1006AIrxOpt" "1007AIrxOptBenchmarks" "2003reviewArticle"))
            (make-directory (expand-file-name name root)))
          (writing-habit-helpers-tests--with-week
            (let ((writing-habit-helpers-projects-root root)
                  (writing-habit-coach-use-model nil)
                  (writing-habit-coach-chooser (lambda (_title _note _items) '(0))))
              (let ((s (writing-habit-helpers-number-project "AIrxOpt web server")))
                (should (equal (car (writing-habit-helpers-suggestion-numbers s)) "1008"))
                (should (string-search "| - 1008AIrxOptWebServer" (buffer-string)))))))
      (delete-directory root t))))

(provide 'writing-habit-helpers-tests)
;;; writing-habit-helpers-tests.el ends here
