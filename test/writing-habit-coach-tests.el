;;; writing-habit-coach-tests.el --- ERT tests for the coach -*- lexical-binding: t; -*-

;;; Commentary:

;; The request bodies, the reply parsing, and the local-only rule.  No test
;; reaches a model server.

;;; Code:

(require 'ert)
(require 'json)
(require 'writing-habit-coach)

(defun writing-habit-coach-tests--decode (string)
  "Decode the JSON STRING into hash tables."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string (decode-coding-string string 'utf-8))))

(ert-deftest writing-habit-coach/prompts-cover-every-task ()
  (let ((tasks (gethash "tasks" (writing-habit-coach-prompts))))
    (dolist (name '("objectives" "grasscatcher" "debrief" "weeks" "size"))
      (should (gethash name tasks)))
    (should (string-match-p "never write" (gethash "rules" (writing-habit-coach-prompts))))))

(ert-deftest writing-habit-coach/ollama-request ()
  (let* ((writing-habit-coach-api "ollama")
         (writing-habit-coach-url "http://localhost:11434/")
         (request (writing-habit-coach-request "objectives" "Task: R01"))
         (body (writing-habit-coach-tests--decode (cdr request))))
    (should (equal (car request) "http://localhost:11434/api/chat"))
    (should (equal (gethash "model" body) "llama3.2"))
    (should (eq (gethash "stream" body) :json-false))
    (should (gethash "format" body))
    (should (string-suffix-p "Task: R01"
                             (gethash "content" (nth 1 (gethash "messages" body)))))))

(ert-deftest writing-habit-coach/openai-request-without-schema ()
  (let* ((writing-habit-coach-api "openai")
         (writing-habit-coach-url "http://127.0.0.1:1234")
         (request (writing-habit-coach-request "debrief" "Project: A"))
         (body (writing-habit-coach-tests--decode (cdr request))))
    (should (equal (car request) "http://127.0.0.1:1234/v1/chat/completions"))
    (should-not (gethash "response_format" body))))

(ert-deftest writing-habit-coach/remote-refused ()
  (let ((writing-habit-coach-url "https://api.example.com")
        (writing-habit-coach-allow-remote nil))
    (should-error (writing-habit-coach-request "size" "x") :type 'user-error))
  (let ((writing-habit-coach-url "https://api.example.com")
        (writing-habit-coach-allow-remote t))
    (should (writing-habit-coach-request "size" "x"))))

(ert-deftest writing-habit-coach/reply-text-both-interfaces ()
  (let ((writing-habit-coach-api "ollama"))
    (should (equal (writing-habit-coach-reply-text
                    (writing-habit-coach-tests--decode
                     "{\"message\": {\"content\": \" Done. \"}}"))
                   "Done.")))
  (let ((writing-habit-coach-api "openai"))
    (should (equal (writing-habit-coach-reply-text
                    (writing-habit-coach-tests--decode
                     "{\"choices\": [{\"message\": {\"content\": \"Ok\"}}]}"))
                   "Ok"))
    (should-error (writing-habit-coach-reply-text
                   (writing-habit-coach-tests--decode "{\"choices\": []}")))))

(ert-deftest writing-habit-coach/objectives-checklist ()
  (let ((reply (writing-habit-coach-tests--decode
                "{\"objectives\": [\"Draft aim 2\", \"Find 3 citations\", \"c\", \"d\"],
                  \"if_then\": \"If email calls, then I will park it.\"}")))
    (should (equal (writing-habit-coach-objectives-checklist reply)
                   (concat "- [ ] Draft aim 2\n- [ ] Find 3 citations\n- [ ] c\n"
                           "If email calls, then I will park it.")))))

(provide 'writing-habit-coach-tests)
;;; writing-habit-coach-tests.el ends here
