;;; writing-habit-coach.el --- A local language model as a writing coach -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Keywords: tools, convenience

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The twin of the Python coach.py, for the coach tasks that need no
;; database: drafting a block's objectives (Rule 4) and writing a debrief
;; after a block (Rule 5).  Both ports read the same prompts from
;; coach_prompts.json, so they give the same advice.
;;
;; The coach asks a model served on this computer, through the Ollama
;; interface or the OpenAI-style interface of LM Studio and llama.cpp.  An
;; address that is not on this computer is refused unless
;; `writing-habit-coach-allow-remote' is set.  The model never writes the
;; manuscript, and only the facts typed at the prompt are sent.
;;
;; Commands:
;;   `writing-habit-coach-objectives'  insert suggested objectives as an org
;;                                     checklist at point
;;   `writing-habit-coach-debrief'     show a debrief for a block in a buffer

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)

(defgroup writing-habit-coach nil
  "A local language model as a writing coach."
  :group 'tools
  :prefix "writing-habit-coach-")

(defcustom writing-habit-coach-model "llama3.2"
  "The model the coach asks."
  :type 'string)

(defcustom writing-habit-coach-url "http://localhost:11434"
  "The address of the model server."
  :type 'string)

(defcustom writing-habit-coach-api "ollama"
  "The interface of the model server: \"ollama\" or \"openai\"."
  :type '(choice (const "ollama") (const "openai")))

(defcustom writing-habit-coach-allow-remote nil
  "Non-nil allows a model server that is not on this computer."
  :type 'boolean)

(defconst writing-habit-coach--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "The directory of this library, which holds the shared prompts.")

(defvar writing-habit-coach-prompts-file
  (expand-file-name "coach_prompts.json" writing-habit-coach--dir)
  "File of the prompts the Python and Emacs Lisp ports share.")

(defun writing-habit-coach-prompts ()
  "Return the shared prompts as a hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-file writing-habit-coach-prompts-file)))

(defun writing-habit-coach-local-p (url)
  "Return non-nil when URL names this computer."
  (let ((host (url-host (url-generic-parse-url url))))
    (or (member host '("localhost" "127.0.0.1" "::1" "[::1]"))
        (string-prefix-p "127." (or host "")))))

(defun writing-habit-coach--check ()
  "Signal an error when the settings forbid a request."
  (when (string-empty-p writing-habit-coach-model)
    (user-error "No model is named for the coach"))
  (unless (member writing-habit-coach-api '("ollama" "openai"))
    (user-error "Unknown interface %s; use ollama or openai" writing-habit-coach-api))
  (unless (or writing-habit-coach-allow-remote
              (writing-habit-coach-local-p writing-habit-coach-url))
    (user-error "%s is not on this computer, so the records stay here"
                writing-habit-coach-url)))

(defun writing-habit-coach-request (task facts &optional prompts)
  "Return (ENDPOINT . BODY) for asking TASK about FACTS.
Use PROMPTS in place of the shared prompts file when it is given."
  (writing-habit-coach--check)
  (let* ((prompts (or prompts (writing-habit-coach-prompts)))
         (spec (gethash task (gethash "tasks" prompts)))
         (system (concat (gethash "rules" prompts) " " (gethash "system" spec)))
         (user (concat (gethash "instruction" spec) "\n\n" facts))
         (schema (gethash "schema" spec))
         (messages (vector `((role . "system") (content . ,system))
                           `((role . "user") (content . ,user))))
         (base (string-trim-right writing-habit-coach-url "/+"))
         body endpoint)
    (if (equal writing-habit-coach-api "ollama")
        (setq endpoint (concat base "/api/chat")
              body `((model . ,writing-habit-coach-model)
                     (messages . ,messages)
                     (stream . :json-false)
                     (options . ((temperature . 0.3)))
                     ,@(when schema `((format . ,schema)))))
      (setq endpoint (concat base "/v1/chat/completions")
            body `((model . ,writing-habit-coach-model)
                   (messages . ,messages)
                   (temperature . 0.3)
                   ,@(when schema
                       `((response_format
                          . ((type . "json_schema")
                             (json_schema . ((name . "reply")
                                             (schema . ,schema))))))))))
    (cons endpoint (encode-coding-string (json-encode body) 'utf-8))))

(defun writing-habit-coach-reply-text (reply)
  "Return the reply text from REPLY, the decoded JSON a server sent."
  (let ((text (if (equal writing-habit-coach-api "ollama")
                  (or (gethash "content" (or (gethash "message" reply)
                                             (make-hash-table)))
                      (gethash "response" reply))
                (let ((choice (car (gethash "choices" reply))))
                  (and choice (gethash "content" (gethash "message" choice)))))))
    (if (and text (not (string-empty-p (string-trim text))))
        (string-trim text)
      (error "The local model sent an empty reply"))))

(defun writing-habit-coach-ask (task facts)
  "Ask the model for TASK about FACTS and return the reply text or parsed JSON."
  (let* ((request (writing-habit-coach-request task facts))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (cdr request))
         (buffer (url-retrieve-synchronously (car request) t nil 60))
         reply)
    (unless buffer
      (error "The local model at %s did not answer" writing-habit-coach-url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n" nil 'move)
      (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'string))
        (setq reply (json-read-from-string
                     (decode-coding-string
                      (buffer-substring-no-properties (point) (point-max)) 'utf-8)))))
    (kill-buffer buffer)
    (let ((text (writing-habit-coach-reply-text reply))
          (spec (gethash task (gethash "tasks" (writing-habit-coach-prompts)))))
      (if (gethash "schema" spec)
          (let ((json-object-type 'hash-table)
                (json-array-type 'list)
                (json-key-type 'string))
            (json-read-from-string text))
        text))))

(defun writing-habit-coach-objective-facts (task project minutes)
  "Return the facts for the objectives task, from TASK, PROJECT, and MINUTES."
  (format "Task: %s\nProject: %s\nBlock length: %d minutes" task project minutes))

(defun writing-habit-coach-objectives-checklist (reply)
  "Return an org checklist from REPLY, the parsed objectives reply."
  (let ((items (seq-take (gethash "objectives" reply) 3))
        (if-then (gethash "if_then" reply)))
    (concat (mapconcat (lambda (item) (format "- [ ] %s" (string-trim item)))
                       items "\n")
            (if (and if-then (not (string-empty-p if-then)))
                (format "\n%s" (string-trim if-then))
              ""))))

;;;###autoload
(defun writing-habit-coach-objectives (task project minutes)
  "Insert one to three objectives for a block as an org checklist at point.
TASK is the vague task, PROJECT its name, and MINUTES the block length."
  (interactive
   (list (read-string "Task (such as work on the R01): ")
         (read-string "Project: ")
         (read-number "Block length in minutes: " 120)))
  (let ((reply (writing-habit-coach-ask
                "objectives" (writing-habit-coach-objective-facts task project minutes))))
    (insert (writing-habit-coach-objectives-checklist reply) "\n")))

;;;###autoload
(defun writing-habit-coach-debrief (facts)
  "Show a debrief of a block described by FACTS.
FACTS are lines such as Project, Objectives met, Self-rated focus, and
Interruptions counted, as the Python port sends them."
  (interactive (list (read-string "Describe the block (project, objectives met, focus): ")))
  (let ((text (writing-habit-coach-ask "debrief" facts)))
    (with-current-buffer (get-buffer-create "*writing-habit debrief*")
      (erase-buffer)
      (insert text "\n")
      (display-buffer (current-buffer)))))

(provide 'writing-habit-coach)
;;; writing-habit-coach.el ends here
