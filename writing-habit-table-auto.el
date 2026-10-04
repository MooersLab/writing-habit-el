;;; writing-habit-table-auto.el --- Turn on the table mode in schedule-code files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 0.0.0
;; Keywords: convenience, tools, org
;; URL: https://github.com/MooersLab/writing-habit-el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

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

;; This small file decides, for every org buffer, whether to turn on
;; `writing-habit-table-mode'.  It is kept apart from the mode so that
;; opening an ordinary org file loads only the schedule-code decoder.  The
;; mode itself, with writing-schedule.el, loads only for a file whose name
;; is a schedule code, such as 4gAeA-gW.org or 2026-01-19_4gAeA-gW.org.

;;; Code:

(require 'writing-habit-name)
(require 'writing-habit-plan)

(declare-function writing-habit-table-mode "writing-habit-table-mode" (&optional arg))
(declare-function writing-habit-table-current "writing-habit-table-mode" ())
(declare-function writing-habit-table-columns "writing-habit-table" (table))

(defgroup writing-habit-table nil
  "Editing weekly block tables."
  :group 'writing-habit
  :prefix "writing-habit-table-")

(defcustom writing-habit-table-mode-auto t
  "When non-nil, turn on `writing-habit-table-mode' in schedule-code files.
A schedule-code file is an org file whose name decodes as a schedule code
and that holds a weekly table."
  :type 'boolean
  :group 'writing-habit-table)

(defun writing-habit-table-schedule-file-p (file)
  "Return non-nil when the name of FILE decodes as a schedule code."
  (and file
       (string-suffix-p ".org" file)
       (let ((code (writing-habit-plan--schedule-code file)))
         (condition-case nil (and (writing-habit-name-decode code) t)
           (error nil)))))

;;;###autoload
(defun writing-habit-table-mode-maybe ()
  "Turn on `writing-habit-table-mode' in a schedule-code file with a table.
This runs from `org-mode-hook' while `writing-habit-table-mode-auto' is
non-nil.  It does nothing when writing-schedule.el is not installed."
  (when (and writing-habit-table-mode-auto
             (writing-habit-table-schedule-file-p buffer-file-name)
             (require 'writing-habit-table-mode nil t)
             (ignore-errors (writing-habit-table-columns (writing-habit-table-current))))
    (writing-habit-table-mode 1)))

;;;###autoload
(add-hook 'org-mode-hook #'writing-habit-table-mode-maybe)

(provide 'writing-habit-table-auto)
;;; writing-habit-table-auto.el ends here
