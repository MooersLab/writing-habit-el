;;; writing-habit-menu.el --- Larger text in the writing-habit menus -*- lexical-binding: t; -*-

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

;; The transient menus of writing-habit, such as the table menu on
;; C-c C-; and the `writing-habit-dispatch' menu, draw their text
;; `writing-habit-menu-text-scale' times the size of the default font.
;; Other transient menus, such as those of Magit, keep their own size.
;;
;; The text is scaled in the transient buffer alone, by a relative face
;; remapping of the default face, so the rest of the frame is unchanged.
;; Transient then fits its window to the larger text.  A recent transient
;; runs `transient-setup-buffer-hook' when it sets up its buffer, and the
;; scaling hangs on that hook.  The transient that ships with Emacs 29 has
;; no such hook, so there the scaling runs just before the menu is drawn.

;;; Code:

(require 'eieio)
(require 'subr-x)
(require 'face-remap)
(require 'transient)

(defgroup writing-habit-menu nil
  "The look of the writing-habit menus."
  :group 'writing-habit
  :prefix "writing-habit-menu-")

(defcustom writing-habit-menu-text-scale 2.0
  "How many times larger than the default font the menu text is.
The value 1.0 leaves the menus at the size of the default font."
  :type 'number)

(defvar-local writing-habit-menu--cookie nil
  "The face remapping that scales the text of the transient buffer.")

(defun writing-habit-menu-ours-p (command)
  "Return non-nil when COMMAND is a writing-habit menu."
  (and (symbolp command)
       (string-prefix-p "writing-habit" (symbol-name command))))

(defun writing-habit-menu--command ()
  "Return the command of the transient menu being drawn, or nil."
  (and (boundp 'transient--prefix)
       transient--prefix
       (ignore-errors (eieio-oref transient--prefix 'command))))

(defun writing-habit-menu-scale-text ()
  "Scale the text of the current transient buffer for a writing-habit menu.
Any earlier scaling is removed first, so another package's menu drawn in
the same buffer keeps the size of the default font."
  (when writing-habit-menu--cookie
    (face-remap-remove-relative writing-habit-menu--cookie)
    (setq writing-habit-menu--cookie nil))
  (when (and (numberp writing-habit-menu-text-scale)
             (/= writing-habit-menu-text-scale 1)
             (writing-habit-menu-ours-p (writing-habit-menu--command)))
    (setq writing-habit-menu--cookie
          (face-remap-add-relative 'default :height (float writing-habit-menu-text-scale)))))

(defun writing-habit-menu--before-show (&rest _)
  "Scale the transient buffer before an older transient draws it."
  (with-current-buffer (get-buffer-create transient--buffer-name)
    (writing-habit-menu-scale-text)))

(if (boundp 'transient-setup-buffer-hook)
    (add-hook 'transient-setup-buffer-hook #'writing-habit-menu-scale-text)
  (advice-add 'transient--show :before #'writing-habit-menu--before-show))

(provide 'writing-habit-menu)
;;; writing-habit-menu.el ends here
