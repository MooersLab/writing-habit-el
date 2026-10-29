;;; writing-habit-menu-tests.el --- Tests for writing-habit-menu -*- lexical-binding: t; -*-

;;; Commentary:

;; The writing-habit menus draw larger text, and other menus do not.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'writing-habit-menu)

(defun writing-habit-menu-tests--height (command)
  "Return the default face height remapped in a menu buffer for COMMAND."
  (let ((transient--prefix (transient-prefix :command command)))
    (with-temp-buffer
      (writing-habit-menu-scale-text)
      (plist-get (car (alist-get 'default face-remapping-alist)) :height))))

(ert-deftest writing-habit-menu/our-menus-are-twice-the-size ()
  (let ((writing-habit-menu-text-scale 2.0))
    (should (equal (writing-habit-menu-tests--height 'writing-habit-table-menu) 2.0))
    (should (equal (writing-habit-menu-tests--height 'writing-habit-dispatch) 2.0))))

(ert-deftest writing-habit-menu/other-menus-keep-their-size ()
  (let ((writing-habit-menu-text-scale 2.0))
    (should-not (writing-habit-menu-tests--height 'magit-dispatch))))

(ert-deftest writing-habit-menu/a-scale-of-one-leaves-the-text-alone ()
  (let ((writing-habit-menu-text-scale 1))
    (should-not (writing-habit-menu-tests--height 'writing-habit-table-menu))))

(ert-deftest writing-habit-menu/drawing-again-does-not-stack-the-scaling ()
  (let ((writing-habit-menu-text-scale 2.0)
        (transient--prefix (transient-prefix :command 'writing-habit-table-menu)))
    (with-temp-buffer
      (writing-habit-menu-scale-text)
      (writing-habit-menu-scale-text)
      (should (= 1 (cl-count-if #'consp (alist-get 'default face-remapping-alist))))
      (let ((transient--prefix (transient-prefix :command 'magit-dispatch)))
        (writing-habit-menu-scale-text))
      (should-not (cl-some #'consp (alist-get 'default face-remapping-alist))))))

(provide 'writing-habit-menu-tests)
;;; writing-habit-menu-tests.el ends here
