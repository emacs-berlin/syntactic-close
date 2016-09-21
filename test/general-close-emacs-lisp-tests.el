;;; general-close-emacs-lisp-tests.el ---  Tests -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: A still naive implementation of a general-close command

;;

;;; Code:

(ert-deftest general-close--elisp-nested-bracket-paren-test ()
  (general-close-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n"
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest general-close--elisp-list-test ()
  (general-close-test-with-elisp-buffer
    "(member (list 'asdf"
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest general-close--elisp-interactive-spec-test ()
  (general-close-test-with-elisp-buffer
    "(defun foo ()
  (interactive "
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest general-close--char-class-test ()
  (general-close-test-with-elisp-buffer
    "(string-match \"[[:alpha:]"
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\"))))

(ert-deftest general-close--arglist-test ()
  (general-close-test-with-elisp-buffer
      "(defun asdf"
      (general-close)
    (should (looking-back "asdf ()"))))

(provide 'general-close-emacs-lisp-tests)
;;; general-close-emacs-lisp-tests.el ends here
