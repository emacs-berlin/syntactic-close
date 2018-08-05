;;; syntactic-close-emacs-lisp-tests.el ---  Tests -*- lexical-binding: t; -*-

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

;;; Commentary: A still naive implementation of a syntactic-close command

;;

;;; Code:

(ert-deftest syntactic-close--elisp-nested-bracket-paren-test-1 ()
  (syntactic-close-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-close--elisp-nested-bracket-paren-test-2 ()
  (syntactic-close-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n]"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-nested-bracket-paren-test-3 ()
  (syntactic-close-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n])"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-list-test ()
  (syntactic-close-test-with-elisp-buffer
    "(member (list 'asdf"
    (syntactic-close)
    (should (eq (char-before) ?\)))
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-interactive-spec-test ()
  (syntactic-close-test-with-elisp-buffer
    "(defun foo ()
  (interactive "
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-char-class-test-1 ()
  (syntactic-close-test-with-elisp-buffer
    "(string-match \"[[:alpha:]]"
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close--elisp-arglist-test ()
  (syntactic-close-test-with-elisp-buffer
      "(defun asdf ("
      (syntactic-close)
    (should (looking-back "asdf ()"))))

(ert-deftest syntactic-close--elisp-arglist-test ()
  (syntactic-close-test-with-elisp-buffer
      "(defun asdf ("
      (syntactic-close)
    (should (looking-back "asdf ()"))))

(ert-deftest syntactic-close--elisp-padding-test ()
  (syntactic-close-test-with-elisp-buffer
      "(defun foo ( arg"
      (syntactic-close)
    (should (eq (char-before (1- (point))) ?\ ))))

;; +BEGIN_QUOTE", "+BEGIN_VERSE", "+BEGIN_EXAMPLE" and "+BEGIN_SRC" to syntactic close? (Akin to C-] in latex-mode, when closing environments) So that would add a corresponding "+END_SRC", "+END_QUOT

(ert-deftest syntactic-close--elisp-org-quote-test ()
  (syntactic-close-test-with-elisp-buffer
      "#+BEGIN_QUOTE
asdf"
      (org-mode)
      (syntactic-close)
    (should (looking-back "#\\+END_QUOTE" (line-beginning-position)))))

(ert-deftest syntactic-close--elisp-org-src-test ()
  (syntactic-close-test-with-elisp-buffer
      "#+BEGIN_SRC
asdf"
      (org-mode)
      (syntactic-close)
    (should (looking-back "#\\+END_SRC" (line-beginning-position)))))

(ert-deftest syntactic-close--ogham-feather-mark-close-test ()
  (syntactic-close-test-with-elisp-buffer
      "?\᚛"
      (should (char-equal ?\᚜ (syntactic-close--return-complement-char-maybe (char-before))))))

(ert-deftest syntactic-close--multidelim-test-1 ()
  (syntactic-close-test-with-elisp-buffer
      "{{{{asdf"
      (syntactic-close)
    (should (looking-back "}}}}" (line-beginning-position)))))

(ert-deftest syntactic-close--escaped-test-1 ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "\"\\(^ sadf"
      (let ((syntactic-close-generic-p t))
	(syntactic-close)
	(should (looking-back "\\\\)" (line-beginning-position))))))

(ert-deftest syntactic-close--escaped-test-2 ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "\"\\(^ *\\|^Passwort: *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th"
      (let ((syntactic-close-generic-p t))
	(syntactic-close 3)
	(should (looking-back "\\\\)" (line-beginning-position))))))

(ert-deftest syntactic-close--escaped-test-3 ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "\"\\(^ *\\|^Passwort: *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th"
      (let ((syntactic-close-generic-p t))
	(syntactic-close '(4))
	(should (eq (char-before) ?\")))))

(ert-deftest syntactic-close--escaped-test-4 ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "\"\\(^ *\\|^Passwort: *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)\\)\\)\\)\\)\""
      (let ((orig (point)) 
	    (syntactic-close-generic-p t))
	(syntactic-close)
	(should (eq orig (point))))))

;;; syntactic-close-emacs-lisp-tests.el ends here
