;;; syntactic-close-emacs-lisp-tests.el ---  Tests -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Andreas Röhler, <andreas.roehler@online.de>
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

(ert-deftest syntactic-close--elisp-nested-bracket-paren-test-rNRF1C ()
  (syntactic-close-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-close--elisp-nested-bracket-paren-test-cNi9NK ()
  (syntactic-close-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n]"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-nested-bracket-paren-test-O2UNEs ()
  (syntactic-close-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n])"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-list-4ETJll ()
  (syntactic-close-test-with-elisp-buffer
    "(member (list 'asdf"
    (syntactic-close)
    (should (eq (char-before) ?\)))
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-interactive-spec-aBfETo ()
  (syntactic-close-test-with-elisp-buffer
    "(defun foo ()
  (interactive "
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-char-class-test-qMPgJc ()
  (syntactic-close-test-with-elisp-buffer
    "(string-match \"[[:alpha:]]"
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close--elisp-char-class-test-RTbDdh ()
  (syntactic-close-test-with-elisp-buffer
    "(string-match \"[[:alpha"
    (syntactic-close)
    (should (eq (char-before) ?:))))

(ert-deftest syntactic-close--elisp-char-class-test-ZKucTB ()
  (syntactic-close-test-with-elisp-buffer
    "(string-match \"[[:alpha:"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-close--elisp-char-class-test-QylOuh ()
  (syntactic-close-test-with-elisp-buffer
    "(string-match \"[[:alpha:]"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-close--elisp-arglist-l91fpm ()
  (syntactic-close-test-with-elisp-buffer
      "(defun asdf ("
      (syntactic-close)
    (should (looking-back "asdf ()" (line-beginning-position)))))

(ert-deftest syntactic-close-electric-delete-whitespace-test-x7Ax7w ()
  (syntactic-close-test-with-elisp-buffer
      "(defun asdf ( "
      (let ((syntactic-close-electric-delete-whitespace-p t))
	(syntactic-close)
	(should (looking-back "asdf ()")))))

(ert-deftest syntactic-close-electric-delete-whitespace-test-p0vsKD ()
  (syntactic-close-test-with-elisp-buffer
      "(defun asdf ( "
      (let ((syntactic-close-electric-delete-whitespace-p nil))
	(syntactic-close)
	(should (looking-back "asdf (  )")))))

(ert-deftest syntactic-close--elisp-padding-4SCxQH ()
  (syntactic-close-test-with-elisp-buffer
      "(defun foo ( arg"
      (syntactic-close)
    (should (eq (char-before) ?\)))
    (should (eq (char-before (1- (point))) ?\ ))
    ))

;; +BEGIN_QUOTE", "+BEGIN_VERSE", "+BEGIN_EXAMPLE" and "+BEGIN_SRC" to syntactic close? (Akin to C-] in latex-mode, when closing environments) So that would add a corresponding "+END_SRC", "+END_QUOT


(ert-deftest syntactic-close--ogham-feather-mark-close-lVxPDf ()
  (syntactic-close-test-with-elisp-buffer
      "?\᚛"
      (should (char-equal ?\᚜ (syntactic-close--return-complement-char-maybe (char-before))))))

(ert-deftest syntactic-close--multidelim-test-tIMXsr ()
  (syntactic-close-test-with-elisp-buffer
      "{{{{asdf"
      (syntactic-close)
    (should (looking-back "}" (line-beginning-position)))))

(ert-deftest syntactic-close--multidelim-test-lvyjjU ()
  (syntactic-close-test-with-elisp-buffer
      "{{{{asdf}"
      (goto-char (point-max))
      (syntactic-close)
    (should (looking-back "}}" (line-beginning-position)))))

(ert-deftest syntactic-close--escaped-test-nm0AqK ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "\"\\(^ sadf"
      (let ((syntactic-close-generic-p t))
        (goto-char (point-max))
        (skip-chars-backward " \t\r\n\f")
	(syntactic-close)
	(should (looking-back "\\\\)" (line-beginning-position))))))

(ert-deftest syntactic-close--escaped-test-32G2OA ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "\"\\\\(^ *\\\\|^Passwort: *\\\\|\\\\( SMB\\\\|'s\\\\|Bad\\\\|CVS\\\\|Enter\\\\(?: \\\\(?:\\\\(?:sam\\\\|th"
      (let ((syntactic-close-generic-p t))
	(syntactic-close)
	(should (looking-back "\\\\)" (line-beginning-position))))))

(ert-deftest syntactic-close--escaped-test-PbxFlK ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "\"\\(^ *\\|^Passwort: *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th"
      (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
      (let ((syntactic-close-unary-delimiter-chars (list ?` ?\" ?+)))
	(syntactic-close '(4))
	(should (eq (char-before) ?\")))))


(ert-deftest syntactic-close--tqs-test-pBcUxG ()
  ;; comint-password-prompt-regexp
  (syntactic-close-test-with-elisp-buffer
      "(search-forward \"'''"
      (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f") 
      (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close-close-elisp-nested-bracket-paren-2e8EOm ()
  (syntactic-close-test
    "(list ([\n;;{123\n;;{123\n"
    'emacs-lisp-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (eq (char-before) ?\]))
    (syntactic-close)
    (should (eq (char-before) ?\)))
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close--elisp-char-class-test-QylOuh ()
  (syntactic-close-test-with-elisp-buffer
    "(string-match \"[[:alpha:]"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-close--elisp-char-class-test-Ru2ZUr ()
  (syntactic-close-test-with-elisp-buffer
      "(string-match \"[[:alpha:]]\""
    (syntactic-close)
    (should (eq (char-before) ?\)))))


(provide 'syntactic-close-emacs-lisp-tests)
;;; syntactic-close-emacs-lisp-tests.el ends here
