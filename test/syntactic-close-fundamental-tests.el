;;; syntactic-close-fundamental-tests.el --- tests in fundamental   -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Keywords: languages, lisp

;; Copyright (C) 2018  Andreas Röhler

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(ert-deftest syntactic-close--multibrace-fundamental-test-pgwBPJ ()
  (syntactic-close-test
      "{{{{asdf"
    'fundamental-mode
    syntactic-close-debug-p
    (syntactic-close '(4))
    (should (looking-back "asdf}}}}" (line-beginning-position)))))

(ert-deftest syntactic-close--multibrace-fundamental-test-R2F2xV ()
  (syntactic-close-test
      "{{{{asdf"
    'fundamental-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (looking-back "asdf}" (line-beginning-position)))))

(ert-deftest syntactic-close--multibrace-unary-fundamental-test-JM8aED ()
  (syntactic-close-test
      "{{{[[[[+asdf"
    'fundamental-mode
    syntactic-close-debug-p
    (let ((syntactic-close-unary-delimiter-chars (push 43 syntactic-close-unary-delimiter-chars)))
      (syntactic-close)
      (should (eq (char-before) ?\+)))))

(ert-deftest syntactic-close--multibrace-unary-fundamental-test-WlZ71s ()
  (syntactic-close-test
      "{{{[[[[+++asdf"
    'fundamental-mode
    syntactic-close-debug-p
    (let ((syntactic-close-unary-delimiter-chars (push 43 syntactic-close-unary-delimiter-chars)))
      (syntactic-close)
      (should (eq (char-before) ?+)))))

(ert-deftest syntactic-close--triple-grave-accent-test-WlZ71s ()
  (syntactic-close-test
      "```
foo
"   'fundamental-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (looking-back "`" (line-beginning-position)))))

(ert-deftest syntactic-close--triple-grave-accent-test-iH0kqj ()
  (syntactic-close-test
      "```
foo
"   'fundamental-mode
    syntactic-close-debug-p
    (syntactic-close '(4))
    (should (looking-back "```" (line-beginning-position)))))

(provide 'syntactic-close-fundamental-tests)
;;; syntactic-close-fundamental-tests.el ends here
