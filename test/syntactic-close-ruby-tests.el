;;; syntactic-close-ruby-tests.el --- Ruby tests -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Keywords: lisp

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

;;; Commentary:

;;

;;; Code:
(ert-deftest syntactic-close-close-ruby-string-test ()
  (syntactic-close-test-with-ruby-buffer "def deliver(from: \"A\", to: nil, via: \"mail"
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close-close-ruby-paren-test ()
  (syntactic-close-test-with-ruby-buffer "def deliver(from: \"A\", to: nil, via: \"mail\""
    (syntactic-close)
    ;; (sit-for 1 t)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-close-ruby-string-interpolation-test-2 ()
  (syntactic-close-test-with-ruby-buffer "def deliver(from: \"A\", to: nil, via: \"mail\")
  \"Sending from #{from} to #{to} via #{via}"
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(provide 'syntactic-close-ruby-tests)
;;; syntactic-close-ruby-tests.el ends here
