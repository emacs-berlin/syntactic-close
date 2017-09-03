;;; syntactic-close-js-tests.el --- -*- lexical-binding: t; -*-

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

(defvar syntactic-close-js-test-string-1 " $(document).ready(function() {

          $('.nav-tabs-custom-2').tabs();

          $('.table').delegate('td','mouseover mouseleave',function(e) {
              if (e.type == 'mouseover') {
                  $(this).addClass('hover")

(setq syntactic-close-js-test-string-1 " $(document).ready(function() {

          $('.nav-tabs-custom-2').tabs();

          $('.table').delegate('td','mouseover mouseleave',function(e) {
              if (e.type == 'mouseover') {
                  $(this).addClass('hover")

(ert-deftest syntactic-close-close-js-test-1 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
    (syntactic-close)
    (should (eq (char-before) ?'))))

(ert-deftest syntactic-close-close-js-test-2 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "'")
      (syntactic-close)
      (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-close-js-test-3 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "')")
      (syntactic-close)
      (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-close-js-test-4 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "');")
      (syntactic-close)
      (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-close-js-test-5 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "');\n")
      (insert (make-string 14 32))
      (insert "}")
      (syntactic-close)
      (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-close-js-test-5 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "');\n")
      (insert (make-string 14 32))
      (insert "}")
      (syntactic-close)
      (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-close-js-test-6 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "');\n")
      (insert (make-string 14 32))
      (insert "}\n")
      (insert (make-string 10 32))
      (insert "}")
      (syntactic-close)
      (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-close-js-test-7 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "');\n")
      (insert (make-string 14 32))
      (insert "}\n")
      (insert (make-string 10 32))
      (insert "})")
      (syntactic-close)
      (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-close-js-test-8 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "');\n")
      (insert (make-string 14 32))
      (insert "}\n")
      (insert (make-string 10 32))
      (insert "});")
      (syntactic-close)
      (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-close-js-test-9 ()
  (syntactic-close-test-with-js-buffer
      syntactic-close-js-test-string-1
      (insert "');\n")
      (insert (make-string 14 32))
      (insert "}\n")
      (insert (make-string 10 32))
      (insert "});\n")
      (insert (make-string 2 32))
      (insert "}")
      (syntactic-close)
      (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-close-js-function-test-1 ()
  (syntactic-close-test-with-js-buffer
      "function foo(a,b,c,d) {
if ( (a == b) || (c == d"
    (syntactic-close)
    (should (eq (char-before) ?\)))
    (syntactic-close)
    (should (eq (char-before) ?\)))
    (syntactic-close)
    (should (eq (char-before) ?\;))
    (syntactic-close)
    (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-web-mode-test ()
  (syntactic-close-test-with-js-buffer
    "$(document).ready(function() {
    var test = 0 []"
    (setq major-mode 'web-mode)
    (syntactic-close)
    (should (eq (char-before) ?\;))
    (syntactic-close)
    (should (eq (char-before) ?\}))
    (syntactic-close)
    (should (eq (char-before) ?\)))))


(ert-deftest syntactic-close-padded-brace-test ()
  (syntactic-close-test-with-js-buffer
      "{ display: inline;"
    (syntactic-close)
    (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-js-element-test ()
  (syntactic-close-test-with-js-buffer
      "<script"
    (syntactic-close)
    (should (eq (char-before) ?>))))

(ert-deftest syntactic-close-js-assignment-test ()
  (syntactic-close-test-with-js-buffer
      "var Counter = 1"
    (syntactic-close)
    (should (eq (char-before) ?\;))))


(provide 'syntactic-close-js-tests)
;;; syntactic-close-js-tests.el ends here
