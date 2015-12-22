;;; general-close-js-tests.el ---

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

(defvar general-close-js-test-string-1 " $(document).ready(function() {

          $('.nav-tabs-custom-2').tabs();

          $('.table').delegate('td','mouseover mouseleave',function(e) {
              if (e.type == 'mouseover') {
                  $(this).addClass('hover")

(setq general-close-js-test-string-1 " $(document).ready(function() {

          $('.nav-tabs-custom-2').tabs();

          $('.table').delegate('td','mouseover mouseleave',function(e) {
              if (e.type == 'mouseover') {
                  $(this).addClass('hover")

(ert-deftest general-close-close-js-test-1 ()
  (general-close-test-with-js-buffer
      general-close-js-test-string-1
    (skip-chars-backward " \t\r\n\f")
    (general-close)
    (should (eq (char-before) ?'))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\;))
    (general-close)
    (should (eq (char-before) ?\}))
    (general-close)
    (should (eq (char-before) ?\}))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\;))
    (general-close)
    (should (eq (char-before) ?\}))
    (general-close)
    (should (eq (char-before) ?\)))
    ;; (general-close)
    ;; (should (eq (char-before) ?\;))
))

(ert-deftest general-close-close-js-test-2 ()
  (general-close-test-with-js-buffer
      "function foo(a,b,c,d) {
if ( (a == b) || (c == d"

    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\;))
    (general-close)
    (should (eq (char-before) ?}))))

(ert-deftest general-close-web-mode-test ()
  (general-close-test-with-js-buffer
    "$(document).ready(function() {
    var test = 0 []"
    (setq major-mode 'web-mode)
    (general-close)
    (should (eq (char-before) ?\;))
    (general-close)
    (should (eq (char-before) ?\}))
    (general-close)
    (should (eq (char-before) ?\)))))
    


(provide 'general-close-js-tests)
;;; general-close-js-tests.el ends here
