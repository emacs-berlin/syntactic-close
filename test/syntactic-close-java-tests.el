;;; syntactic-close-java-tests.el --- Ruby tests -*- lexical-binding: t; -*-

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
(ert-deftest syntactic-close-java-string-test-8fM8uW ()
  (syntactic-close-test
      "public class Foo { 
    public static void main(String\[] args) 
    { 
        System\.out\.println(\"Foo Bar: \" + foo) 
    } 
} "
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (search-backward ")")
    (forward-char 1) 
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(provide 'syntactic-close-java-tests)
;;; syntactic-close-java-tests.el ends here
