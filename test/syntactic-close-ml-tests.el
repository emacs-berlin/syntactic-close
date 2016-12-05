;;; syntactic-close-ml-tests.el ---  Tests -*- lexical-binding: t; -*-

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

(ert-deftest syntactic-close--nxml-close-test-1 ()
  (syntactic-close-test-with-nxml-buffer
    "<catalog"
    (nxml-mode)
    (syntactic-close)
    (should (eq (char-before) ?>))))

(ert-deftest syntactic-close--html-close-test ()
  (syntactic-close-test-with-html-buffer
    "<html"
    (html-mode)
    (syntactic-close)
    (should (eq (char-before) ?>))))

(ert-deftest syntactic-close--nxml-close-test-2 ()
  (syntactic-close-test-with-nxml-buffer
      "<CATALOG>
  <PLANT>
    <COMMON>Bloodroot</COMMON>
    <BOTANICAL>Sanguinaria canadensis"
    (syntactic-close)
    (save-excursion
      (forward-char -1)
      (skip-syntax-backward "w")
      (should (looking-at "BOTANICAL")))
    (syntactic-close)
    (forward-char -1)
    (skip-syntax-backward "w")
    (should (looking-at "PLANT"))))

(ert-deftest syntactic-close--nxml-close-comment-test ()
  (syntactic-close-test-with-nxml-buffer
      "<!-- <CATALOG> "
    (syntactic-close)
    (should (eq (char-before) ?>))))

(provide 'syntactic-close-ml-tests)
;;; syntactic-close-emacs-lisp-tests.el ends here
