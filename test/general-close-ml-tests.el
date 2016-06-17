;;; general-close-ml-tests.el ---  Tests

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

(ert-deftest general-close--nxml-close-test ()
  (general-close-test-with-nxml-buffer
    "<catalog"
    (nxml-mode)
    (general-close)
    (should (eq (char-before) ?>))))

(ert-deftest general-close--html-close-test ()
  (general-close-test-with-html-buffer
    "<html"
    (html-mode)
    (general-close)
    (should (eq (char-before) ?>))))

(provide 'general-close-ml-tests)
;;; general-close-emacs-lisp-tests.el ends here
