;;; syntactic-close-xml-tests.el --- xml tests -*- lexical-binding: t; -*-

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
(ert-deftest syntactic-close--xml-test-lnZ4WE ()
  (syntactic-close-test-with-xml-buffer
      "<body>"
    (syntactic-close)
    (goto-char (point-max)) 
    (should (looking-back "</body>" (line-beginning-position)))))

(provide 'syntactic-close-xml-tests)
;;; syntactic-close-xml-tests.el ends here
