;;; syntactic-close-sml-tests.el --- -*- lexical-binding: t; -*-

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

;; Test succeeds at syntactic-close-interactive-tests.el but fails in batch-mode

;; (ert-deftest syntactic-close-sml-comment-test ()
;;   (syntactic-close-test "(* definition of nat"
;;     'sml-mode
;;     'syntactic-close-debug-p
;;     (syntactic-close)
;;     (should (eq (char-before) ?\)))))


(provide 'syntactic-close-sml-tests)
;;; syntactic-close-sml-tests.el ends here
