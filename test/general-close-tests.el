;;; general-close-tests.el --- Insert closing delimiter

;; Authored by Emacs User Group Berlin

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

;; Some valid Emacs Lisp suitable for testing
;; (setq foo (list "([{123}])"))

(ert-deftest gen-close-test-1 ()
  (with-temp-buffer
    (insert "(list \"([{123")
    ;; (switch-to-buffer (current-buffer))
    (general-close)
    (should (eq (char-before) ?}))
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\"))))

(ert-deftest gen-close-test-2 ()
  (with-temp-buffer
    (insert "(list \"([\n;;{123\n;;{123\n")
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    (emacs-lisp-mode) 
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\"))))

(provide 'general-close-tests)
;;; general-close-tests.el ends here
