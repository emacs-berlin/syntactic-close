;;; general-close-tests.el ---  Tests

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
  (gen-test-with-python-buffer
      "(list ([{123"
    (general-close)
    (should (eq (char-before) ?}))
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest gen-close-test-2 ()
  (gen-test-with-elisp-buffer
    "(list ([\n;;{123\n;;{123\n"
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest gen-close-test-3 ()
  (with-temp-buffer
    (insert "(list ([{123")
    ;; python-mode sets "{" to parenthesis syntax
    (python-mode)
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    (general-close)
    (should (eq (char-before) ?}))
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest gen-close-test-4 ()
  (with-temp-buffer
    (insert "(list ([\n# {123\n# {123\n")
    (python-mode)
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    ;; python-mode sets "{" to parenthesis syntax
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest gen-close-doublequoted-test ()
  (with-temp-buffer
    (insert "\"Some Doku")
    (python-mode)
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    ;; python-mode sets "{" to parenthesis syntax
    (general-close)
    (should (eq (char-before) ?\"))))

(ert-deftest gen-close-singlequoted-test ()
  (with-temp-buffer
    (insert "'Some Doku")
    (python-mode)
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    ;; python-mode sets "{" to parenthesis syntax
    (general-close)
    (should (eq (char-before) ?'))))

(ert-deftest gen-close-doublequoted-tqs-test ()
  (with-temp-buffer
    (insert "\"\"\"Some Doku")
    (python-mode)
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    ;; python-mode sets "{" to parenthesis syntax
    (general-close)
    (should (eq (char-before) ?\"))
    (should (eq -3 (skip-chars-backward "\"")))))

(ert-deftest gen-close-singlequoted-tqs-test ()
  (with-temp-buffer
    (insert "'''Some Doku")
    (python-mode)
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    ;; python-mode sets "{" to parenthesis syntax
    (general-close)
    (should (eq (char-before) ?'))
    (should (eq -3 (skip-chars-backward "'")))))

(ert-deftest gen-close-python-dedent-test ()
  (gen-test-with-python-buffer
      "for i in range(anzahl):
    klauf.pylauf()
    datei.write(str(spiel[i]) + \"\\n"
    (general-close)
    (should (eq (char-before) ?\"))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq 0 (current-indentation)))))


(provide 'general-close-tests)
;;; general-close-tests.el ends here
