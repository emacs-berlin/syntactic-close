;;; general-close-python-tests.el --- Python tests

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

(defvar general-close-python-test-string-1 "\"Some Doku")
(setq general-close-python-test-string-1 "\"Some Doku")

(defvar general-close-python-test-string-2 "{[(123")
(setq general-close-python-test-string-2 "{[(123")

(ert-deftest gen-close-python-doublequoted-test ()
  (gen-test-with-python-buffer
      general-close-python-test-string-1
    (general-close)
    (should (eq (char-before) ?\"))))

(ert-deftest gen-close-python-singlequoted-test ()
  (gen-test-with-python-buffer
      "'Some Doku"
    (general-close)
    (should (eq (char-before) ?'))))

(ert-deftest gen-close-python-doublequoted-tqs-test ()
  (gen-test-with-python-buffer
      "\"\"\"Some Doku"
    (font-lock-fontify-buffer)
    (general-close)
    (should (eq (char-before) ?\"))
    (should (eq -3 (skip-chars-backward "\"")))))

(ert-deftest gen-close-python-singlequoted-tqs-test ()
  (gen-test-with-python-buffer
      "'''Some Doku"
    (font-lock-fontify-buffer)
    (general-close)
    (should (eq (char-before) ?'))
    (should (eq -3 (skip-chars-backward "'")))))

;; (ert-deftest gen-close-python-dedent-test ()
;;   (gen-test-with-python-buffer
;;       "for i in range(anzahl):
;;     klauf.pylauf()
;;     datei.write(str(spiel[i]) + \"\\n\")"
;;     (general-close)
;;     ;; (sit-for 0.1 t)
;;     ;; (should (eq 0 (current-indentation)))
;;     (should (eq 10 (char-before)))
;;     ))

(ert-deftest gen-close-python-brace-paren-bracket-test ()
  (gen-test-with-python-buffer
      general-close-python-test-string-2
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?}))))

(ert-deftest gen-close-delete-whitespace-backward-test ()
  (gen-test-with-python-buffer
      "(list ([{123 "
    (let ((gen-delete-whitespace-backward-p t))
      (general-close)
      (should (eq (point) 14)))))

(ert-deftest gen-close-python-nested-paren-test ()
  (gen-test-with-python-buffer
    "(list ([\n# {123\n# {123\n"
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest gen-close-python-string-interpolation-test-1 ()
  (gen-test-with-python-buffer "print('%(language"
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest gen-close-python-string-interpolation-test-2 ()
  (gen-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language"
    (general-close)
    (should (eq (char-before) ?'))))

(ert-deftest gen-close-python-string-interpolation-test-3 ()
  (gen-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language': \"Python"
    (general-close)
    (should (eq (char-before) ?\"))))

(ert-deftest gen-close-python-string-interpolation-test-4 ()
  (gen-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language': \"Python\", \"number\": 2"
    (general-close)
    (should (eq (char-before) ?}))))

(ert-deftest gen-close-python-string-interpolation-test-5 ()
  (gen-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language': \"Python\", \"number\": 2}"
    (general-close)
    (should (eq (char-before) ?\)))))

(provide 'general-close-python-tests)
;;; general-close-python-tests.el ends here
