;;; syntactic-close-python-tests.el --- Python tests -*- lexical-binding: t; -*-

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

(ert-deftest syntactic-close-python-no-colon-test-blHQQc1 ()
  (syntactic-close-test-with-python-buffer
      "print(1)"
    (let ((orig (point)))
      (syntactic-close)
      (should (eq (char-before) ?\)))
      (should (eq (point) orig)))))

(ert-deftest syntactic-close-python-tqs-sq-test-tmeKth ()
  (syntactic-close-test-with-python-buffer
      "'''asdf"
    (goto-char (point-max))
    (syntactic-close)
    (should (char-equal (char-before) ?'))))

(ert-deftest syntactic-close-python-tqs-sq-test-eS7oPB ()
  (syntactic-close-test-with-python-buffer
      "'''asdf"
    (goto-char (point-max))
    (syntactic-close)
    (sit-for 0.1) 
    (should (char-equal (char-before (- (point) 2)) ?'))))

(ert-deftest syntactic-close-python-tqs-dq-test-10mjq3 ()
  (syntactic-close-test-with-python-buffer
      "\"\"\"asdf"
    (goto-char (point-max))
    (syntactic-close)
    (should (char-equal (char-before) ?\"))))

(ert-deftest syntactic-close-python-sq-test-UM9ZGb ()
  (syntactic-close-test-with-python-buffer
      "'asdf"
    (goto-char (point-max))
    (syntactic-close)
    (should (looking-back "'asdf'" (line-beginning-position)))))

(ert-deftest syntactic-close-python-dq-test-Zu6qkI ()
  (syntactic-close-test-with-python-buffer
      "\"asdf"
    (goto-char (point-max))
    (syntactic-close)
    (should (looking-back "\"asdf\"" (line-beginning-position)))))

(ert-deftest syntactic-close-python-f-string-test-Zu6qkI ()
  (syntactic-close-test-with-python-buffer
      "print(f\"Elementweise Addition: {m1 + m2"
    (goto-char (point-max))
    (syntactic-close)
    (should (eq (char-before) ?}))))


(ert-deftest syntactic-close-python-f-string-test-JaSpMC ()
  (syntactic-close-test-with-python-buffer
      "print(f'{val:.5f}"
    (goto-char (point-max))
    (syntactic-close)
    (should (eq (char-before) ?'))))

(ert-deftest syntactic-close-python-f-string-test-UAau25 ()
  (syntactic-close-test-with-python-buffer
      ;; print(f'{name} is {age} years old')
      "print(f'{name} is {age} years old"
    (goto-char (point-max))
    (syntactic-close)
    (should (eq (char-before) ?'))))

(provide 'syntactic-close-python-tests)

;;; syntactic-close-python-tests.el ends here
