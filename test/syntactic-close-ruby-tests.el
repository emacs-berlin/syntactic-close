;;; syntactic-close-ruby-tests.el --- Ruby tests -*- lexical-binding: t; -*-

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
(ert-deftest syntactic-close-close-ruby-string-test ()
  (syntactic-close-test
      "def deliver(from: \"A\", to: nil, via: \"mail"
    'ruby-mode
    syntactic-close-debug-p
    (goto-char (point-max)) 
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close-close-ruby-paren-test ()
  (syntactic-close-test
      "def deliver(from: \"A\", to: nil, via: \"mail\""
    'ruby-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-close-ruby-string-interpolation-test-2 ()
  (syntactic-close-test
      "def deliver(from: \"A\", to: nil, via: \"mail\")
  \"Sending from #{from} to #{to} via #{via}"
    'ruby-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close-ruby-block-close-GZBbFA ()
  (syntactic-close-test
      "values.each do |value|
  break if value.even?"
    'ruby-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (looking-back "end" (line-beginning-position)))))

(ert-deftest syntactic-close-ruby-block-close-oP7aaw ()
  (syntactic-close-test
      "more_nested_array.each do |element|
  element.each do |inner_element|
    inner_element << \"test\"
"
    'ruby-mode
    syntactic-close-debug-p
    (goto-char (point-max)) 
    (syntactic-close)
    (should (looking-back "end" (line-beginning-position)))
    (should (eq (current-indentation) 2))))

(ert-deftest syntactic-close-ruby-block-close-XhSYAJ ()
  (syntactic-close-test
      "more_nested_array.each do |element|
  element.each do |inner_element|
    inner_element << \"test\"
  end"
    'ruby-mode
    syntactic-close-debug-p
    (goto-char (point-max)) 
    (syntactic-close)
    (should (looking-back "end" (line-beginning-position)))
    (should (eq (current-indentation) 0))))

(ert-deftest syntactic-close-ruby-test-gszMLh ()
  (syntactic-close-test
      ;; "puts \"Hello #{name}!\""
      "puts \"Hello #{name"
    'ruby-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (char-equal (char-before) ?}))))

(ert-deftest syntactic-close-ruby-test-yKdmju ()
  (syntactic-close-test
      ;; "puts \"Hello #{name}!\""
      "puts \"Hello #{name}!"
    'ruby-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (char-equal (char-before) ?\"))))

(ert-deftest syntactic-close-mode-ruby-test-VvXkWj ()
  (syntactic-close-test
      ;; "hi(\"Matz\")"
      "hi(\"Matz"
    'ruby-mode
    syntactic-close-debug-p
    (syntactic-close)
    (should (char-equal (char-before) ?\"))))

(ert-deftest syntactic-close-mode-ruby-mode-test-3Sumx6 ()
  (syntactic-close-test
      ;; "hi(\"Matz\")"
      "hi(\"Matz\""
    'ruby-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (syntactic-close)
    (should (char-equal (char-before) ?\)))))

(provide 'syntactic-close-ruby-tests)
;;; syntactic-close-ruby-tests.el ends here
