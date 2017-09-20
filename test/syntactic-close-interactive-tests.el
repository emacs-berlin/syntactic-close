;;; syntactic-close-interactive-tests.el --- Tests known to work when called interactively only -*- lexical-binding: t; -*-

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

;; Some tests fail in batch mode for unknown reasons. Comments welcome

;;; Code:

(ert-deftest syntactic-close-close-ruby-block-test ()
  (syntactic-close-test-with-ruby-buffer "$DBH.SELECT_ALL(\"SELECT \* FROM FOO\") DO |ROW|"
    (syntactic-close)
    (should (eq (char-before) ?d))))

(ert-deftest syntactic-close-c-comment-test ()
  (syntactic-close-test "/* The open system call "
    'c-mode
    (syntactic-close)
    (should (eq (char-before) ?/))))

(ert-deftest syntactic-close-c++-star-comment-test ()
  (syntactic-close-test "/* The open system call "
    'c++-mode
    (syntactic-close)
    (should (eq (char-before) ?/))))

(ert-deftest syntactic-close-haskell-comment-test-1 ()
  (syntactic-close-test-with-haskell-buffer
      "{- To explore this file: "
    (syntactic-close)
    (should (looking-back "-}"))))

;; (ert-deftest syntactic-close-haskell-comment-test-2 ()
;;   (syntactic-close-test-with-haskell-buffer
;;       "{- To explore this file: -}"
;;     (syntactic-close)
;;     (sit-for 0.1)
;;     (should (ar-empty-line-p))))

(ert-deftest syntactic-close-haskell-close-paren-test-1 ()
  (syntactic-close-test-with-haskell-buffer
      "add :: (Int,Int"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-python-colon-test-2 ()
  (syntactic-close-test-with-python-buffer
      "class TutorialApp(App):
    def build(self):
        return Button(text=\"Hello!\",
                      background_color=(0, 0, 1, 1)
                      font_size=150)
if __name__ == \"__main__\""
    (syntactic-close)
    (should (eq (char-before) ?:))))

(ert-deftest syntactic-close-list-single-var-test-1 ()
  (syntactic-close-test-with-haskell-buffer "potenz(x,y"
    (syntactic-close '(4))
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-sml-comment-test ()
  (syntactic-close-test "(* definition of nat"
    'sml-mode
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-sml-assignment-test-1 ()
  (syntactic-close-test "val z"
    'sml-mode
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?=))))

(ert-deftest syntactic-close-sml-no-pad-after-test ()
  (syntactic-close-test "val z = (x + y) (a +)"
    'sml-mode
    (forward-char -1)
    (syntactic-close)
    (should (eq (char-before) ?b))
    (should (eq (char-after) ?\)))))

(ert-deftest syntactic-close-sml-assignment-1 ()
  (syntactic-close-test "val z = (x + y) (a + b)"
    'sml-mode
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-sml-assignment-2 ()
  (syntactic-close-test "val z"
    'sml-mode
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?=))))

(ert-deftest syntactic-close-sml-assignment-3 ()
  (syntactic-close-test "fun foo (z : int)"
    'sml-mode
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?=))))

(ert-deftest syntactic-close-sml-tuple-separator-1 ()
  (syntactic-close-test "val x = (3"
    'sml-mode
    (syntactic-close)
    (should (eq (char-before) ?,))))

(ert-deftest syntactic-close-sml-function-1 ()
  (syntactic-close-test "fun foo (x : int)"
    'sml-mode
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?=))))

;; (ert-deftest syntactic-close-sml-function-2 ()
;;   (syntactic-close-test "fun foo"
;;     'sml-mode
;; ;;       (syntactic-close)
;;       (skip-chars-backward " \t\r\n\f")
;;       (should (eq (char-before) ?\())))

(ert-deftest syntactic-close-sml-backward-block-1 ()
  (syntactic-close-test "fun silly1 (z : int) =
  let
      val"
    'sml-mode
    (ar-smart-indentation t))
  (sml-backward-top-level)
  (should (eq (char-after) ?f)))

(ert-deftest syntactic-python-space-separator-test-1 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-python-space-separator-test-2 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c)"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-python-space-separator-test-3 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c)]"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-python-space-separator-test-4 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c)])"
    (syntactic-close)
    (should (looking-back " }"))))

(ert-deftest syntactic-close-python-brace-paren-bracket-test-1 ()
  (syntactic-close-test-with-python-buffer
      "{[(123"
    (syntactic-close)
    (sit-for 0.1)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-python-brace-paren-bracket-test-2 ()
  (syntactic-close-test-with-python-buffer
      "{[(123)"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-close-python-brace-paren-bracket-test-3 ()
  (syntactic-close-test-with-python-buffer
      "{[(123)]"
    (syntactic-close)
    (should (eq (char-before) ?}))))

(ert-deftest syntactic-python-space-separator-test-1 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-python-space-separator-test-2 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c)"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-python-space-separator-test-3 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c)]"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-python-space-separator-test-4 ()
  (syntactic-close-test-with-python-buffer
      "{ (a * [(b) - (c)])"
    (syntactic-close)
    (should (looking-back " }"))))

(ert-deftest syntactic-close-python-singlequoted-tqs-test ()
  (syntactic-close-test-with-python-buffer
      "'''Some Doku"
    (font-lock-fontify-buffer)
    (syntactic-close)
    (should (eq (char-before) ?'))
    (should (eq -3 (skip-chars-backward "'")))))

(ert-deftest syntactic-close-python-singlequoted-test ()
  (syntactic-close-test-with-python-buffer
      "'Some Doku"
    (message "%s" "syntactic-close-python-singlequoted-test")
    (syntactic-close)
    (should (eq (char-before) ?'))))

(ert-deftest syntactic-close-python-doublequoted-tqs-test ()
  (syntactic-close-test-with-python-buffer
      "\"\"\"Some Doku"
    (font-lock-fontify-buffer)
    ;; (message "%s" "Starte syntactic-close-python-doublequoted-tqs-test")
    (syntactic-close)
    (should (eq (char-before) ?\"))
    (should (eq -3 (skip-chars-backward "\"")))))

(ert-deftest syntactic-close-python-electric-test-1 ()
  (syntactic-close-test-with-python-buffer
      "['a"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (eq (char-before) ?')))))

(ert-deftest syntactic-close-python-electric-test-2 ()
  (syntactic-close-test-with-python-buffer
      "['a','b'"
    (let ((syntactic-close-electric-listify-p t))
      ;; force final closing
      (syntactic-close '(4))
      (should (eq (char-before) ?\])))))

(ert-deftest syntactic-close-python-electric-test-3 ()
  (syntactic-close-test-with-python-buffer
      "[\"a\""
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close '(4))
      (should (eq (char-before) ?\])))))

(ert-deftest syntactic-close-python-electric-test-4 ()
  (syntactic-close-test-with-python-buffer
      "def potenz(x"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-python-electric-test-5 ()
  (syntactic-close-test-with-python-buffer
      "def potenz(x,y"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close '(4))
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-python-doublequoted-test ()
  (syntactic-close-test-with-python-buffer
      "\"Some Doku"
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close-python-nested-paren-test-1 ()
  (syntactic-close-test-with-python-buffer
      "(list ([\n# {123\n# {123\n"
    (syntactic-close)
    (should (eq (char-before) ?\]))))

(ert-deftest syntactic-close-python-nested-paren-test-2 ()
  (syntactic-close-test-with-python-buffer
      "(list ([\n# {123\n# {123\n]"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-python-nested-paren-test-3 ()
  (syntactic-close-test-with-python-buffer
      "(list ([\n# {123\n# {123\n])"
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(provide 'syntactic-close-interactive-tests)
;;; syntactic-close-interactive-tests.el ends here
