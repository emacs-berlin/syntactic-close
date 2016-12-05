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

(ert-deftest syntactic-close-c-nesting-comment-test ()
  (syntactic-close-test "/* The open system call "
    'c-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (should (eq (char-before) ?/))))

(ert-deftest syntactic-close-c++-nesting-comment-test ()
  (syntactic-close-test "/* The open system call "
    'c++-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (should (eq (char-before) ?/))))

(ert-deftest syntactic-close-haskell-comment-test-1 ()
  (syntactic-close-test-with-haskell-buffer
      "{- To explore this file: "
    'syntactic-close-debug-p
    (syntactic-close)
    (should (looking-back "-}"))))

(ert-deftest syntactic-close-haskell-comment-test-2 ()
  (syntactic-close-test-with-haskell-buffer
      "{- To explore this file: -}"
    'syntactic-close-debug-p
    (syntactic-close)
    (sit-for 0.1)
    (should (ar-empty-line-p))))

(ert-deftest syntactic-close-haskell-close-paren-test-1 ()
  (syntactic-close-test-with-haskell-buffer
      "add :: (Int,Int"
    (let (syntactic-close-electric-listify-p)
      (syntactic-close)
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-haskell-right-arrow-test-1 ()
  (syntactic-close-test-with-haskell-buffer "asdf :: Int"
    (let (syntactic-close-electric-listify-p)
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?>)))))

(ert-deftest syntactic-close-haskell-right-arrow-test-2 ()
  (syntactic-close-test-with-haskell-buffer "add :: (Int,Int)"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?>)))))

(ert-deftest syntactic-close-haskell-close-paren-test-2 ()
  (syntactic-close-test-with-haskell-buffer "add :: (Int,"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "Int,Int,")))))

(ert-deftest syntactic-close-haskell-close-paren-test-2a ()
  (syntactic-close-test-with-haskell-buffer "add :: (Int"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-haskell-assign-test-1 ()
  (syntactic-close-test-with-haskell-buffer "asdf "
    (let (syntactic-close-electric-listify-p)
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?=)))))

(ert-deftest syntactic-close-haskell-asign-test-2 ()
  (syntactic-close-test-with-haskell-buffer "asdf :: Int -> Int
asdf n"
    (let (syntactic-close-electric-listify-p)
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?=)))))

(ert-deftest syntactic-close-haskell-asign-test-3 ()
  (syntactic-close-test-with-haskell-buffer "asdf :: Int -> [Int]
asdf n = ["
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back syntactic-close-default-argument-1)))))

(ert-deftest syntactic-close-haskell-asign-test-4 ()
  (syntactic-close-test-with-haskell-buffer "asdf :: Int -> [Int]
asdf "
    (let (syntactic-close-electric-listify-p)
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?=)))))

(ert-deftest syntactic-close-haskell-concat-test ()
  ;; indent s = "    " ++ s
  (syntactic-close-test-with-haskell-buffer "indent s = \"asdf\""
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\+))))

(ert-deftest syntactic-close-haskell-typedef-test ()
  (syntactic-close-test-with-haskell-buffer "signum :: Int ->"
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?t))))

(ert-deftest syntactic-close-haskell-default-type-test ()
  (syntactic-close-test-with-haskell-buffer "signum :: "
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (looking-back syntactic-close-default-type))))

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

(ert-deftest syntactic-close-python-colon-test-3 ()
  (syntactic-close-test-with-python-buffer
      "class TutorialApp(App):
    def build(self):
        return Button(text=\"Hello!\",
                      background_color=(0, 0, 1, 1)
                      font_size=150)
if __name__ == \"__main__\":"
    (syntactic-close)
    (should (eq 4 (current-indentation)))))

(ert-deftest syntactic-close-list-comprehension-test-1 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(asdb,"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (looking-back "asdb,asdb," (line-beginning-position))))))

(ert-deftest syntactic-close-list-comprehension-test-2 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(asdb,cdfg"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-list-comprehension-test-3 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(x"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (eq (char-before) ?,)))))

(ert-deftest syntactic-close-list-comprehension-test-4 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(a)"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (eq (char-before) ?\])))))

(ert-deftest syntactic-close-list-comprehension-test-5 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(a,"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (looking-back "a,b,")))))

(ert-deftest syntactic-close-list-comprehension-test-6 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(a,b)] |"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?a)))))

(ert-deftest syntactic-close-list-comprehension-test-7 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(x,y)] |"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?x)))))

(ert-deftest syntactic-close-list-comprehension-test-8 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(abd,def)] |"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "abd")))))

(ert-deftest syntactic-close-list-comprehension-test-9 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(a,b)] | a"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest syntactic-close-list-comprehension-test-10 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(x,y)] | x"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest syntactic-close-list-comprehension-test-11 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(abd,def)] | abd"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest syntactic-close-list-comprehension-test-12 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(asdb, asdb"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-list-comprehension-test-13 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(abd,def)] | abd <- "
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\[)))))

(ert-deftest syntactic-close-list-comprehension-test-14 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(abd,def)] | abd <- [1..3 "
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\])))))

(ert-deftest syntactic-close-list-comprehension-test-15 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(x,y)] | x <-[1..3]"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?,)))))

(ert-deftest syntactic-close-list-comprehension-test-16 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(x,y)] | x <-[1..3],"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?y)))))

(ert-deftest syntactic-close-list-comprehension-test-17 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "[(x,y)] | x <-[1..3], y"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest syntactic-close-list-comprehension-test-18 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (syntactic-close-test-with-haskell-buffer "potenz(x,y"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close '(4))
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-list-single-var-test-1 ()
  (syntactic-close-test-with-haskell-buffer "potenz(x,y"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close '(4))
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\))))))

(ert-deftest syntactic-close-type-test-1 ()
  (syntactic-close-test-with-haskell-buffer "type Radius = Float
type Width  = Float
type Height = Float

Date Shape = Circle Radius
           | Rect Width Height

area "
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?:)))))

(ert-deftest syntactic-close-haskell-typelist-1 ()
  (syntactic-close-test-with-haskell-buffer
      "zip :: (["
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back syntactic-close-default-argument-1)))))

(ert-deftest syntactic-close-haskell-typelist-2 ()
  (syntactic-close-test-with-haskell-buffer
      "zip :: ([x"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\])))))

(ert-deftest syntactic-close-haskell-typelist-2 ()
  (syntactic-close-test-with-haskell-buffer
      "zip :: ([x"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\])))))

(ert-deftest syntactic-close-haskell-typelist-3 ()
  (syntactic-close-test-with-haskell-buffer
      "zip :: ([x],"
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\[)))))

(ert-deftest syntactic-close-haskell-typelist-4 ()
  (syntactic-close-test-with-haskell-buffer
      "zip :: ([x],["
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?y)))))

(ert-deftest syntactic-close-sml-comment-test ()
  (syntactic-close-test "(* definition of nat"
    'sml-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-sml-assignment-test-1 ()
  (syntactic-close-test "val z"
    'sml-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?=))))

(ert-deftest syntactic-close-sml-no-pad-after-test ()
  (syntactic-close-test "val z = (x + y) (a +)"
    'sml-mode
    'syntactic-close-debug-p
    (forward-char -1)
    (syntactic-close)
    (should (eq (char-before) ?b))
    (should (eq (char-after) ?\)))))

(ert-deftest syntactic-close-sml-assignment-1 ()
  (syntactic-close-test "val z = (x + y) (a + b)"
    'sml-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-sml-assignment-2 ()
  (syntactic-close-test "val z"
    'sml-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?=))))

(ert-deftest syntactic-close-sml-assignment-3 ()
  (syntactic-close-test "fun foo (z : int)"
    'sml-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?=))))

(ert-deftest syntactic-close-sml-tuple-separator-1 ()
  (syntactic-close-test "val x = (3"
    'sml-mode
    'syntactic-close-debug-p
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (should (eq (char-before) ?,)))))

(ert-deftest syntactic-close-sml-function-1 ()
  (syntactic-close-test "fun foo (x : int)"
    'sml-mode
    'syntactic-close-debug-p
    (let ((syntactic-close-electric-listify-p t))
      (syntactic-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?=)))))

;; (ert-deftest syntactic-close-sml-function-2 ()
;;   (syntactic-close-test "fun foo"
;;     'sml-mode
;;     'syntactic-close-debug-p
;;     (let ((syntactic-close-electric-listify-p t))
;;       (syntactic-close)
;;       (skip-chars-backward " \t\r\n\f")
;;       (should (eq (char-before) ?\()))))

(ert-deftest syntactic-close-backward-block-1 ()
  (syntactic-close-test "fun silly1 (z : int) =
  let
      val"
    'sml-mode
    'syntactic-close-debug-p
    (let ((syntactic-close-electric-listify-p t)
	  (ar-smart-indentation t))
      (ar-backward-block)
      (should (eq (char-after) ?f)))))

;; braucht beg-end
;; (ert-deftest syntactic-close-close-ruby-string-interpolation-test-1 ()
;;   (syntactic-close-test-with-ruby-buffer "def deliver(from: \"A\", to: nil, via: \"mail\")
;;   \"Sending from #{from} to #{to} via #{via"
;;     (syntactic-close)
;;     (should (eq (char-before) ?}))))

(provide 'syntactic-close-interactive-tests)
;;; syntactic-close-interactive-tests.el ends here
