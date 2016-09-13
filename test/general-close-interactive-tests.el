;;; general-close-interactive-tests.el --- Tests known to work when called interactively only -*- lexical-binding: t; -*-

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

(ert-deftest general-close-close-ruby-block-test ()
  (general-close-test-with-ruby-buffer "$DBH.SELECT_ALL(\"SELECT \* FROM FOO\") DO |ROW|"
    (general-close)
    (should (eq (char-before) ?d))))

(ert-deftest general-close-c-nesting-comment-test ()
  (general-close-test "/* The open system call "
    'c-mode
    'general-close-debug-p
    (general-close)
    (should (eq (char-before) ?/))))

(ert-deftest general-close-c++-nesting-comment-test ()
  (general-close-test "/* The open system call "
    'c++-mode
    'general-close-debug-p
    (general-close)
    (should (eq (char-before) ?/))))

(ert-deftest general-close-haskell-comment-test ()
  (general-close-test-with-haskell-buffer
      "{- To explore this file: "
    'general-close-debug-p
    (general-close)
    (should (eq (char-before) ?\}))))

(ert-deftest general-close-haskell-right-arrow-test-1 ()
  (general-close-test-with-haskell-buffer
      "add :: (Int,Int"
    (let (general-close-electric-listify-p)
      (general-close)
      (should (eq (char-before) ?\))))))

(ert-deftest general-close-haskell-right-arrow-test-2 ()
  (general-close-test-with-haskell-buffer "asdf :: Int"
    (let (general-close-electric-listify-p)
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?>)))))

(ert-deftest general-close-haskell-right-arrow-test-3 ()
  (general-close-test-with-haskell-buffer "add :: (Int,Int)"
    (let (general-close-electric-listify-p)
      (general-close)
      (skip-chars-backward " \t\r\n\f") 
      (should (eq (char-before) ?>)))))

(ert-deftest general-close-haskell-assign-test-1 ()
  (general-close-test-with-haskell-buffer "asdf "
    (let (general-close-electric-listify-p)
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?=)))))


(ert-deftest general-close-haskell-asign-test-2 ()
  (general-close-test-with-haskell-buffer "asdf :: Int -> Int
asdf n"
    (let (general-close-electric-listify-p)
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?=)))))

(ert-deftest general-close-haskell-concat-test ()
  ;; indent s = "    " ++ s
  (general-close-test-with-haskell-buffer "indent s = \"asdf\""
    (general-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\+))))

(ert-deftest general-close-haskell-typedef-test ()
  (general-close-test-with-haskell-buffer "signum :: Int ->"
    (general-close)
    (skip-chars-backward " \t\r\n\f") 
    (should (eq (char-before) ?t))))

(ert-deftest general-close-python-colon-test-2 ()
  (general-close-test-with-python-buffer
      "class TutorialApp(App):
    def build(self):
        return Button(text=\"Hello!\",
                      background_color=(0, 0, 1, 1)
                      font_size=150)
if __name__ == \"__main__\""
    (general-close)
    (should (eq (char-before) ?:))
    (general-close)
    (should (eq 8 (current-indentation)))))


(ert-deftest general-close-list-comprehension-test-1 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(asdb,"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?b)))))

(ert-deftest general-close-list-comprehension-test-2 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(asdb,cdfg"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?\))))))

(ert-deftest general-close-list-comprehension-test-3 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?,)))))

(ert-deftest general-close-list-comprehension-test-4 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?y)))))

(ert-deftest general-close-list-comprehension-test-5 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(a,"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?b)))))

(ert-deftest general-close-list-comprehension-test-6 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(a,b)] |"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?a)))))

(ert-deftest general-close-list-comprehension-test-7 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,y)] |"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?x)))))

(ert-deftest general-close-list-comprehension-test-8 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(abd,def)] |"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "abd")))))

(ert-deftest general-close-list-comprehension-test-9 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(a,b)] | a"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest general-close-list-comprehension-test-10 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,y)] | x"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest general-close-list-comprehension-test-11 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(abd,def)] | abd"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest general-close-list-comprehension-test-12 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(asdb, asdb"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\))))))


(ert-deftest general-close-list-comprehension-test-13 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(abd,def)] | abd <- "
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\[)))))

(ert-deftest general-close-list-comprehension-test-14 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(abd,def)] | abd <- [1..3 "
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\]))))) 

(ert-deftest general-close-list-comprehension-test-15 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,y)] | x <-[1..3]"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?,)))))

(ert-deftest general-close-list-comprehension-test-16 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,y)] | x <-[1..3],"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?y)))))

(ert-deftest general-close-list-comprehension-test-17 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,y)] | x <-[1..3], y"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-"))))) 

;; SML
(ert-deftest general-close-sml-comment-test ()
  (general-close-test "(* definition of nat"
    'sml-mode
    'general-close-debug-p
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest general-close-sml-assignment-test-1 ()
  (general-close-test "val z"
    'sml-mode
    'general-close-debug-p
    (general-close)
    (skip-chars-backward " \t\r\n\f") 
    (should (eq (char-before) ?=))))


(ert-deftest general-close-sml-no-pad-after-test ()
  (general-close-test "val z = (x + y) (a +)"
    'sml-mode
    'general-close-debug-p
    (forward-char -1)
    (general-close)
    (should (eq (char-before) ?b))
    (should (eq (char-after) ?\)))))

(ert-deftest general-close-sml-assignment-1 ()
  (general-close-test "val z = (x + y) (a + b)"
    'sml-mode
    'general-close-debug-p
    (general-close)
    (should (eq (char-before) ?\;))))

(provide 'general-close-interactive-tests)
;;; general-close-interactive-tests.el ends here
