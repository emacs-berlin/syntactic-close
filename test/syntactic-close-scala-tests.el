;;; syntactic-close-scala-tests.el --- Scala tests -*- lexical-binding: t; -*-

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
(ert-deftest syntactic-close-scala-test-2EBKNw ()
  (syntactic-close-test
      "(1 to 9).flatMap(x => (1 to 9).filter { y => (x + 4 * y) > (x * y)"
    'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (syntactic-close)
    (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-scala-test-CFGK23 ()
  (syntactic-close-test
      "(1 to 9).flatMap(x => (1 to 9).filter { y => (x + 4 * y) > (x * y) }"
    'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-scala-test-l33NWm ()
  "If you add more than one filter on a generator,

the filter’s if clauses must be separated by semicolons.

Source: Odersky, Spoon, Venners: Programming in Scala"
  (syntactic-close-test
      "def fileLines(file: java.io.File) =
  scala.io.Source.fromFile(file).getLines.toList

def grep(pattern: String) =
  for (
    file <- filesHere
    if file.getName.endsWith(\".scala\")
    line <- fileLines(file)
    if line.trim.matches(pattern)
  ) println(file +\": \"+ line.trim)
grep(\".*gcd.*\")
"
    'scala-mode
    (font-lock-ensure)
    syntactic-close-debug-p
    (goto-char (point-max))
    (search-backward "scala")
    (end-of-line)
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-scala-test-2LJ2DM ()
  "If you add more than one filter on a generator,

the filter’s if clauses must be separated by semicolons.

If you prefer, you can use curly braces instead of parentheses to
surround the generators and filters. One advantage to using curly
braces is that you can leave off some of the semicolons that are
needed when you use parentheses.

Source: Odersky, Spoon, Venners: Programming in Scala"
  (syntactic-close-test
      "def fileLines(file: java.io.File) =
  scala.io.Source.fromFile(file).getLines.toList

def grep(pattern: String) =
  for {
    file <- filesHere
    if file.getName.endsWith(\".scala\")
    line <- fileLines(file)
    if line.trim.matches(pattern)
  } println(file +\": \"+ line.trim)
grep(\".*gcd.*\")
"
    'scala-mode
    (font-lock-ensure)
    syntactic-close-debug-p
    (goto-char (point-max))
    (search-backward "scala")
    (end-of-line)
    (syntactic-close)
    (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-scala-test-5xD6NO ()
  (syntactic-close-test
 "val a = 1
{
  val a = 2;
  {
    println(a)
  }
}
"
    'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (search-backward "1")
    (forward-char 1)
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-scala-test-moJaUT ()
  (syntactic-close-test
 "val a = 1;
{
  val a = 2;
  {
    println(a)
  }
}
"
    'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (search-backward "2")
    (forward-char 1)
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-scala-test-sbaqDQ ()
  (syntactic-close-test
      "def sum(s: Seq[Int]): Int = leftFold(s, 0, (x, y"
        'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?\)))))

(ert-deftest syntactic-close-scala-test-OcCPv9 ()
  (syntactic-close-test
      "def sum(s: Seq[Int])"
        'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?:))
    (should (eq (char-before  (1- (point))) 41))
    ))

(ert-deftest syntactic-close-scala-test-nJghbp ()
  (syntactic-close-test
      "/*Comment starts
continues
continues
Comment ends"
        'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?/))
    (should (eq (char-before  (1- (point))) ?\*))
    ))

(ert-deftest syntactic-close-scala-test-dOobA3 ()
  (syntactic-close-test
      "val foo =  this.totalFoo(Seq((\"apple\", 2))"
        'scala-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?\)))
    (should (eq (char-before  (1- (point))) ?\)))
    ))

(ert-deftest syntactic-close-scala-multidelim-test-lvyjjU ()
  (syntactic-close-test
      "{{{{asdf}"
    'scala-mode
    (goto-char (point-max))
    (syntactic-close)
    (should (looking-back "}}" (line-beginning-position)))))

(provide 'syntactic-close-scala-tests)
;;; syntactic-close-scala-tests.el ends here
