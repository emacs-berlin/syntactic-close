;;; syntactic-close-java-tests.el --- java tests -*- lexical-binding: t; -*-

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
(ert-deftest syntactic-close-java-string-test-8fM8uW ()
  (syntactic-close-test
      "public class Foo {
    public static void main(String\[] args)
    {
        System\.out\.println(\"Foo Bar: \" + foo)
    }
} "
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (search-backward ")")
    (forward-char 1)
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-java-import-test-wo4a9t ()
  (syntactic-close-test
      "import java.util.Random"
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-java-test-oc9rmB ()
  (syntactic-close-test
      "System.out.println(\"x\")"
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-java-test-ikuEEF ()
  (syntactic-close-test
      "public class Foo1 {
    public static void foo1() {
        int x = 1;
        double y = 2.0;
        foo2(x, y);
        System.out.println(y + \" \" + x"
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) 41))))

(ert-deftest syntactic-close-java-test-UApYpP ()
  (syntactic-close-test
"public class FooBau {

  public static void main(String[] args) {
    var sb = new Foo();
    for(int i = 0; i < 100; i++) {
        sb.append(\"bla\" + i + \" \");
    }
    String s = sb.toString();
    System.out.println(s);
    }
"
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (syntactic-close)
    (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-java-test-rP3vyd ()
  (syntactic-close-test
"public static boolean main(int[] data) {
    if(data == null || data.length < 2)
        return false;

    for(i = 0
    "
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) 32))
    (should (eq (char-before (1- (point))) ?\;))
    ))

(ert-deftest syntactic-close-java-test-smbeId ()
  (syntactic-close-test
"public static boolean main(int[] data) {
    if(data == null || data.length < 2)
        return false;

    for(i = 0; i < x; i++
    "
    'java-mode
    syntactic-close-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) 41))))

(provide 'syntactic-close-java-tests)
;;; syntactic-close-java-tests.el ends here
