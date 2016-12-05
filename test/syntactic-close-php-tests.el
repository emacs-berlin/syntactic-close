;;; syntactic-close-php-tests.el --- -*- lexical-binding: t; -*-

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

(defvar syntactic-close-php-test-string-1 "<?php
\$l = mysql_connect(\"localhost\", \"user\", \"passw\");
if (!\$l) die(\"Keine Verbindung zu DB\\n\");
if (mysql_select_db(\"abkuerz\", \$l))
  echo(\"DB abkuerz aktiv\\n\");
else
  die(\"Konnte keine DB finden!\\n\");
\$frage =\"describe gii\";
\$resID = mysql_query(\$frage, \$l);
\$resID = mysql_query(\$frage, \$l);

<td><?php echo \$person->getName(); ?></td>

if (!\$resID) die(\"Kein Ergebnis von DB\");
while (\$zeile = mysql_fetch_array(\$resID)) {
  echo(\$zeile[0] . \" \" . \$zeile[1] . \"\\n\");}
// echo($resID);

?>
")

(setq syntactic-close-php-test-string-2 "<?php
\$l = mysql_connect(\"localhost\", \"user\", \"passw
?>
")

(setq syntactic-close-php-test-string-3 "<?php
\$l = mysql_connect(\"localhost\", \"user\", \"passw\");
if (!\$l) die(\"Keine Verbindung zu DB\\n\");
if (mysql_select_db(\"abkuerz\", \$l))
  echo(\"DB abkuerz aktiv\\n\");
else
  die(\"Konnte keine DB finden!\\n\");
\$frage =\"describe gii\";
\$resID = mysql_query(\$frage, \$l);
\$resID = mysql_query(\$frage, \$l);

<td><?php echo \$person->getName(); ?></td>

if (!\$resID) die(\"Kein Ergebnis von DB\");
while (\$zeile = mysql_fetch_array(\$resID)) {
  echo(\$zeile[0] . \" \" . \$zeile[1] . \"\\n\");}
// echo($resID);

?>
")

(setq syntactic-close-php-test-string-4
      "<?php
function €()
{
   echo 'foo';
}
?>
")

(setq syntactic-close-php-test-string-5
      "<td><?php echo \$person->getName(); ?></td"
      )

(ert-deftest syntactic-close-close-php-paren-semicolon-test ()
  (syntactic-close-test-with-php-buffer-point-min
      syntactic-close-php-test-string-2
    (search-forward "passw")
    (syntactic-close)
    (should (eq (char-before) ?\"))
    (syntactic-close)
    (should (eq (char-before) ?\)))
    (syntactic-close)
    (should (eq (char-before) ?\;))))

(ert-deftest syntactic-close-close-php-public-function-test ()
  (syntactic-close-test-with-php-buffer
      "public function Foobar(){
  echo \"Foobar"
    (syntactic-close)
    (should (eq (char-before) ?\"))
    (syntactic-close)
    (should (eq (char-before) ?\;))
    (syntactic-close)
    (should (eq (char-before) ?}))))


(ert-deftest syntactic-close-close-php-indented-line-test ()
  (syntactic-close-test-with-php-buffer
      "if ($foobar){
    foreach ($foobar as $key =>  $val) {
         echo $key;
	 "
    (syntactic-close)
    (should (eq (char-before) ?}))))

(ert-deftest syntactic-close-close-php-check-indent-test ()
  (let ((syntactic-close-electric-indent-p t))
    (syntactic-close-test-with-php-buffer
	"if ($foobar){
    foreach ($foobar as $key =>  $val) {
         echo $key;
	 "
      (syntactic-close) 
      (should (eq (current-indentation) 4))
      (newline-and-indent)
      (syntactic-close)
      (should (eq (current-indentation) 0)))))

(provide 'syntactic-close-php-tests)
;;; syntactic-close-php-tests.el ends here
