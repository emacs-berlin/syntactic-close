;;; general-close-php-tests.el ---

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

(defvar general-close-php-test-string-1 "<?php
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

(setq general-close-php-test-string-2 "<?php
\$l = mysql_connect(\"localhost\", \"user\", \"passw
?>
")

(setq general-close-php-test-string-3 "<?php
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

(setq general-close-php-test-string-4
      "<?php
function €()
{
   echo 'foo';
}
?>
")

(setq general-close-php-test-string-5
      "<td><?php echo \$person->getName(); ?></td"
      )

(ert-deftest gen-close-php-paren-semicolon-test ()
  (gen-test-with-php-buffer-point-min
      general-close-php-test-string-2
    (search-forward "passw")
    (general-close)
    (should (eq (char-before) ?\"))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\;))))

(ert-deftest gen-close-php-public-function-test ()
  (gen-test-with-php-buffer
      "public function Foobar(){
  echo \"Foobar"
    (general-close)
    (should (eq (char-before) ?\"))
    (general-close)
    (should (eq (char-before) ?\;))
    (general-close)
    (should (eq (char-before) ?}))))


(ert-deftest gen-close-php-indented-line-test ()
  (gen-test-with-php-buffer
      "if ($foobar){
    foreach ($foobar as $key =>  $val) {
         echo $key;
	 "
    (general-close)
    (should (eq (char-before) ?}))))

(ert-deftest gen-close-php-check-indent-test ()
  (let ((gen-electric-indent-p t))
    (gen-test-with-php-buffer
	"if ($foobar){
    foreach ($foobar as $key =>  $val) {
         echo $key;
	 "
      (general-close) 
      (should (eq (current-indentation) 4))
      (newline-and-indent)
      (general-close)
      (should (eq (current-indentation) 0)))))

(provide 'general-close-php-tests)
;;; general-close-php-tests.el ends here
