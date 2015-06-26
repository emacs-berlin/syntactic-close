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
