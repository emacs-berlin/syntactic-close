#!/bin/sh

# Author: Andreas Roehler <andreas.roehler@online.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# Commentary:

# This script tests functions from ar-mode.el.

# Code:

# ORT=${ORT:=1}

# echo "\$ORT: $ORT"

if [ $1 == e25 ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e26 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e27 ];then
    #  export EMACS="$HOME/emacs-20220306/src/emacs -Q"
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e28 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e29 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
else
    EMACS=emacs
fi

echo "before shift \$EMACS: $EMACS"
shift

echo "\$*: $*"
PDIR=$PWD
echo "\$PWD: $PWD"
# WERKSTATT set in .bashrc, thus unset remotly
WERKSTATT=${WERKSTATT:=1}
echo "\$WERKSTATT: $WERKSTATT"
IFLOCAL=${IFLOCAL:=1}
echo "\$IFLOCAL: $IFLOCAL"

TESTDIR=$PDIR/test/
#  export TESTDIR
echo "\$TESTDIR: $TESTDIR"

echo "\$HASKELL_MODE_DIR: $HASKELL_MODE_DIR"

FILE1=syntactic-close.el
FILE2=$WERKSTATT/thingatpt-utils-core/ar-subr.el
FILE3=$WERKSTATT/thingatpt-utils-core/ar-beg-end.el
FILE4=$WERKSTATT/thingatpt-utils-core/ar-thingatpt-basic-definitions.el
FILE5=$WERKSTATT/thingatpt-utils-core/ar-thingatpt-utils-core.el
FILE6=$WERKSTATT/thing-at-point-utils/ar-thingatpt-utils.el
FILE7=$WERKSTATT/thing-at-point-utils/ar-sexp.el
FILE8=$HASKELL_MODE_DIR/haskell-mode.el

SETUP=test/syntactic-close-setup-ert-tests.el

TEST1=test/syntactic-close-emacs-lisp-tests.el
TEST2=test/syntactic-close-org-mode-tests.el
TEST3=test/syntactic-close-python-tests.el
TEST4=test/syntactic-close-scala-tests.el
TEST5=test/syntactic-close-haskell-tests.el
TEST6=test/syntactic-close-js-tests.el
TEST7=test/syntactic-close-php-tests.el
TEST8=test/syntactic-close-sml-tests.el
TEST9=test/syntactic-close-ruby-tests.el
TEST10=test/syntactic-close-xml-tests.el
TEST11=test/syntactic-close-fundamental-tests.el
TEST12=test/syntactic-close-java-tests.el
TEST13=test/syntactic-close-c++-tests.el
TEST14=test/syntactic-close-c-tests.el

h1 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
\
-load $TEST1 \
-f ert-run-tests-batch-and-exit
}

h2 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
\
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

h3 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST3 \
-f ert-run-tests-batch-and-exit
}

h4 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST4 \
-f ert-run-tests-batch-and-exit
}

h5 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
--eval "(add-to-list 'load-path (getenv\"HASKELL_MODE_DIR\"))" \
-load $FILE1 \
-load $FILE8 \
-load $SETUP \
-load $TEST5 \
-f ert-run-tests-batch-and-exit
}

h6 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST6 \
-f ert-run-tests-batch-and-exit
}

h7 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
--eval "(when (< 25 (string-to-number (substring emacs-version 0 2))) (defvar bootstrap-version) (let ((bootstrap-file (expand-file-name \"straight/repos/straight.el/bootstrap.el\" user-emacs-directory)) (bootstrap-version 5)) (unless (file-exists-p bootstrap-file) (with-current-buffer (url-retrieve-synchronously \"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el\" (quote silent) (quote inhibit-cookies)) (goto-char (point-max)) (eval-print-last-sexp))) (load bootstrap-file nil (quote nomessage))) (straight-use-package (quote php-mode)))" \
--eval "(sit-for 3)" \
-load $FILE1 \
-load $SETUP \
-load $TEST7 \
-f ert-run-tests-batch-and-exit
}

h8 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST8 \
-f ert-run-tests-batch-and-exit
}

h9 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST9 \
-f ert-run-tests-batch-and-exit
}

h10 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST10 \
-f ert-run-tests-batch-and-exit
}

h11 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST11 \
-f ert-run-tests-batch-and-exit
}

h12 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST12 \
-f ert-run-tests-batch-and-exit
}

h13 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
--eval "(when (< 25 (string-to-number (substring emacs-version 0 2))) (defvar bootstrap-version) (let ((bootstrap-file (expand-file-name \"straight/repos/straight.el/bootstrap.el\" user-emacs-directory)) (bootstrap-version 5)) (unless (file-exists-p bootstrap-file) (with-current-buffer (url-retrieve-synchronously \"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el\" (quote silent) (quote inhibit-cookies)) (goto-char (point-max)) (eval-print-last-sexp))) (load bootstrap-file nil (quote nomessage))) (straight-use-package (quote haskell-mode)))" \
--eval "(sit-for 3)" \
-load $FILE1 \
-load $FILE8 \
-load $SETUP \
-load $TEST13 \
-f ert-run-tests-batch-and-exit
}

h14 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq operator-mode-debug nil)" \
-load $FILE1 \
-load $SETUP \
-load $TEST14 \
-f ert-run-tests-batch-and-exit
}

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $SETUP \
\
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-load $TEST4 \
-load $TEST5 \
-load $TEST7 \
-load $TEST9 \
-load $TEST10 \
-load $TEST11 \
-load $TEST12 \
-load $TEST13 \
-f ert-run-tests-batch-and-exit
}

hier () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv\"HASKELL_MODE_DIR\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $SETUP \
\
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-load $TEST4 \
-load $TEST5 \
-load $TEST7 \
-load $TEST9 \
-load $TEST10 \
-load $TEST11 \
-load $TEST12 \
-load $TEST13 \
-load $TEST14 \
-f ert-run-tests-batch-and-exit
}

if [ $IFLOCAL -eq 0 ]; then
    while getopts 123456789abcdefghijklmnopqrstuvx option
    do
        case $option in
	    1) echo "h1: Lade \$TEST1: \"$TEST1\"";h1;;
	    2) echo "h2: Lade \$TEST2: \"$TEST2\"";h2;;
	    3) echo "h3: Lade \$TEST3: \"$TEST3\"";h3;;
	    4) echo "h4: Lade \$TEST4: \"$TEST4\"";h4;;
	    5) echo "h5: Lade \$TEST5: \"$TEST5\"";h5;;
	    6) echo "h6: Lade \$TEST6: \"$TEST6\"";h6;;
	    7) echo "h7: Lade \$TEST7: \"$TEST7\"";h7;;
	    8) echo "h8: Lade \$TEST8: \"$TEST8\"";h8;;
	    9) echo "h9: Lade \$TEST9: \"$TEST9\"";h9;;
	    a) echo "h10: Lade \$TEST10: \"$TEST10\"";h10;;
	    b) echo "h11: Lade \$TEST11: \"$TEST11\"";h11;;
	    c) echo "h12: Lade \$TEST12: \"$TEST12\"";h12;;
	    d) echo "h13: Lade \$TEST13: \"$TEST13\"";h13;;
	    e) echo "h14: Lade \$TEST14: \"$TEST14\"";h14;;
	    # f) echo "h15: Lade \$TEST15: \"$TEST15\"";h15;;
	    # g) echo "h16: Lade \$TEST16: \"$TEST16\"";h16;;
            # h) echo "h17: Running python-tests.el";h17;;
	    # i) echo "h18: Lade \$TEST18: \"$TEST18\"";h18;;
	    # j) echo "h19: Lade \$TEST19: \"$TEST19\"";h19;;
	    # k) echo "h20: Lade \$TEST20: \"$TEST20\"";h20;;
	    #  l) echo "h21: Lade \$TEST21: \"$TEST21\"";h21;;
	    #  m) echo "h22: Lade \$TEST22: \"$TEST22\"";h22;;
	    n) echo "Lade Testumgebung ‘hier’";hier;;

	esac
	shift
	echo "\$*: $*"
	EMACS=$1
	
    done

    else
    echo "entfernt"
    echo "\$WERKSTATT: $WERKSTATT"
    echo "Lade testumgebung \"ENTFERNT\""
    entfernt
fi

# if [ $ORT -eq 0 ]; then
#     echo calling "source ./run-travis-hier.sh"
#     . ./run-travis-hier.sh
    
# else
#     echo "Lade Testumgebung \"entfernt\""
#     echo \$ORT: $ORT
#     entfernt
# fi
