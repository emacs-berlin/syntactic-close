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

ORT=${ORT:-$1}

echo "\$ORT: $ORT"

if [ $ORT -eq 0 ]; then
    #  EMACS=$HOME/arbeit/emacs/emacs-UA/src/emacs-27.0.50.1
    EMACS=emacs
elif [ -s emacs24 ]; then
    EMACS=emacs24
else
    EMACS=emacs
fi

echo "\$EMACS: $EMACS"

FILE1=syntactic-close.el
FILE2=.cask/24.4/elpa/php-mode-20160910.1801/php-mode.el


TEST1=test/syntactic-close-setup-ert-tests.el
TEST2=test/syntactic-close-c-tests.el
TEST3=test/syntactic-close-c++-tests.el
TEST4=test/syntactic-close-sml-tests.el
TEST5=test/syntactic-close-haskell-tests.el
TEST6=test/syntactic-close-emacs-lisp-tests.el
TEST7=test/syntactic-close-js-tests.el
TEST8=test/syntactic-close-php-tests.el
TEST9=test/syntactic-close-python-tests.el
TEST10=test/syntactic-close-ruby-tests.el
TEST11=test/syntactic-close-xml-tests.el
TEST12=test/syntactic-close-fundamental-tests.el
TEST13=test/syntactic-close-tests.el

hier () {
    $EMACS -Q --batch \
--eval "(add-to-list 'load-path \"$PWD/.cask/24.4/elpa/php-mode-20160910.1801\")" \
-load $FILE1 \
-load $FILE2 \
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-load $TEST4 \
-load $TEST5 \
-load $TEST6 \
-load $TEST7 \
-load $TEST8 \
-load $TEST9 \
-load $TEST10 \
-load $TEST11 \
-load $TEST12 \
-load $TEST13 \
-f ert-run-tests-batch-and-exit
}

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
-load $FILE1 \
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-load $TEST4 \
-load $TEST5 \
-load $TEST7 \
-load $TEST8 \
-load $TEST9 \
-load $TEST10 \
-load $TEST11 \
-load $TEST12 \
-load $TEST13 \
-f ert-run-tests-batch-and-exit
}


if [ $ORT -eq 0 ]; then
    hier
else
    echo "Lade Testumgebung \"entfernt\""
    entfernt
fi

