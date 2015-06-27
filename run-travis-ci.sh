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

# This script tests functions from python-mode.el.

# Code:

PDIR=$PWD

TESTDIR=$PDIR/test
export TESTDIR

echo "\$1: $1"

SETUP=$TESTDIR/general-close-setup-ert-tests.el

TEST1=$TESTDIR/general-close-tests.el
TEST2=$TESTDIR/general-close-ruby-tests.el
TEST3=$TESTDIR/general-close-python-tests.el
TEST4=$TESTDIR/general-close-php-tests.el
TEST5=$TESTDIR/general-close-js-tests.el

if [ -s emacs24 ]; then
    EMACS=emacs24
else
    EMACS=emacs
fi

echo "\$EMACS: $EMACS"

# Form delivered by $SETUP is not used yet

$EMACS -Q --batch --eval "(message (emacs-version))" \
       -L "$PDIR" \
       -L "$TESTDIR" \
       -l $PDIR/general-close.el \
       -l $PDIR/modes/php-mode.el \
       -l $SETUP \
       --eval "(setq gen-verbose-p nil)" \
       -l $TEST1 -l $TEST2 -l $TEST3 -l $TEST4 -l $TEST5 \
       -f ert-run-tests-batch-and-exit
