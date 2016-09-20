General Close [![Build Status](https://travis-ci.org/emacs-berlin/general-close.svg?branch=master)](https://travis-ci.org/emacs-berlin/general-close)
===

When it's about to insert a closing delimiter, commonly typing a
parenthesis, bracket, brace or whatever is needed.

Beside these keys are often more difficult to reach than others on
some keyboards, all closing in all modes could be done by just one
convenient key.

Also closing must not mean to insert a single character: if behind an
"if" in shell-script, `general-close' might provide the "fi", etc.

This should speed up editing to some extend.

A first draft was proposed at emacs-devel mailing-list: 
http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

Being designed to work for every language, please open a ticket should
anything fail.