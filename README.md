Syntactic Close [![Build Status](https://travis-ci.org/emacs-berlin/syntactic-close.svg?branch=master)](https://travis-ci.org/emacs-berlin/syntactic-close)
===

Insert closing delimiter char(s), i.e. parenthesis, bracket(s), brace(s) or
whatever is needed - think of syntactic consequence.

Honor multi-char pairs like triplequoted string, padding and
escape-sequences.

For example when called from behind an open string like

"\\(^ *\\|^Foo: *\\|\\( BAR\\|'s\\|Bad\\|BAZ\\|Bnter\\(?: \\(?:\\(?:sam\\|th

C-u M-x syntactic-close RET should result in

"\\(^ *\\|^Foo: *\\|\\( BAR\\|'s\\|Bad\\|BAZ\\|Bnter\\(?: \\(?:\\(?:sam\\|th\\)\\)\\)\\)\\)"

That way continuation may be inserted by just one convenient key.

A first draft was proposed at emacs-devel mailing-list: 
http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html 
