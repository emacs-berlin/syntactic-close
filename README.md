[![CircleCI thingatpt-utils-core](https://circleci.com/gh/andreas-roehler/syntactic-close.svg?style=svg)](https://app.circleci.com/pipelines/gh/andreas-roehler/syntactic-close)

Insert closing delimiter char(s), i.e. parenthesis, bracket(s), brace(s) or
whatever is needed - think of syntactic consequence.

Honor multi-char pairs like triplequoted string, padding and
escape-sequences.

For example when called at the end of an open string like

"\\(^ *\\|^Foo: *\\|\\( BAR\\|'s\\|Bad\\|BAZ\\|Bnter\\(?: \\(?:\\(?:sam\\|th

<kbd>C-u M-x</kbd> `syntactic-close` <kbd>RET</kbd> should result in

"\\(^ *\\|^Foo: *\\|\\( BAR\\|'s\\|Bad\\|BAZ\\|Bnter\\(?: \\(?:\\(?:sam\\|th\\)\\)\\)\\)\\)"

That way continuation may be inserted by just one key.

Sometimes language-specific treatment is needed. 

Currently supports the following modes: 

agda2-mode
emacs-lisp-mode 
html-mode 
js-mode 
mhtml-mode 
nxml-mode 
org-mode 
php-mode 
python-mode 
ruby-mode 
sgml-mode 
web-mode 
xml-mode 
xxml-mode

Feel free to file a feature request should your favourite mode not
being mentioned.

A first draft was proposed at emacs-devel mailing-list: 
http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html 
