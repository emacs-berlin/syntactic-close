;; general-close-modes.el --- mode-specific functions -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

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

(defvar general-close-python-keywords "\\<\\(ArithmeticError\\|AssertionError\\|AttributeError\\|BaseException\\|BufferError\\|BytesWarning\\|DeprecationWarning\\|EOFError\\|Ellipsis\\|EnvironmentError\\|Exception\\|False\\|FloatingPointError\\|FutureWarning\\|GeneratorExit\\|IOError\\|ImportError\\|ImportWarning\\|IndentationError\\|IndexError\\|KeyError\\|KeyboardInterrupt\\|LookupError\\|MemoryError\\|NameError\\|NoneNotImplementedError\\|NotImplemented\\|OSError\\|OverflowError\\|PendingDeprecationWarning\\|ReferenceError\\|RuntimeError\\|RuntimeWarning\\|StandardError\\|StopIteration\\|SyntaxError\\|SyntaxWarning\\|SystemError\\|SystemExit\\|TabError\\|True\\|TypeError\\|UnboundLocalError\\|UnicodeDecodeError\\|UnicodeEncodeError\\|UnicodeError\\|UnicodeTranslateError\\|UnicodeWarning\\|UserWarning\\|ValueError\\|Warning\\|ZeroDivisionError\\|__debug__\\|__import__\\|__name__\\|abs\\|all\\|and\\|any\\|apply\\|as\\|assert\\|basestring\\|bin\\|bool\\|break\\|buffer\\|bytearray\\|callable\\|chr\\|class\\|classmethod\\|cmp\\|coerce\\|compile\\|complex\\|continue\\|copyright\\|credits\\|def\\|del\\|delattr\\|dict\\|dir\\|divmod\\|elif\\|else\\|enumerate\\|eval\\|except\\|exec\\|execfile\\|exit\\|file\\|filter\\|float\\|for\\|format\\|from\\|getattr\\|global\\|globals\\|hasattr\\|hash\\|help\\|hex\\|id\\|if\\|import\\|in\\|input\\|int\\|intern\\|is\\|isinstance\\|issubclass\\|iter\\|lambda\\|len\\|license\\|list\\|locals\\|long\\|map\\|max\\|memoryview\\|min\\|next\\|not\\|object\\|oct\\|open\\|or\\|ord\\|pass\\|pow\\|print\\|property\\|quit\\|raise\\|range\\|raw_input\\|reduce\\|reload\\|repr\\|return\\|round\\|set\\|setattr\\|slice\\|sorted\\|staticmethod\\|str\\|sum\\|super\\|tuple\\|type\\|unichr\\|unicode\\|vars\\|while\\|with\\|xrange\\|yield\\|zip\\|\\)\\>"
  "Contents like py-font-lock-keyword")

(require 'sgml-mode)
(require 'comint)

(defvar general-close-comint-haskell-pre-right-arrow-re   "let [alpha][A-Za-z0-9_]+ +::")
;; (setq general-close-comint-haskell-pre-right-arrow-re   "let [alpha][A-Za-z0-9_]+ +::")
(defcustom general-close-comint-haskell-pre-right-arrow-re
  "let [alpha][A-Za-z0-9_]+ +::"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-comint-haskell-pre-right-arrow-re"
  :group 'general-close)


(defvar haskell-interactive-mode-prompt-start (ignore-errors (require 'haskell-interactive-mode) haskell-interactive-mode-prompt-start)
  "Defined in haskell-interactive-mode.el, silence warnings. ")

(defvar general-close--current-source-buffer nil
  "Set by `general-close--set-current-source-buffer' maybe.

Default is nil.
Comint-modes might want to load stuff from " )

(defgroup general-close nil
  "Insert closing delimiter whichever needed. "
  :group 'languages
  :tag "general-close"
  :prefix "general-close-")

(defcustom general-close-delete-whitespace-backward-p nil
  "If whitespace characters before point should be deleted.

Default is nil"

  :type 'boolean
  :tag "general-close-delete-whitespace-backward-p"
  :group 'general-close)

(defcustom general-close-insert-with-padding-p t
  "Ensure a whitespace character before point.

Default is t"

  :type 'boolean
  :tag "general-close-insert-with-padding-p"
  :group 'general-close)

(defvar general-close-electric-listify-p t)
(defcustom general-close-electric-listify-p nil
  "When inside a list, assume list-separator.

If after list-separator, replace it by closing the list
Default is nil"

  :type 'boolean
  :tag "general-close-electric-listify-p"
  :group 'general-close)
(make-variable-buffer-local 'general-close-electric-listify-p)

(defvar general-close-list-separator-char 44)
(defcustom general-close-list-separator-char 44
  "Char separating elements of a list.

Takes effect with electric mode
Default is `,'"

  :type 'character
  :tag "general-close-list-separator-char"
  :group 'general-close)
(make-variable-buffer-local 'general-close-list-separator-char)

(defcustom general-close-list-element-delimiter-1 39
  "Char delimiting elements of a list.

Takes effect with electric mode
Default is `''"

  :type 'character
  :tag "general-close-list-element-delimiter-1"
  :group 'general-close)
(make-variable-buffer-local 'general-close-list-element-delimiter-1)

(defcustom general-close-list-element-delimiter-2 34
  "Char delimiting elements of a list.

Takes effect with `electric' mode
Default is `\"'"

  :type 'character
  :tag "general-close-list-element-delimiter-2"
  :group 'general-close)
(make-variable-buffer-local 'general-close-list-element-delimiter-2)

(defcustom general-close-guess-p nil
  "When non-nil, guess default arguments, list-separators etc. "
  :type 'boolean
  :tag "general-close-guess-p"
  :group 'general-close)
(make-variable-buffer-local 'general-close-guess-p)

(defcustom general-close-electric-indent-p nil
  "When `t', after insert at empty line indent according to mode.

Default is nil"

  :type 'boolean
  :tag "general-close-electric-indent-p"
  :group 'general-close)
(make-variable-buffer-local 'general-close-electric-indent-p)

(defcustom general-close-electric-newline-p nil
  "Insert a newline if feasible.

Default is nil"

  :type 'boolean
  :tag "general-close-electric-newline-p"
  :group 'general-close)

(defcustom general-close-auto-p nil
  "Enable auto-close. Experienced users only.

If `t', lists will be auto-filled.
Default is nil"

  :type 'boolean
  :tag "general-close-auto-p"
  :group 'general-close)
(make-variable-buffer-local 'general-close-auto-p)

(defcustom general-close--semicolon-separator-modes
  (list
   'inferior-sml-mode
   'js-mode
   'js2-mode
   'perl-mode
   'php-mode
   'sml-mode
   'web-mode
   )
  "List of modes which commands must be closed by `general-close-command-separator-char. "

  :type 'list
  :tag "general-close--semicolon-separator-modes"
  :group 'general-close)

(defcustom general-close--singlequote-modes
  (list
   'haskell-mode
   'inferior-haskell
   )
  "List of modes using singlequote as delimiters without string-syntax. "

  :type 'list
  :tag "general-close--singlequote-modes"
  :group 'general-close)

(defcustom general-close--colon-separator-modes
  (list
   'python-mode
   )
  "List of modes which commands which require a colon after arguments list. "

  :type 'list
  :tag "general-close--semicolon-separator-modes"
  :group 'general-close)

(defcustom general-close--ml-modes
  (list
   'html-mode
   'nxml-mode
   'sgml-mode
   'xml-mode
   'xxml-mode
   )
  "List of modes using markup language. "

  :type 'list
  :tag "general-close--semicolon-separator-modes"
  :group 'general-close)

(defvar general-close-comint-pre-assignment-re   "let [[:alpha:]][A-Za-z0-9_]")
(defcustom general-close-comint-pre-assignment-re
  "let [[:alpha:]][A-Za-z0-9_]"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-comint-pre-assignment-re"
  :group 'general-close)

(defvar general-close-pre-assignment-re "[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]*[ \t]*$\\|[[:alpha:]][A-Za-z0-9_]*[ \t]*$")

(setq general-close-pre-assignment-re   "[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]*[ \t]*$\\|[[:alpha:]][A-Za-z0-9_]*[ \t]*$")

(defcustom general-close-pre-assignment-re
  "[[:alpha:]][A-Za-z0-9_]+[ \t]*[^=]*$"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-pre-assignment-re"
  :group 'general-close)

(defvar general-close-comint-pre-right-arrow-re   "let [[:alpha:]][A-Za-z0-9_]+ +::")
;; (setq general-close-comint-pre-right-arrow-re   "let [[:alpha:]][A-Za-z0-9_]+ +::")
(defcustom general-close-comint-pre-right-arrow-re
  "let [[:alpha:]][A-Za-z0-9_]+ +::"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-comint-pre-right-arrow-re"
  :group 'general-close)

(defcustom general-close-pre-right-arrow-re-raw
  (concat
   ;; asdf :: Int
   "\\([[:alpha:]][A-Za-z0-9_]+\\) +:: \\([^ ]+\\)"
   "\\|"
   ;; [(x,y)|x<-[1..3],y<-[4,5]]
   "\\([[:alpha:]][A-Za-z0-9_]+\\) +:: +\\([[:alpha:]]+[A-Za-z0-9_]*\\),\\([[:alpha:]]+[A-Za-z0-9_]*\\) +|")
  ""
  ;; :type '(repeat string)
  :type '(repeat regexp)
  :tag "general-close-pre-right-arrow-re-raw"
  :group 'python-mode)

(defvar general-close-pre-right-arrow-re ""
  "Content of this var is controlled by customizable `general-close-pre-right-arrow-re-raw'")

(setq general-close-pre-right-arrow-re-raw
      (concat
       ;; asdf :: Int
       "\\([[:alpha:]][A-Za-z0-9_]+\\) +:: \\([[:alnum:]]+\\)"
       "\\|"
       ;; add :: (Int,Int)
       "\\([[:alpha:]][A-Za-z0-9_]+\\) +:: +\\([[:alpha:]]+[A-Za-z0-9_]*\\),\\([[:alpha:]]+[A-Za-z0-9_]*\\) +|")
      ;; [(x,y)|x<-[1..3],y<-[4,5]]

      )

(setq general-close-pre-right-arrow-re
  (concat
   "[ \t]*\\_<"
   general-close-pre-right-arrow-re-raw
   "\\_>[ \t]*"))
;; (setq general-close-pre-right-arrow-re "\\([[:alpha:]][A-Za-z0-9_]+\\) +:: \\([[:alnum:]]+\\)$")

(defvar general-close-typedef-re "[a-z][A-Za-z_]* *$")

(defvar general-close-default-type "Int")

(defvar general-close-default-type-re "[[:alpha:]][A-Za-z0-9_]+[ \t]+::[ \t]*$")

(defvar general-close-emacs-lisp-block-re
  (concat
   "[ \t]*\\_<"
   "(if\\|(cond\\|when\\|unless"
   "\\_>[ \t]*"))

;; (string-match "[ 	]*\\((defun\\|(defmacro\\)\\_>[ 	]*" "(defun asdf")
(defvar general-close-emacs-lisp-function-re
  (concat
   "[ \t]*"
   "(defun\\|(defmacro"
   "\\_>[ \t]*"))

(defvar sml-block-re (list "abstraction" "abstype" "and" "andalso" "as" "before" "case"
                 "datatype" "else" "end" "eqtype" "exception" "do" "fn"
                 "fun" "functor" "handle" "if" "in" "include" "infix"
                 "infixr" "let" "local" "nonfix" "o" "of" "op" "open" "orelse"
                 "overload" "raise" "rec" "sharing" "sig" "signature"
                 "struct" "structure" "then" "type" "val" "where" "while"
                 "with" "withtype"))

(defvar general-close-sml-fun-after-arglist-re
  (concat
   "[ \t]*"
   "fun"
   "\\_>[ \t]*"))

(defvar general-close-sml-function-before-arglist-re
  (concat
   "[ \t]*"
   "fun"
   "\\_>[ \t]+[^(]+$"))
(setq general-close-sml-function-before-arglist-re
  (concat
   "[ \t]*"
   "fun"
   "\\_>[ \t]+[^(]+$"))

(defvar general-close-sml-assignment-re   "[ \t]*val[ \t]+[[:alpha:]][A-Za-z0-9_]*\\_>[ \t]*")
(setq general-close-sml-assignment-re   "[ \t]*val[ \t]+[[:alpha:]][A-Za-z0-9_]*\\_>[ \t]*")

(setq general-close-emacs-lisp-function-re
  (concat
   "[ \t]*\\("
   "(defun\\|(defmacro"
   "\\)\\_>[ \t]*"))

(defcustom general-close-command-operator-chars
  "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*"
  "Matches most of syntactical meaningful characters, inclusive whitespaces around. "
  :type 'regexp
  :tag "general-close-command-operator-chars"
  :group 'general-close)

(defcustom general-close-default-argument-1
  "x"
  "Insert a \"x\" maybe when general-close-guess-p is `t'. "
  :type 'string
  :tag "general-close-default-argument-1"
  :group 'general-close)

(defcustom general-close-default-argument-2
  "y"
  "Insert an \"y\" maybe when general-close-guess-p is `t'. "
  :type 'string
  :tag "general-close-default-argument-2"
  :group 'general-close)

(defvar general-close-verbose-p nil)

(defvar general-close-keywords nil
  "Knowing keywords avoids call for face-at-point:

conditionals closed by a colon for example. ")

(unless (boundp 'py-block-re)
  (defvar py-block-re "[ \t]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. "))

(defvar general-close-command-separator-char ?\;
  "This char might be modified internally. ")

(defvar general-close-list-separator-char ?,
  "This char might be modified internally. ")

(defvar general-close-known-comint-modes (list 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode)
  "`parse-partial-sexp' must scan only from last prompt. ")
;; (setq general-close-known-comint-modes (list 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode))

(defvar general-close-empty-line-p-chars "^[ \t\r]*$")
(defcustom general-close-empty-line-p-chars "^[ \t\r]*$"
  "general-close-empty-line-p-chars"
  :type 'regexp
  :group 'convenience)

(defvar general-close--current-source-buffer (current-buffer)
  "Used by modes loading source from comint-shell")

(defun general-close--set-current-source-buffer ()
  (interactive)
  "Set value of `general-close--current-source-buffer' to current buffer. "
  (setq general-close--current-source-buffer (current-buffer)))

(defun general-close-toggle-electric-listify ()
  "Switch the value of `general-close-electric-listify-p' in current session. "
  (interactive)
  (setq general-close-electric-listify-p (not general-close-electric-listify-p))
  (when (called-interactively-p 'any) (message "general-close-electric-listify-p: %s" general-close-electric-listify-p)))

(defun general-close-empty-line-p (&optional iact)
  "Returns t if cursor is at an empty line, nil otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at general-close-empty-line-p-chars)))
    (looking-at general-close-empty-line-p-chars)))

(defun general-close-toggle-verbosity ()
  "If `general-close-verbose-p' is nil, switch it on.

Otherwise switch it off. "
  (interactive)
  (setq general-close-verbose-p (not general-close-verbose-p))
  (when (called-interactively-p 'any) (message "general-close-verbose-p: %s" general-close-verbose-p)))

(defun general-close--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\". "
  (cond ((eq erg ?\")
	 erg)
	((eq erg ?')
	 erg)
	((eq erg ?\[)
	 ?\])
	((eq erg ?\])
	 ?\[)
	((eq erg ?{)
	 ?\})
	((eq erg ?})
	 ?\{)
	((eq erg ?\))
	 ?\()
	((eq erg ?\()
	 ?\))
	))

(defun general-close--return-complement-string-maybe (erg)
  (cond
   ((string= erg "{-")
    "-}")
   ))

(defun general-close--in-string-p-intern (pps)
  "Return the delimiting string. "
  (goto-char (nth 8 pps))
  (buffer-substring-no-properties (point) (progn  (skip-chars-forward (char-to-string (char-after))) (point))))

(defun general-close-in-string-maybe (&optional pps)
  "if inside a double- triple- or singlequoted string,

Return delimiting chars "
  (interactive)
  (save-excursion
    (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
	   (erg (when (nth 3 pps)
		  (general-close--in-string-p-intern pps))))
      (unless erg
	(when (looking-at "\"")
	  (forward-char 1)
	  (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	  (when (nth 3 pps)
	    (setq erg (general-close--in-string-p-intern pps)))))

    ;; (list (nth 8 pps) (char-before) (1+ (skip-chars-forward (char-to-string (char-before)))))
    (when (and general-close-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg)))

(defun general-close-stack-based ()
  "Command will insert closing delimiter whichever needed.

Does not require parenthesis syntax WRT \"{[(\" "
  (interactive "*")
  (let (closer stack done)
    (save-excursion
      (while (and (not (bobp)) (not done))
	(cond ((member (char-before) (list ?\) ?\] ?}))
	       (push (char-before) stack)
	       (forward-char -1))
	      ((member (char-before) (list ?\( ?\" ?{ ?\[))
	       (setq closer (general-close--return-complement-char-maybe (char-before)))
	       (if (eq (car stack) closer)
		   (progn
		     (pop stack)
		     (forward-char -1))
		 (setq done t)))
	      (t (skip-chars-backward "^\"{\(\[\]\)}")))))
    (insert closer)))

(defun general-close--list-inside-string-maybe (strg)
  (with-temp-buffer
    (insert strg)
    ;; (switch-to-buffer (current-buffer))
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (when (nth 1 pps)
	(save-excursion
	  (goto-char (nth 1 pps))
	  (general-close--return-complement-char-maybe (char-after)))))))

(defun nth-1-pps-complement-char-maybe (pps)
  "Return complement character from (nth 1 pps). "
  (save-excursion
    (goto-char (nth 1 pps))
    (general-close--return-complement-char-maybe (char-after))))

(defun general-close--fetch-electric-delimiter-maybe (pps &optional force)
  ""
  (cond ((and (nth 1 pps)(not force)
	      (save-excursion
		(progn
		  (skip-chars-backward " \t\r\n\f")
		  (eq (char-before) general-close-list-separator-char))))
	 (char-before (1- (point))))
	((and (nth 1 pps)(not force)
	      (save-excursion
		(progn
		  (skip-chars-backward " \t\r\n\f")
		  (member (char-before (1- (point))) (list ?' ?\")))))
	 (char-before (1- (point))))
	((and (not force) (nth 1 pps)
	      (save-excursion
		(progn (skip-chars-backward " \t\r\n\f")
		       (not (eq (char-before) general-close-list-separator-char)))))
	 general-close-list-separator-char)))

(defun general-close-in-string-interpolation-maybe (&optional pps)
  "Return nearest openener.

Check if list opener inside a string. "
  (interactive)
  (let ((pps (or pps (parse-partial-sexp (point-min) (point))))
	erg last)
    (cond ((and (nth 3 pps)
		;; paren inside string maybe
		(setq erg (nth 1 (setq last (parse-partial-sexp (1+ (nth 8 pps)) (point)))))(< (nth 8 pps) erg))
	   (setq erg (nth-1-pps-complement-char-maybe last)))
	  ((nth 3 pps)
	   (setq erg (general-close-in-string-maybe pps))))
    erg))

;; See also general-close--guess-symbol
(defun general-close--fetch-delimiter-maybe (pps)
  "Close the innermost list resp. string. "
  (let (erg closer strg)
    (cond
     ((nth 3 pps)
      (cond ((setq closer (general-close-in-string-interpolation-maybe pps)))

	    (t (save-excursion
		 (setq strg (buffer-substring-no-properties (1+ (nth 8 pps)) (point)))
		 (if (setq closer (general-close--list-inside-string-maybe strg))
		     closer
		   ;; returns a list to construct TQS maybe
		   (and (setq erg (general-close--in-string-p-intern pps))
			(setq closer (make-string (nth 2 erg)(nth 1 erg)))))
		 closer))))
     ((and (member major-mode general-close--singlequote-modes) (eq (char-before (1- (point))) ?'))
      "'")
     ((and (nth 1 pps)
	   (eq ?\] (setq erg (nth-1-pps-complement-char-maybe pps))))
      erg)
     ((nth 1 pps)
      (save-excursion
	(goto-char (nth 1 pps))
	(general-close--return-complement-char-maybe (char-after)))))))

(defun general-close--insert-delimiter-char-maybe (orig closer)
  (when closer
    (save-excursion
      (when (and (not (looking-back "^[ \t]+" nil))
                 general-close-delete-whitespace-backward-p
		 (< 0 (abs (skip-chars-backward " \t\r\n\f")))
		 ;;  not in comment
		 (not (nth 4 (parse-partial-sexp (point-min) (point)))))
	(delete-region (point) orig)))
    (cond
     ((and (eq closer ?}) (not (eq major-mode 'php-mode)))
      (insert closer)
      closer)
     ((not (eq closer ?}))
      (insert closer)
      closer))))

(defun general-close--insert-separator-maybe (orig)
  "Returns `t', if separator was inserted. "
  (let (erg)
    (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
      (delete-region (point) orig))
    (when
	(not (eq (char-before) general-close-command-separator-char))
      (when (save-excursion
	      (forward-char -1)
	      (when (ignore-errors (setq erg (nth 1 (parse-partial-sexp (point-min) (point)))))
		(goto-char erg))
	      (back-to-indentation)
	      ;; ert does no font-lock
	      (or (and general-close-keywords (looking-at general-close-keywords))
		  (face-at-point)))
	(insert general-close-command-separator-char) t))))

(defun general-close--handle-separator-modes (orig closer)
  "Some languages close expressions with a special char, often `:'

See `general-close-command-separator-char'"
  (let (done)
    (cond ((eq closer ?})
	   (if
	       (save-excursion
		 (skip-chars-backward " \t\r\n\f")
		 (or (eq (char-before) general-close-command-separator-char)
		     (eq (char-before) closer)))
	       (progn
		 (unless (looking-back "^[ \t]+" nil)
		   (newline-and-indent))
		 (insert closer)
		 (setq done t))
	     (insert general-close-command-separator-char)
	     (setq done t)))
	  ((and (eq closer ?\)) (eq (char-before) ?\;))
	   (newline-and-indent)
	   (insert closer)
	   closer)
	  ;; Semicolon inserted where it probably shouldn't be? #12
	  ;; ((and (eq closer ?\)) (eq (char-before) ?\)))
	  ;;  (insert general-close-command-separator-char)
	  ;;  closer)
	  ((and closer general-close-electric-listify-p (not (eq (char-before) general-close-list-separator-char)))
	   (skip-chars-backward " \t\r\n\f")
	   (insert general-close-list-separator-char)
	   (setq done t))
	  (closer
	   (skip-chars-backward " \t\r\n\f")
	   (insert closer)
	   closer)
	  (t (general-close--insert-separator-maybe orig)))
    done))

(defun general-close-insert-with-padding-maybe (strg &optional nbefore nafter)
  "Takes a string. Insert a space before and after maybe.

When `general-close-insert-with-padding-p' is `t', the default "
  (skip-chars-backward " \t\r\n\f")
  (if general-close-insert-with-padding-p
      (cond ((looking-back "([ \t]*" (line-beginning-position))
	     (delete-region (match-beginning 0) (match-end 0))
	     (insert strg)
	     (insert " "))
	    ((looking-at "[ \t]*)")
	     (delete-region (match-beginning 0) (1- (match-end 0)))
	     (insert " ")
	     (insert strg))
	    (t (unless nbefore (insert " "))
	       (insert strg)
	       (unless
		   (or
		    (eq 5 (car (syntax-after (point))))
		    ;; (eq (char-after) ?\))
		    nafter) (insert " "))))))

(defun general-close--semicolon-separator-modes-dispatch (orig closer pps)
  (let ((closer (or closer (and (nth 1 pps) (nth-1-pps-complement-char-maybe pps))))
	done erg)
    (cond ((and closer (eq closer ?\))(progn (save-excursion (skip-chars-backward " \t\r\n\f")(looking-back general-close-command-operator-chars (line-beginning-position)))))
	   (setq erg (car (general-closer-uniq-varlist (nth 1 pps) orig)))
	   (cond ((and (stringp erg)(< 1 (length erg)))
		  (general-close-insert-with-padding-maybe erg)
		  (setq done t))
		 ((and (stringp erg)(eq 1 (length erg)))
		  (general-close-insert-with-padding-maybe
		   (general-close--raise-symbol-maybe (string-to-char erg)))
		  (setq done t))))
	  ((progn (save-excursion (beginning-of-line) (looking-at general-close-pre-assignment-re)))
	   (general-close-insert-with-padding-maybe "=")
	   (setq done t))
	  (t (setq general-close-command-separator-char 59)
	     (setq done (general-close--handle-separator-modes orig closer))))
    done))

(defun general-close--others (orig closer pps)
  (let (done erg)
    (cond
     ((nth 3 pps)
      (cond ((characterp (nth 3 pps))
	     (insert (nth 3 pps)))
	    ((setq erg (general-close-in-string-interpolation-maybe pps))
	     (general-close--return-complement-char-maybe erg))
	    (t (general-close--return-complement-char-maybe (nth 8 pps))
	))
      (setq done t))
     ;; a command separator may precede closing delimiter
     ((and
       ;; (nth 1 pps)
       (member major-mode general-close--semicolon-separator-modes))
      (setq done (general-close--semicolon-separator-modes-dispatch orig closer pps)))
     ((and (not (nth 1 pps)) (member major-mode general-close--colon-separator-modes))
      (setq general-close-command-separator-char ?\:)
      (setq done (general-close--handle-separator-modes orig closer)))
     (closer (setq done (general-close--insert-delimiter-char-maybe orig closer)))
     (t (setq done (general-close--insert-assignment-maybe (line-beginning-position) general-close-pre-assignment-re))))
    done))

(defun general-close--comint-send ()
  (let (done)
    (comint-send-input)
    (goto-char (point-max))
    (setq done t)
    done))

(defun general-close--insert-and-fixup (strg)
  (if (eq (char-before) ?\ )
      (insert (concat strg " "))
      (insert (concat " " strg " "))))

(defun general-close--insert-assignment-maybe (beg regexp)
  (let (done)
    (when (save-excursion
	    (goto-char beg)
	    (skip-chars-forward " \t\r\n\f")
	    (looking-at regexp))
      (general-close-insert-with-padding-maybe "=")
      (setq done t))
    done))

(defun general-close--repeat-type-maybe (beg regexp)
  (let (done)
    (when (save-excursion
	    (skip-chars-backward " \t\r\n\f")
	    (and (looking-back "->" beg)
	    (goto-char beg)
	    (looking-at regexp)))
      (fixup-whitespace)
      (if (eq (char-after) ?\ )
	  (forward-char 1)
	(insert 32))
      (insert (match-string-no-properties 2))
      (setq done t))
    done))

(defun general-close--right-arrow-maybe (beg regexp)
  (let (done)
    (when (save-excursion
	    (goto-char beg)
	    (skip-chars-forward " \t\r\n\f")
	    (looking-at regexp))
      (general-close-insert-with-padding-maybe "->")
      (setq done t))
    done))

(defun general-close--default-type-maybe (beg regexp)
  (let (done)
    (when (save-excursion
	    (and
	     (goto-char beg)
	     (ar-previous-line-empty-or-BOB-p)
	     (skip-chars-forward " \t\r\n\f")
	     (looking-at regexp)))
      (general-close-insert-with-padding-maybe general-close-default-type)
      (setq done t))
    done))

(defun general-close--typedef-maybe (beg regexp)
  (let (done)
    (when (save-excursion
	    (and
	     (goto-char beg)
	     (ar-previous-line-empty-or-BOB-p)
	     (skip-chars-forward " \t\r\n\f")
	     (looking-at regexp)))
      (general-close-insert-with-padding-maybe "::")
      (setq done t))
    done))

(defun general-close--which-right-arrow-regex ()
  (cond ((member major-mode  (list 'haskell-interactive-mode 'inferior-haskell-mode))
	 general-close-comint-pre-right-arrow-re)
	(t general-close-pre-right-arrow-re)))

(defun general-close-comint (beg &optional closer)
  (let ((right-arrow-re (general-close--which-right-arrow-regex))
	done)
    (cond (closer
	   (insert closer)
	   (setq done t))
	  ((eq (char-before) general-close-command-separator-char)
	   (setq done (general-close--comint-send)))
	  ((setq done (general-close--right-arrow-maybe beg right-arrow-re)))
	  ;; if looking back at "let myVar " assume "="
	  ((setq done (general-close--insert-assignment-maybe beg general-close-pre-assignment-re)))
	  (t (insert general-close-command-separator-char)
	     (setq done (general-close--comint-send))))
    done))

(defun general-close--comments-intern (orig start end)
  (if (looking-at start)
      (progn (goto-char orig)
	     (insert end))
    (goto-char orig)
    (newline-and-indent)))

(defun general-close--insert-comment-end-maybe (pps)
  (let ((orig (point))
	done)
    (cond
     ((eq major-mode 'haskell-mode)
      (goto-char (nth 8 pps))
      (general-close--comments-intern orig "{-" "-}")
      (setq done t))
     ((or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
      (goto-char (nth 8 pps))
      (general-close--comments-intern orig "/*" "*/")
      (setq done t))
     (t (if (string= "" comment-end)
	    (if (eq system-type 'windows-nt)
		(insert "\r\n")
	      (insert "\n"))
	  (insert comment-end))
	(setq done t) ))
    done))

(defun general-close--travel-comments-maybe (pps)
  (let (done)
    (or (and (nth 4 pps) (nth 8 pps)
	     ;; (not (string= "" comment-end))
	     (setq done (general-close--insert-comment-end-maybe pps)))
	(while (and (setq pps (parse-partial-sexp (line-beginning-position) (point))) (nth 4 pps) (nth 8 pps))
	  (unless (eobp)
	    (forward-line 1)
	    (end-of-line)
	    (skip-chars-backward " \t\r\n\f" (line-beginning-position)))))
    done))

(defun general-close--point-min ()
  (cond ((and (member major-mode (list 'haskell-interactive-mode 'inferior-haskell-mode)))
	 haskell-interactive-mode-prompt-start)
	((save-excursion
	   (and (member major-mode general-close-known-comint-modes) comint-prompt-regexp
		(message "%s" (current-buffer))
		(re-search-backward comint-prompt-regexp nil t 1)
		(looking-at comint-prompt-regexp)
		(message "%s" (match-end 0))))
	 (match-end 0))
	(t (point-min))))

(defun general-close--in-known-comint (beg &optional closer)
  (let (done)
    (setq done (general-close-comint beg closer))
    (unless done
      ;; maybe no char, but input to send
      (comint-send-input)
      (newline)
      (setq done t))
    done))

(defun general-close--common (beg pps)
  (let ((closer (general-close--fetch-delimiter-maybe pps))
	done)
    (when (member major-mode general-close-known-comint-modes)
      (setq done (general-close--in-known-comint beg closer)))
    (unless done
      (when closer
	(unless (and (eq closer ?})(member major-mode general-close--semicolon-separator-modes))
	  (insert closer)
	  (setq done t))))
    done))

(defun general-close--guess-list-element-delimiter ()
  (save-excursion
    (forward-char -1)
    (cond
     ((eq (char-before) general-close-list-element-delimiter-1)
      general-close-list-element-delimiter-1)
     ((eq (char-before) general-close-list-element-delimiter-2)
      general-close-list-element-delimiter-2))))

(defun general-close--cleanup-inserts ()
  ;; (when (< 0 (abs (skip-chars-backward (concat "^" (char-to-string general-close-list-separator-char)))))
  ;;   (delete-region (point) orig))
  (skip-chars-backward " \t\r\n\f")
  (let ((orig (point))
	(pps (parse-partial-sexp (point-min) (point))))
    (cond ((eq (char-before) general-close-list-separator-char)
	   (delete-char -1)
	   (when (< (point) orig)
	     (general-close--cleanup-inserts)))
	  ((and (nth 3 pps)(eq 1 (nth 0 pps))(eq 7 (syntax-class (syntax-after (1- (point))))))
	   (delete-char -1)
	   (when (< (point) orig)
	     (general-close--cleanup-inserts))))))

(defun general-close-fetch-delimiter (pps)
  "In some cases in (nth 3 pps only returns `t'. "
  (save-excursion
    (goto-char (nth 8 pps))
    (char-after)))

(defun general-close--electric (pps closer &optional force)
  (let (done separator)
    (if (and closer force)
	(progn
	  ;; (general-close--cleanup-inserts)
	  (insert closer)
	  (setq done t))
      (cond ((nth 3 pps)
	     (if
		 (eq t (nth 3 pps))
		 (insert (char-to-string (general-close-fetch-delimiter pps)))
	       (insert (nth 3 pps)))
	     (setq done t))
	    ((eq (char-before) general-close-list-separator-char)
	     ;; open a new list element
	     (if (setq separator (general-close--guess-list-element-delimiter))
		 (progn
		   (insert separator)
		   (setq done t))
	       (delete-char -1)
	       (insert closer)
	       (setq done t)))
	    (t (insert general-close-list-separator-char)
	       (setq done t))))
    done))

(defun general-close--guess-from-string-interpolation-maybe (pps)
  "Returns the character of innermost sexp in inside. "
  (when (and (nth 1 pps) (nth 3 pps))
    (let* ((listchar (save-excursion (goto-char (nth 1 pps))
				     (char-after)))
	   (inner-listpos (progn
			    (skip-chars-backward (concat "^" (char-to-string listchar)))
			    (1- (point)))))
      (if
	  (< (nth 8 pps) inner-listpos)
	  (general-close--return-complement-char-maybe listchar)
	(save-excursion (goto-char (nth 8 pps))(char-after))))))

(defun general-close--guess-closer (pps)
  (save-excursion
    (cond ((and (nth 1 pps) (nth 3 pps))
	   (if (general-close--guess-from-string-interpolation-maybe pps)
	       (progn
		 (goto-char (nth 1 pps))
		 (general-close--return-complement-char-maybe (char-after)))
	     (progn (goto-char (nth 8 pps)) (char-after)))))))

;; Ml
(defun general-close-ml ()
  (interactive "*")
  (let ((oldmode major-mode) done)
    (cond ((save-excursion
	     (and (< 0 (abs (skip-syntax-backward "w")))
		  (not (bobp))
		  ;; (syntax-after (1- (point)))
		  (or (eq ?< (char-before (point)))
		      (and (eq ?< (char-before (1- (point))))
			   (eq ?/ (char-before (point)))))))
	   (insert ">")
	   (setq done t))
	  (t (when (eq ?> (char-before (point)))(newline))
	     (sgml-mode)
	     (sgml-close-tag)
	     (funcall oldmode)
	     (font-lock-fontify-buffer)
	     (setq done t)))
    done))

(defun general-close-python-listclose (list-separator-char closer force pps electric)
  "If inside list, assume another item first. "
  (let (done)
    (cond ((and force (eq (char-before) list-separator-char))
	   (delete-char -1)
	   (insert closer)
	   (setq done t))
	  ((member (char-before) (list ?' ?\"))
	   (if force
	       (progn
		 (insert closer)
		 ;; only closing `"' or `'' was inserted here
		   (when (setq closer (general-close--fetch-delimiter-maybe (parse-partial-sexp (point-min) (point))))
		     (insert closer))
		 (setq done t))
	     (if (nth 3 pps)
		 (insert (char-before))
	       (insert ","))
	     (unless electric
	       (setq done t))))
	  ((eq (char-before) list-separator-char)
	   (if electric
	       (progn
		 (save-excursion
		   (forward-char -1)
		   (setq closer (char-before)))
		 (insert closer))
	     (delete-char -1)
	     (insert closer))
	   (setq done t))
	  (t (insert closer)
	     (setq done t)))
    done))

;; Emacs-lisp
(defun general-close-emacs-lisp-close (closer pps)
  (let ((closer (or closer (general-close--fetch-delimiter-maybe pps)))
	done)
    (cond
     ((and (nth 1 pps) (nth 3 pps)
	   ;; (if (< (nth 1 pps) (nth 8 pps))
	   (looking-back "\\[\\[:[a-z]+" (line-beginning-position)))
      (insert ":")
      (setq done t))
     ((and (eq 2 (nth 1 pps)) (looking-back "\\[\\[:[a-z]+" (1- (nth 1 pps))))
      (insert ":")
      (setq done t))
     ((and (eq 1 (nth 1 pps))
	   (save-excursion
	     (beginning-of-line)
	     (looking-at general-close-emacs-lisp-function-re)))
      (general-close-insert-with-padding-maybe "()" nil t)
      (setq done t))
     ((save-excursion
	(skip-chars-backward " \t\r\n\f")
	(looking-back general-close-emacs-lisp-block-re (line-beginning-position)))
      (general-close-insert-with-padding-maybe (char-to-string 40) t t))
     (closer
      (skip-chars-backward " \t\r\n\f" (line-beginning-position))
      (insert closer)
      (setq done t)))
    done))

;; See also general-close--fetch-delimiter-maybe - redundancy?
(defun general-close--guess-symbol (&optional pos)
  (save-excursion
    (let ((erg (when pos
		 (progn (goto-char pos)
			(char-after))))
	  end)
      (unless erg
	(save-excursion
	  (progn
	    (forward-char -1)
	    (skip-chars-backward "[:punct:] \t")
	    (when (looking-back "[[:alpha:]]+" (line-beginning-position))
	      (setq end (point))
	      (skip-chars-backward "[:alnum:]" (line-beginning-position))
	      (setq erg (buffer-substring-no-properties (point) end))))))
      (when (string= "" erg)
	(setq erg (cond ((member (char-before (1- (point))) (list ?' ?\"))
			 (char-before (1- (point)))))))
      (unless
	  (or (characterp erg)(< 1 (length erg))(string= "" erg))
	(setq erg (string-to-char erg)))
      erg)))

(defun general-close--raise-symbol-maybe (symbol)
  "Return the symbol following in asci decimal-values.

If at char `z', follow up with `a'
If arg SYMBOL is a string, return it unchanged"
  (if (stringp symbol)
      symbol
    (cond
     ((eq 122 symbol)
      ;; if at char `z', follow up with `a'
      97)
     ((eq symbol 90)
      65)
     ((and (< symbol 123)(< 96 symbol))
      (1+ symbol))
     ((and (< symbol 133)(< 64 symbol))
      (1+ symbol))
     ;; raise until number 9
     ((and (< 47 symbol)(< symbol 57))
      (1+ symbol))
     (t symbol))))

(defun general-close-python-electric-close (list-separator-char pps closer force)
  (let (done)
    (cond
     ((and closer (eq 2 (nth 0 pps))
	   (eq 1 (car (syntax-after (1- (point))))))
      (insert (general-close--guess-symbol))
      (setq done t))
     ((and closer (eq 2 (nth 0 pps)))
      (when (eq 2 (car (syntax-after (1- (point)))))
	(insert list-separator-char)
	(setq done t)))
     ;; simple lists
     ((eq 1 (car (syntax-after (1- (point)))))
      ;; translate a single char into its successor
      ;; if multi-char symbol, repeat
      (insert (general-close--raise-symbol-maybe (general-close--guess-symbol)))
      (setq done t))
     ((and closer
	   (eq 1 (nth 0 pps)) (not (nth 3 pps))
	   (not (member (char-before) (list ?\] list-separator-char))))
      (insert list-separator-char)
      (setq done t)
      (when force
	(general-close-python-close list-separator-char closer pps force nil nil list-separator-char)))
     ((and closer
	   (not (eq (char-before) closer)))
      (insert closer)
      (setq done t))
     (closer
      (setq done (general-close--electric pps closer force))
      (unless (eq (char-before) list-separator-char)
	(general-close-python-close list-separator-char closer pps force)))
     (t (error "general-close-python-electric-close: nothing found")))
    done))

;; Python
(defun general-close-python-close (list-separator-char &optional closer pps force b-of-st b-of-bl electric)
  "Might deliver equivalent to `py-dedent'"
  (interactive "*")
  (let* ((closer (or closer
		     (general-close--fetch-delimiter-maybe (or pps (parse-partial-sexp (point-min) (point))))))
	 done)
    (if closer
	(progn
	  (insert closer)
	  (setq done t))
      (let* (
	     (pps (or pps (parse-partial-sexp (point-min) (point))))
	     (general-close-beginning-of-statement
	      (or b-of-st
		  (if (ignore-errors (functionp 'py-backward-statement))
		      'py-backward-statement
		    (lambda ()(beginning-of-line)(back-to-indentation)))))
	     (general-close-beginning-of-block-re (or b-of-bl "[ 	]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n	]*"))
	     done)
	(cond
	 ;; nested lists,
	 ;; Inside a list-comprehension
	 ((and (nth 1 pps) (not (member closer (list ?\) ?\" ?'))) electric)
	  (setq done (general-close-python-electric-close list-separator-char pps closer force)))
	 (closer
	  (setq done (general-close-python-listclose list-separator-char closer force pps electric)))
	 ((and (not (char-equal ?: (char-before)))
	       (save-excursion
		 (funcall general-close-beginning-of-statement)
		 (looking-at general-close-beginning-of-block-re)))
	  (insert ":")
	  (setq done t))
	 ((and (nth 3 pps)(setq closer (general-close-in-string-maybe))(setq done t))
	  (insert closer))
	 (t (eolp)
	    (ignore-errors (newline-and-indent))
	    (setq done t)))
	done)
      done)))

;; Ruby
(defun general-close--generic-fetch-delimiter-maybe ()
  (save-excursion
    (and (< 0 (abs (skip-syntax-backward "\\sw")))
	 (or
	  (eq 1 (car (syntax-after (1- (point)))))
	  (eq 7 (car (syntax-after (1- (point))))))
	 (char-to-string (char-before)))))

(defun general-close--ruby-insert-end ()
  (let (done)
    (unless (or (looking-back ";[ \t]*" nil))
      (unless (and (bolp)(eolp))
	(newline))
      (unless (looking-back "^[^ \t]*\\_<end" nil)
	(insert "end")
	(setq done t)
	(save-excursion
	  (back-to-indentation)
	  (indent-according-to-mode))))
    done))

(defun general-close-ruby-close (&optional closer pps)
  (let ((closer (or closer
		    (and pps (general-close--fetch-delimiter-maybe pps))
		    (general-close--generic-fetch-delimiter-maybe)))
	done)
    (if closer
	(progn
	  (insert closer)
	  (setq done t))
      (setq done (general-close--ruby-insert-end))
      done)))

(defun general-close--insert-string-concat-op-maybe ()
  (let (done)
    (save-excursion
      (skip-chars-backward " \t\r\n\f")
      (and (or (eq (char-before) ?') (eq (char-before) ?\"))
	   (progn
	     (forward-char -1)
	     (setq done (nth 3 (parse-partial-sexp (point-min) (point)))))))
    (when done
      (fixup-whitespace)
      (if (eq (char-before) ?\ )
	  (insert "++ ")
	(insert " ++ ")))
    done))

(defun general-closer-forward-sexp-maybe (pos)
  (ignore-errors (forward-sexp))
  (when (< pos (point))(point)))

(defun general-closer-uniq-varlist (&optional beg end pps)
  "Return a list of variables existing in buffer-substring. "
  (save-excursion
    (let* (sorted
	   (pos (point))
	   (beg (or beg (ignore-errors (nth 1 pps))
		    (or (nth 1 pps)
			(nth 1 (parse-partial-sexp (point-min) (point))))))
	   (end (or end
		    (save-excursion
		      (goto-char beg)
		      (or (general-closer-forward-sexp-maybe pos))
			  pos)))

	   (varlist (split-string (buffer-substring-no-properties beg end) "[[:punct:][0-9 \r\n\f\t]" t)))
      (dolist (elt varlist)
	(unless (member elt sorted)
	  (push elt sorted)))
      (setq sorted (nreverse sorted))
      sorted)))

(defun general-close-insert-var-in-listcomprh (list-separator-char pps &optional sorted splitpos)
  ;; which var of sorted to insert?
  (let* ((sorted sorted)
	 (splitpos (or splitpos
		       (save-excursion (and (skip-chars-backward "^|" (line-beginning-position))(eq (char-before) ?|)(1- (point))))
		       (and (eq (char-before) list-separator-char) (1- (point)))))
	 done vars-at-point candidate)
    (if splitpos
	(progn
	  (setq vars-at-point
		(general-closer-uniq-varlist splitpos (line-end-position) pps))
	  (setq vars-at-point (nreverse vars-at-point))
	  (setq candidate
		(if vars-at-point
		    (cond ((not (or (eq 2 (nth 1 pps))
				    (eq (length vars-at-point) (length sorted))))
			   ;; (eq (member (car vars-at-point) sorted)
			   (nth (length vars-at-point) sorted)))
		  ;; sorted))
		  (cond ((looking-back "<-[ \t]*" (line-beginning-position))
			 "[")
			((looking-back "|[ \t]*" (line-beginning-position))
			 (car sorted))
			((looking-back (char-to-string list-separator-char) (line-beginning-position))
			 (if (< 1 (length (car sorted)))
			     (car sorted)
			   (general-close--raise-symbol-maybe (string-to-char (car sorted)))))
			(t "<-"))))))
    (unless done
      (when candidate
	;; general-close-list-comprehension-test-16
	(general-close-insert-with-padding-maybe candidate)
	(setq done t)))
    done))

(defun general-close-haskell-twofold-list-cases (list-separator-char pps &optional closer electric)
  (let* ((sorted (save-excursion (general-closer-uniq-varlist nil nil pps)))
	 done)
    ;; [(a*b+a) |a<-[1..3],b<-[4..5]]
    (cond
     (;; after a punct-character
      (and closer electric
	   (eq 1 (car (syntax-after (1- (point))))))
      ;; translate a single char into its successor
      ;; if multi-char symbol, repeat
      (insert (general-close--raise-symbol-maybe (general-close--guess-symbol)))
      (setq done t))
     ((and closer electric
	   (eq 2 (car (syntax-after (1- (point)))))(not (save-excursion (progn (skip-chars-backward "[:alnum:]")(skip-chars-backward " \t\r\n\f")(eq (char-before) list-separator-char)))))
      (insert list-separator-char)
      (setq done t))
     ((and closer electric
	   (not (eq 1 (car (syntax-after (1- (point)))))))
      ;; works but not needed (?)
      (save-excursion
	(goto-char (nth 1 pps))
	(setq closer (general-close--return-complement-char-maybe (char-after))))
      (insert closer)
      (setq done t))
     ((and closer electric)
      ;; Inside a list-comprehension
      (when (eq 2 (car (syntax-after (1- (point)))))
	(insert list-separator-char)
	(setq done t)))
     (t (setq done (general-close-insert-var-in-listcomprh list-separator-char pps sorted))))
    done))

(defun general-close-haskell-close-in-list-comprehension (list-separator-char pps orig)
  (let ((splitpos
	 (+ (line-beginning-position)
	    ;; position in substring
	    (string-match "|" (buffer-substring-no-properties (line-beginning-position) (point)))))
	sorted done)
    (cond ((and splitpos (progn (save-excursion (skip-chars-backward " \t\r\n\f")(eq (char-before) ?\]))))
	   (skip-chars-backward " \t\r\n\f")
	   (insert list-separator-char)
	   (setq done t))
	  (t (goto-char splitpos)
	     (skip-chars-backward "^)\[" (line-beginning-position))
	     (and (eq (char-before) ?\))(forward-char -1))
	     (setq sorted (general-closer-uniq-varlist nil splitpos (parse-partial-sexp (line-beginning-position) (point))))
	     (goto-char orig)
	     (setq done
		   (general-close-insert-var-in-listcomprh list-separator-char pps sorted splitpos))))
    done))

(defun general-close-haskell-electric-splitter-forms (list-separator-char &optional closer pps orig)
  (let (done)
    (cond ((and (not closer)
    		(not (save-excursion (progn (skip-chars-backward " \t\r\n\f")(member (char-before) (list ?| list-separator-char))))))
    	   (cond ((looking-back "<-[ \t]*" (line-beginning-position))
    		  (general-close-insert-with-padding-maybe "[" nil t)
    		  (setq done t))
    		 ((save-excursion (skip-chars-backward " \t\r\n\f") (eq (char-before) ?\]))
    		  (insert list-separator-char)
    		  (setq done t))
		 ((looking-back "| +[[:alnum:]]+" (line-beginning-position))
    		  (general-close-insert-with-padding-maybe "<-"))
    		 ((nth 1 pps)
    		  (skip-chars-backward " \t\r\n\f")
    		  (insert (nth-1-pps-complement-char-maybe pps))
    		  (setq done t))
    		 (t (general-close-insert-with-padding-maybe "<-")
    		    (setq done t))))
	   (t (setq done (general-close-haskell-close-in-list-comprehension list-separator-char pps orig))))
    done))

(defun general-close-haskell-electric-close (list-separator-char &optional closer pps orig)
  (let* ((splitter (and (eq 1 (count-matches "|" (line-beginning-position) (point)))))
	 (closer (or closer
		     (unless splitter
		       ;; with `|' look for arrows needed
		       (or (and pps (general-close--fetch-delimiter-maybe pps))
			   (general-close--generic-fetch-delimiter-maybe)))))
	 (electric t)
	 done erg)
    (cond
     ((and (eq 2 (nth 0 pps))(member (char-before) (list ?\( ?\[))(eq (char-before (1- (point))) list-separator-char))
      (insert (general-close--raise-symbol-maybe (general-close--guess-symbol)))
      (setq done t))
     ;; Default list var
     ((and
	(nth 1 pps)
       ;; (eq 2 (nth 0 pps))
    	   (setq erg
		 (car-safe (member (char-before) (list ?\( ?\[))))
	   (looking-back
	    (concat "[a-z][A-Za-z_]* +\\(::\\|=\\) +.?"
		    (regexp-quote (char-to-string erg))) (line-beginning-position)))
      ;; (line-beginning-position))))
      (insert general-close-default-argument-1)
      (setq done t))
     ((ignore-errors (eq closer ?\]))
      (insert closer)
      (setq done t))
     (splitter
      (setq done (general-close-haskell-electric-splitter-forms list-separator-char closer pps orig)))
     ((and (eq 2 (nth 0 pps))(not (eq ?\] closer)))
      (setq done (general-close-haskell-twofold-list-cases list-separator-char pps closer electric)))
     ((and (eq (char-before) list-separator-char)(not (car-safe (member (char-before (1- (point))) (list ?\) ?\])))))
      (insert (general-close--raise-symbol-maybe (general-close--guess-symbol)))
      (setq done t))
     ((eq (char-before) list-separator-char)(car-safe (member (char-before (1- (point))) (list ?\) ?\])))
      (insert (general-close--return-complement-char-maybe (char-before (1- (point)))))
      (setq done t))
     ((setq done (general-close--repeat-type-maybe (line-beginning-position) general-close-pre-right-arrow-re)))
     ((and (eq 1 (nth 0 pps)) (eq ?\) closer))
      (insert closer)
      (setq done t))
     ((setq done (general-close--right-arrow-maybe (line-beginning-position) general-close-pre-right-arrow-re)))
     ((looking-back ":: [[:alnum:],()]+?" (line-beginning-position))
      (general-close-insert-with-padding-maybe "->")
      (setq done t))
     ((setq done (general-close--typedef-maybe (line-beginning-position) general-close-typedef-re)))
     ((setq done (general-close--default-type-maybe (line-beginning-position) general-close-default-type-re)))
     (closer
      (insert closer)
      (setq done t))
     ((setq done (general-close--insert-assignment-maybe (line-beginning-position) general-close-pre-assignment-re)))
     ((setq done (general-close--insert-string-concat-op-maybe))))
    done))

(defun general-close-haskell-non-electric (list-separator-char &optional closer pps orig)
  (let* ((splitter (and (eq 1 (count-matches "|" (line-beginning-position) (point)))))
	 (closer (or closer
		     (unless splitter
		       ;; with `|' look for arrows needed
		       (or (and pps (general-close--fetch-delimiter-maybe pps))
			   (general-close--generic-fetch-delimiter-maybe)))))
	 done)
    (cond
     ((and (eq 2 (nth 0 pps))(not (eq ?\] closer)))
      (setq done (general-close-haskell-twofold-list-cases list-separator-char pps closer)))
     ((and splitter (eq ?\] closer))
      (skip-chars-backward " \t\r\n\f")
      (insert closer)
      (setq done t))
     ;; in list-comprehension
     ;; [(a,b) |
     ;; not just after pipe
     ((and splitter (not closer)
	   (not (save-excursion (progn (skip-chars-backward " \t\r\n\f")(member (char-before) (list ?| list-separator-char))))))
      (cond ((looking-back "<-[ \t]*" (line-beginning-position))
	     (general-close-insert-with-padding-maybe "[")
	     (setq done t))
	    ((save-excursion (skip-chars-backward " \t\r\n\f") (eq (char-before) ?\]))
	     (insert list-separator-char)
	     (setq done t))
	    ((nth 1 pps)
	     (skip-chars-backward " \t\r\n\f")
	     (insert (nth-1-pps-complement-char-maybe pps))
	     (setq done t))
	    (t (general-close-insert-with-padding-maybe "<-")
	       (setq done t)))
      (setq done t))
     (splitter
      ;; after pipe fetch a var
      (setq done (general-close-haskell-close-in-list-comprehension list-separator-char pps  orig)))
     ((setq done (general-close--repeat-type-maybe (line-beginning-position) general-close-pre-right-arrow-re)))
     ((and (eq 1 (nth 0 pps)) (eq ?\) closer))
      (insert closer)
      (setq done t))
     ((setq done (general-close--right-arrow-maybe (line-beginning-position) general-close-pre-right-arrow-re)))
     (closer
      (insert closer)
      (setq done t))
     ((setq done (general-close--insert-assignment-maybe (line-beginning-position) general-close-pre-assignment-re)))
     ((setq done (general-close--insert-string-concat-op-maybe))))
    done))

(defun general-close-haskell-close (list-separator-char &optional closer pps orig electric)
  (let (erg done)
    (cond ((and (nth 4 pps)
		(save-excursion
		  (goto-char (nth 8 pps))
		  (looking-at "{-")
		  ;; plain comment closed by newline
		  (setq erg (general-close--return-complement-string-maybe "{-"))))
	   (insert erg)
	   (setq done t)))
    (unless done
      (if electric
	  (setq done (general-close-haskell-electric-close list-separator-char closer pps orig))
	(setq done (general-close-haskell-non-electric list-separator-char closer pps orig))))
    done))

(defun general-close-inferior-sml-close ()
  (let (done)
    (cond ((looking-back comint-prompt-regexp (line-beginning-position))
	   (if general-close--current-source-buffer
	       (insert (concat "use \"" (buffer-name general-close--current-source-buffer) "\";"))
	     (insert "use \"\";")
	     (forward-char -2))
	   (setq done t)))
    done))

(defun general-close-sml-close (&optional pps)
  (let (done)
    (cond
     (;; type-colon
      (and (eq 1 (nth 0 pps))
	   (save-excursion
	     (progn
	       (back-to-indentation)
	       (looking-at (concat general-close-sml-fun-after-arglist-re)))))
      (general-close-insert-with-padding-maybe ":")
      (setq done t))
     (;; fun foo
      (and (not (eq 1 (nth 0 pps)))
	   (save-excursion
	     (progn
	       (back-to-indentation)
	       (looking-at (concat general-close-sml-function-before-arglist-re)))))
      (general-close-insert-with-padding-maybe "(" nil t)
      (setq done t))
     (;; assignment
      (looking-back general-close-sml-assignment-re (line-beginning-position)) 
      (general-close-insert-with-padding-maybe "=")
      (setq done t))
     (;; function body assignment
      (save-excursion
	(and
	 (progn
	   (skip-chars-backward " \t\r\n\f")
	   (eq (char-before) ?\)))
	 (progn
	   (back-to-indentation)
	   (looking-at general-close-sml-fun-after-arglist-re))))
      (general-close-insert-with-padding-maybe "=")
      (setq done t)))
    done))

;; (let ((frame (window-frame window))
;; (buffer-list frame)

;; (message "%s" (window--side-check)))))
;; Php
(defun general-close--php-check (pps &optional closer)
  (let ((closer (or closer (general-close--fetch-delimiter-maybe pps)))
	(orig (point))
	done)
    (cond ((and (eq closer ?})(general-close-empty-line-p))
	   (insert closer)
	   (setq done t)
	   (indent-according-to-mode))
	  ((eq closer ?})
	   (if (or (eq (char-before) ?\;) (eq (char-before) closer))
	       (progn
		 (newline)
		 (insert closer)
		 (indent-according-to-mode))
	     (insert ";"))
	   (setq done t))
	  ((eq closer ?\))
	   (insert closer)
	   (setq done t))
	  ;; after asignement
	  ((eq (char-before) ?\))
	   (backward-list)
	   (skip-chars-backward "^ \t\r\n\f")
	   (skip-chars-backward " \t")
	   (when (eq (char-before) ?=)
	     (goto-char orig)
	     (insert ";")
	     (setq done t))))
    (unless done (goto-char orig))
    done))

(defvar general-close-haskell-listcomprh-vars nil)

(defvar general-close-haskell-listcomprh-startpos nil)
(defvar general-close-haskell-listcomprh-counter nil)

(defun general-close-set-listcomprh-update (orig pps)
  (let (pos varlist)
    (setq general-close-haskell-listcomprh-counter 0)
    (cond ((save-excursion (and (nth 0 pps) (goto-char (nth 1 pps))(eq (char-after) ?\[))(setq pos (point)))
	   (goto-char pos)
	   (while (re-search-forward haskell-var-re orig t 1)
	     ;; (unless (member (match-string-no-properties 0) varlist)
	     (cl-pushnew (match-string-no-properties 0) varlist))
	   (goto-char orig)
	   (nreverse varlist)))))

(defun general-close--modes (pps orig list-separator-char &optional closer force electric)
  (let ((closer (or closer (general-close--fetch-delimiter-maybe pps)))
	done)
    (pcase major-mode
      (`inferior-sml-mode
       (setq done (general-close-inferior-sml-close)))
      (`sml-mode
      (setq done (general-close-sml-close pps)))
      (`python-mode
       (setq done (general-close-python-close list-separator-char closer pps force nil nil electric)))
      (`emacs-lisp-mode
       (setq done (general-close-emacs-lisp-close closer pps)))
      (`ruby-mode
       (setq done (general-close-ruby-close closer pps)))
      (_
       (cond
	((member major-mode general-close--ml-modes)
	 (setq done (general-close-ml)))
	((member major-mode (list 'php-mode 'js-mode 'web-mode))
	 (setq done (general-close--php-check pps closer)))
	((member major-mode (list 'haskell-interactive-mode 'inferior-haskell-mode 'haskell-mode))
	 (setq done (general-close-haskell-close list-separator-char closer pps orig electric))))
       done))))

(provide 'general-close-modes)
;;; general-close-modes.el ends here
