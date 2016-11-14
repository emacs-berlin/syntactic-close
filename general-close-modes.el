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

(defun general-close-python-listclose (closer force pps separator-char electric)
  "If inside list, assume another item first. "
  (let (done)
    (cond ((and force (eq (char-before) separator-char))
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
	  ((eq (char-before) general-close-list-separator-char)
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
	  beg end)
      (unless erg
	(save-excursion
	  (progn
	    (forward-char -1)
	    (skip-chars-backward "[:punct:] \t")
	    (when (looking-back "[[:alpha:]]+" (line-beginning-position) )
	      (setq end (point))
	      (skip-chars-backward "[:alnum:]" (line-beginning-position))
		 (setq erg (buffer-substring-no-properties (point) end))
	    ))))
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

(defun general-close-python-electric-close (pps closer force)
  (let (done)
    (cond
     ((and closer (eq 2 (nth 0 pps))
	   (eq 1 (car (syntax-after (1- (point))))))
      (insert (general-close--guess-symbol))
      (setq done t))
     ((and closer (eq 2 (nth 0 pps)))
      (when (eq 2 (car (syntax-after (1- (point)))))
	(insert general-close-list-separator-char)
	(setq done t)))
     ;; simple lists
     ((eq 1 (car (syntax-after (1- (point)))))
      ;; translate a single char into its successor
      ;; if multi-char symbol, repeat
      (insert (general-close--raise-symbol-maybe (general-close--guess-symbol)))
      (setq done t))
     ((and closer
	   (eq 1 (nth 0 pps)) (not (nth 3 pps))
	   (not (member (char-before) (list ?\] general-close-list-separator-char))))
      (insert general-close-list-separator-char)
      (setq done t)
      (when force
	(general-close-python-close closer pps force nil nil general-close-list-separator-char)))
     ((and closer
	   (not (eq (char-before) closer)))
      (insert closer)
      (setq done t))
     (closer
      (setq done (general-close--electric pps closer force))
      (unless (eq (char-before) general-close-list-separator-char)
	(general-close-python-close closer pps force)))
     (t (error "general-close-python-electric-close: nothing found")))
    done))

;; Python
(defun general-close-python-close (&optional closer pps force b-of-st b-of-bl separator-char electric)
  "Might deliver equivalent to `py-dedent'"
  (interactive "*")
  (let* ((closer (or closer
		     (general-close--fetch-delimiter-maybe (or pps (parse-partial-sexp (point-min) (point))))))
	 (separator-char (or separator-char general-close-list-separator-char))
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
	  (setq done (general-close-python-electric-close pps closer force)))
	 (closer
	  (setq done (general-close-python-listclose closer force pps separator-char electric)))
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

(defun general-close-insert-var-in-listcomprh (pps &optional sorted splitpos)
  ;; which var of sorted to insert?
  (let* ((sorted sorted)
	 (splitpos (or splitpos
		       (save-excursion (and (skip-chars-backward "^|" (line-beginning-position))(eq (char-before) ?|)(1- (point))))
		       (and (eq (char-before) general-close-list-separator-char) (1- (point)))))
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
			((looking-back "|[ \t]*")
			 (car sorted))
			((looking-back (char-to-string general-close-list-separator-char) (line-beginning-position))
			 (if (< 1 (length (car sorted)))
			     (car sorted)
			   (general-close--raise-symbol-maybe (string-to-char (car sorted)))))
			(t "<-"))))))
    (unless done
      (when candidate
	(general-close-insert-with-padding-maybe candidate t t)
	(setq done t)))
    ;; (insert closer)
    ;; (setq done t)
    done))

(defun general-close-haskell-twofold-list-cases (pps &optional closer electric)
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
	   (eq 2 (car (syntax-after (1- (point)))))(not (save-excursion (progn (skip-chars-backward "[:alnum:]")(skip-chars-backward " \t\r\n\f")(eq (char-before) general-close-list-separator-char)))))
      (insert general-close-list-separator-char)
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
	(insert general-close-list-separator-char)
	(setq done t)))
     (t (setq done (general-close-insert-var-in-listcomprh pps sorted))))
    done))

(defun general-close-haskell-close-in-list-comprehension (pps orig)
  (let ((splitpos
	 (+ (line-beginning-position)
	    ;; position in substring
	    (string-match "|" (buffer-substring-no-properties (line-beginning-position) (point)))))
	sorted done)
    (cond ((and splitpos (progn (save-excursion (skip-chars-backward " \t\r\n\f")(eq (char-before) ?\]))))
	   (skip-chars-backward " \t\r\n\f")
	   (insert general-close-list-separator-char)
	   (setq done t))
	  (t (goto-char splitpos)
	     (skip-chars-backward "^)\[" (line-beginning-position))
	     (and (eq (char-before) ?\))(forward-char -1))
	     (setq sorted (general-closer-uniq-varlist nil splitpos (parse-partial-sexp (line-beginning-position) (point))))
	     (goto-char orig)
	     (setq done
		   (general-close-insert-var-in-listcomprh pps sorted splitpos))))
    done))

(defun general-close-haskell-electric-splitter-forms (&optional closer pps orig)
  (let (done)
    (cond ((and (not closer)
    		(not (save-excursion (progn (skip-chars-backward " \t\r\n\f")(member (char-before) (list ?| general-close-list-separator-char))))))
    	   (cond ((looking-back "<-[ \t]*")
    		  (general-close-insert-with-padding-maybe "[" nil t)
    		  (setq done t))
    		 ((save-excursion (skip-chars-backward " \t\r\n\f") (eq (char-before) ?\]))
    		  (insert general-close-list-separator-char)
    		  (setq done t))
		 ((looking-back "| +[[:alnum:]]+")
    		  (general-close-insert-with-padding-maybe "<-"))
    		 ((nth 1 pps)
    		  (skip-chars-backward " \t\r\n\f")
    		  (insert (nth-1-pps-complement-char-maybe pps))
    		  (setq done t))
    		 (t (general-close-insert-with-padding-maybe "<-")
    		    (setq done t))))
	   (t (setq done (general-close-haskell-close-in-list-comprehension pps orig))))
    done))

(defun general-close-haskell-electric-close (&optional closer pps orig)
  (let* ((splitter (and (eq 1 (count-matches "|" (line-beginning-position) (point)))))
	 (closer (or closer
		     (unless splitter
		       ;; with `|' look for arrows needed
		       (or (and pps (general-close--fetch-delimiter-maybe pps))
			   (general-close--generic-fetch-delimiter-maybe)))))
	 (electric t)
	 done erg)
    (cond
     ((and (eq 2 (nth 0 pps))(member (char-before) (list ?\( ?\[))(eq (char-before (1- (point))) general-close-list-separator-char))
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
		    (regexp-quote (char-to-string erg)))))
      ;; (line-beginning-position))))
      (insert general-close-default-argument-1)
      (setq done t))
     ;; ((and (nth 1 pps)
     ;; 	   (setq erg
     ;; 		 (car-safe (member (char-before) (list ?\( ?\[))))
     ;; 	   (looking-back
     ;; 	    (concat "[a-z][A-Za-z_]* += +.?"
     ;; 		    (regexp-quote (char-to-string erg)))))
      ;; (insert general-close-default-argument-1)
      ;; (setq done t))
     ((ignore-errors (eq closer ?\]))
      (insert closer)
      (setq done t))
     (splitter
      (setq done (general-close-haskell-electric-splitter-forms closer pps orig)))
     ((and (eq 2 (nth 0 pps))(not (eq ?\] closer)))
      (setq done (general-close-haskell-twofold-list-cases pps closer electric)))
     ((and (eq (char-before) general-close-list-separator-char)(not (car-safe (member (char-before (1- (point))) (list ?\) ?\])))))
      (insert (general-close--raise-symbol-maybe (general-close--guess-symbol)))
      (setq done t))
     ((eq (char-before) general-close-list-separator-char)(car-safe (member (char-before (1- (point))) (list ?\) ?\])))
      (insert (general-close--return-complement-char-maybe (char-before (1- (point)))))
      (setq done t))
     ((setq done (general-close--repeat-type-maybe (line-beginning-position) general-close-pre-right-arrow-re)))
     ((and (eq 1 (nth 0 pps)) (eq ?\) closer))
      (insert closer)
      (setq done t))
     ((setq done (general-close--right-arrow-maybe (line-beginning-position) general-close-pre-right-arrow-re)))
     ((looking-back ":: [[:alnum:],()]+?")
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
      (setq done (general-close-haskell-twofold-list-cases pps closer)))
     ((and splitter (eq ?\] closer))
      (skip-chars-backward " \t\r\n\f")
      (insert closer)
      (setq done t))
     ;; in list-comprehension
     ;; [(a,b) |
     ;; not just after pipe
     ((and splitter (not closer)
	   (not (save-excursion (progn (skip-chars-backward " \t\r\n\f")(member (char-before) (list ?| list-separator-char))))))
      (cond ((looking-back "<-[ \t]*")
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
      (setq done (general-close-haskell-close-in-list-comprehension pps  orig)))
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
	  (setq done (general-close-haskell-electric-close closer pps orig))
	(setq done (general-close-haskell-non-electric list-separator-char closer pps orig))))
    done))

(defun general-close-inferior-sml-close ()
  (let (done)
    (cond ((looking-back comint-prompt-regexp)
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
      (looking-back general-close-sml-assignment-re)
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
       (setq done (general-close-python-close closer pps force nil nil nil electric)))
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
