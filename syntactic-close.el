;;; syntactic-close.el --- Insert closing delimiter -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Version: 0.1
;; Keywords: languages, lisp

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

;;; Commentary: M-x syntactic-close RET: close any syntactic element.

;; ['a','b' ==> ['a','b']

;; A first draft was published at emacs-devel list:
;; http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

;;; Code:

(require 'cl-lib)
(require 'sgml-mode)
(require 'comint)

(defgroup syntactic-close nil
  "Insert closing delimiter whichever needed. "
  :group 'languages
  :tag "syntactic-close"
  :prefix "syntactic-close-")

(defcustom syntactic-close-empty-line-p-chars "^[ \t\r]*$"
  "syntactic-close-empty-line-p-chars"
  :type 'regexp
  :group 'convenience)

(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'syntactic-close-empty-line-p))
(defun syntactic-close-empty-line-p (&optional iact)
  "Returns t if cursor is at an empty line, nil otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at syntactic-close-empty-line-p-chars)))
    (looking-at syntactic-close-empty-line-p-chars)))

(defvar haskell-interactive-mode-prompt-start (ignore-errors (require 'haskell-interactive-mode) haskell-interactive-mode-prompt-start)
  "Defined in haskell-interactive-mode.el, silence warnings. ")

(defcustom syntactic-close-delete-whitespace-backward-p nil
  "If whitespace characters before point should be deleted.

Default is nil"

  :type 'boolean
  :tag "syntactic-close-delete-whitespace-backward-p"
  :group 'syntactic-close)

(defcustom syntactic-close-insert-with-padding-p t
  "Ensure a whitespace character before point.

Default is t"

  :type 'boolean
  :tag "syntactic-close-insert-with-padding-p"
  :group 'syntactic-close)

(defcustom syntactic-close-guess-p nil
  "When non-nil, guess default arguments, list-separators etc. "
  :type 'boolean
  :tag "syntactic-close-guess-p"
  :group 'syntactic-close)
(make-variable-buffer-local 'syntactic-close-guess-p)

(defcustom syntactic-close--semicolon-separator-modes
  (list
   'inferior-sml-mode
   'js-mode
   'js2-mode
   'perl-mode
   'php-mode
   'sml-mode
   'web-mode
   )
  "List of modes which commands must be closed by `syntactic-close-command-separator-char. "

  :type 'list
  :tag "syntactic-close--semicolon-separator-modes"
  :group 'syntactic-close)

(defcustom syntactic-close--ml-modes
  (list
   'html-mode
   'nxml-mode
   'sgml-mode
   'xml-mode
   'xxml-mode
   )
  "List of modes using markup language. "

  :type 'list
  :tag "syntactic-close--semicolon-separator-modes"
  :group 'syntactic-close)

(defvar syntactic-close-pre-assignment-re "[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]*[ \t]*$\\|[[:alpha:]][A-Za-z0-9_]*[ \t]*$")

(setq syntactic-close-pre-assignment-re   "[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]*[ \t]*$\\|[[:alpha:]][A-Za-z0-9_]*[ \t]*$")

(defcustom syntactic-close-pre-assignment-re
  "[[:alpha:]][A-Za-z0-9_]+[ \t]*[^=]*$"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "syntactic-close-pre-assignment-re"
  :group 'syntactic-close)

(defvar syntactic-close-emacs-lisp-block-re
  (concat
   "[ \t]*\\_<"
   "(if\\|(cond\\|when\\|unless"
   "\\_>[ \t]*"))

(defvar syntactic-close-sml-fun-after-arglist-re
  (concat
   "[ \t]*"
   "fun"
   "\\_>[ \t]*"))

(defvar syntactic-close-sml-function-before-arglist-re
  (concat
   "[ \t]*"
   "fun"
   "\\_>[ \t]+[^(]+$"))
(setq syntactic-close-sml-function-before-arglist-re
  (concat
   "[ \t]*"
   "fun"
   "\\_>[ \t]+[^(]+$"))

(defvar syntactic-close-sml-assignment-re   "[ \t]*val[ \t]+[[:alpha:]][A-Za-z0-9_]*\\_>[ \t]*")
(setq syntactic-close-sml-assignment-re   "[ \t]*val[ \t]+[[:alpha:]][A-Za-z0-9_]*\\_>[ \t]*")

(defvar syntactic-close-verbose-p nil)

(defvar syntactic-close-keywords nil
  "Knowing keywords avoids call for face-at-point:

conditionals closed by a colon for example. ")

(unless (boundp 'py-block-re)
  (defvar py-block-re "[ \t]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. "))

(defvar syntactic-close-command-separator-char ?\;
  "This char might be modified internally. ")

(defvar syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode)
  "`parse-partial-sexp' must scan only from last prompt. ")
(setq syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode))

(defvar syntactic-close-empty-line-p-chars "^[ \t\r]*$")
(defcustom syntactic-close-empty-line-p-chars "^[ \t\r]*$"
  "syntactic-close-empty-line-p-chars"
  :type 'regexp
  :group 'convenience)

(defun syntactic-close-toggle-verbosity ()
  "If `syntactic-close-verbose-p' is nil, switch it on.

Otherwise switch it off. "
  (interactive)
  (setq syntactic-close-verbose-p (not syntactic-close-verbose-p))
  (when (called-interactively-p 'any) (message "syntactic-close-verbose-p: %s" syntactic-close-verbose-p)))

(defun syntactic-close--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\". "
  (pcase erg
    (34 ?\")
    (?' ?')
    (?\( ?\))
    (?\) ?\()
    (?\] ?\[)
    (?\[ ?\])
    (?} ?{)
    (?{ ?})))

(defun syntactic-close--return-complement-string-maybe (erg)
  (cond
   ((string= erg "{-")
    "-}")
   ))

(defun syntactic-close--in-string-p-intern (pps)
  "Return the delimiting string. "
  (goto-char (nth 8 pps))
  (buffer-substring-no-properties (point) (progn  (skip-chars-forward (char-to-string (char-after))) (point))))

(defun syntactic-close-in-string-maybe (&optional pps)
  "if inside a double- triple- or singlequoted string,

Return delimiting chars "
  (interactive)
  (save-excursion
    (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
	   (erg (when (nth 3 pps)
		  (syntactic-close--in-string-p-intern pps))))
      (unless erg
	(when (looking-at "\"")
	  (forward-char 1)
	  (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	  (when (nth 3 pps)
	    (setq erg (syntactic-close--in-string-p-intern pps)))))
      (when (and syntactic-close-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

;; currently unused
(defun syntactic-close-stack-based ()
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
	       (setq closer (syntactic-close--return-complement-char-maybe (char-before)))
	       (if (eq (car stack) closer)
		   (progn
		     (pop stack)
		     (forward-char -1))
		 (setq done t)))
	      (t (skip-chars-backward "^\"{\(\[\]\)}")))))
    (insert closer)))

(defun nth-1-pps-complement-char-maybe (pps)
  "Return complement character from (nth 1 pps). "
  (save-excursion
    (goto-char (nth 1 pps))
    (syntactic-close--return-complement-char-maybe (char-after))))

(defun syntactic-close-in-string-interpolation-maybe (&optional pps)
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
	   (setq erg (syntactic-close-in-string-maybe pps))))
    erg))

;; See also syntactic-close--guess-symbol
(defun syntactic-close--fetch-delimiter-maybe (pps)
  "Close the innermost list resp. string. "
  (let (erg closer strg)
    (cond
     ((nth 3 pps)
      (cond ((setq closer (syntactic-close-in-string-interpolation-maybe pps)))

	    (t (save-excursion
		 (setq strg (buffer-substring-no-properties (1+ (nth 8 pps)) (point)))
		 (if (setq closer (syntactic-close--list-inside-string-maybe strg))
		     closer
		   ;; returns a list to construct TQS maybe
		   (and (setq erg (syntactic-close--in-string-p-intern pps))
			(setq closer (make-string (nth 2 erg)(nth 1 erg)))))
		 closer))))
     ;; ((and (member major-mode syntactic-close--singlequote-modes) (eq (char-before (1- (point))) ?'))
     ;;  "'")
     ((nth 1 pps)
      (save-excursion
	(goto-char (nth 1 pps))
	(syntactic-close--return-complement-char-maybe (char-after)))))))

(defun syntactic-close--insert-delimiter-char-maybe (orig closer)
  (when closer
    (save-excursion
      (when (and (not (looking-back "^[ \t]+" nil))
                 syntactic-close-delete-whitespace-backward-p
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

(defun syntactic-close-insert-with-padding-maybe (strg &optional nbefore nafter)
  "Takes a string. Insert a space before and after maybe.

When `syntactic-close-insert-with-padding-p' is `t', the default "
  (skip-chars-backward " \t\r\n\f")
  (if syntactic-close-insert-with-padding-p
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

(defun syntactic-close--others (orig closer pps)
  (let (done erg)
    (cond
     ((nth 3 pps)
      (cond ((characterp (nth 3 pps))
	     (insert (nth 3 pps)))
	    ((setq erg (syntactic-close-in-string-interpolation-maybe pps))
	     (syntactic-close--return-complement-char-maybe erg))
	    (t (syntactic-close--return-complement-char-maybe (nth 8 pps))))
      (setq done t))
     (closer (setq done (syntactic-close--insert-delimiter-char-maybe orig closer))))
    done))

(defun syntactic-close--comments-intern (orig start end)
  (if (looking-at start)
      (progn (goto-char orig)
	     (insert end))
    (goto-char orig)
    (newline-and-indent)))

(defun syntactic-close--insert-comment-end-maybe (pps)
  (let ((orig (point))
	done)
    (cond
     ((eq major-mode 'haskell-mode)
      (goto-char (nth 8 pps))
      (syntactic-close--comments-intern orig "{-" "-}")
      (setq done t))
     ((or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
      (goto-char (nth 8 pps))
      (syntactic-close--comments-intern orig "/*" "*/")
      (setq done t))
     (t (if (string= "" comment-end)
	    (if (eq system-type 'windows-nt)
		(insert "\r\n")
	      (insert "\n"))
	  (insert comment-end))
	(setq done t) ))
    done))

(defun syntactic-close--travel-comments-maybe (pps)
  (let (done)
    (or (and (nth 4 pps) (nth 8 pps)
	     ;; (not (string= "" comment-end))
	     (setq done (syntactic-close--insert-comment-end-maybe pps)))
	(while (and (setq pps (parse-partial-sexp (line-beginning-position) (point))) (nth 4 pps) (nth 8 pps))
	  (unless (eobp)
	    (forward-line 1)
	    (end-of-line)
	    (skip-chars-backward " \t\r\n\f" (line-beginning-position)))))
    done))

(defun syntactic-close--point-min ()
  (cond ((and (member major-mode (list 'haskell-interactive-mode 'inferior-haskell-mode)))
	 (ignore-errors haskell-interactive-mode-prompt-start))
	((save-excursion
	   (and (member major-mode syntactic-close-known-comint-modes) comint-prompt-regexp
		(message "%s" (current-buffer))
		(re-search-backward comint-prompt-regexp nil t 1)
		(looking-at comint-prompt-regexp)
		(message "%s" (match-end 0))))
	 (match-end 0))
	(t (point-min))))

(defun syntactic-close--common (closer)
  (let (done)
    (unless (and (eq closer ?})(member major-mode syntactic-close--semicolon-separator-modes))
      (insert closer)
      (setq done t))
    done))

(defun syntactic-close-fetch-delimiter (pps)
  "In some cases in (nth 3 pps only returns `t'. "
  (save-excursion
    (goto-char (nth 8 pps))
    (char-after)))

(defun syntactic-close--guess-from-string-interpolation-maybe (pps)
  "Returns the character of innermost sexp in inside. "
  (when (and (nth 1 pps) (nth 3 pps))
    (let* ((listchar (save-excursion (goto-char (nth 1 pps))
				     (char-after)))
	   (inner-listpos (progn
			    (skip-chars-backward (concat "^" (char-to-string listchar)))
			    (1- (point)))))
      (if
	  (< (nth 8 pps) inner-listpos)
	  (syntactic-close--return-complement-char-maybe listchar)
	(save-excursion (goto-char (nth 8 pps))(char-after))))))

(defun syntactic-close--guess-closer (pps)
  (save-excursion
    (cond ((and (nth 1 pps) (nth 3 pps))
	   (if (syntactic-close--guess-from-string-interpolation-maybe pps)
	       (progn
		 (goto-char (nth 1 pps))
		 (syntactic-close--return-complement-char-maybe (char-after)))
	     (progn (goto-char (nth 8 pps)) (char-after)))))))

;; Ml
(defun syntactic-close-ml ()
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

(defun syntactic-close-python-listclose (closer force pps)
  "If inside list, assume another item first. "
  (let (done)
    (cond ((member (char-before) (list ?' ?\"))
	   (if force
	       (progn
		 (insert closer)
		 ;; only closing `"' or `'' was inserted here
		 (when (setq closer (syntactic-close--fetch-delimiter-maybe (parse-partial-sexp (point-min) (point))))
		   (insert closer))
		 (setq done t))
	     (if (nth 3 pps)
		 (insert (char-before))
	       (insert ","))
	     (setq done t)))
	  (t (insert closer)
	     (setq done t)))
    done))

;; Emacs-lisp
(defun syntactic-close-emacs-lisp-close (closer pps)
  (let ((closer (or closer (syntactic-close--fetch-delimiter-maybe pps)))
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
     ((save-excursion
	(skip-chars-backward " \t\r\n\f")
	(looking-back syntactic-close-emacs-lisp-block-re (line-beginning-position)))
      (syntactic-close-insert-with-padding-maybe (char-to-string 40) t t))
     (closer
      (skip-chars-backward " \t\r\n\f" (line-beginning-position))
      (insert closer)
      (setq done t)))
    done))

;; See also syntactic-close--fetch-delimiter-maybe - redundancy?
(defun syntactic-close--guess-symbol (&optional pos)
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
	    (when (looking-back "[[:alnum:]]+" (line-beginning-position))
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

(defun syntactic-close-python-close (closer pps force b-of-st b-of-bl)
  "Might deliver equivalent to `py-dedent'"
  (interactive "*")
  (let* ((closer (or closer
		     (syntactic-close--fetch-delimiter-maybe (or pps (parse-partial-sexp (point-min) (point))))))
	 done)
    (if closer
	(progn
	  (insert closer)
	  (setq done t))
      (let* (
	     (pps (or pps (parse-partial-sexp (point-min) (point))))
	     (syntactic-close-beginning-of-statement
	      (or b-of-st
		  (if (ignore-errors (functionp 'py-backward-statement))
		      'py-backward-statement
		    (lambda ()(beginning-of-line)(back-to-indentation)))))
	     (syntactic-close-beginning-of-block-re (or b-of-bl "[ 	]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n	]*"))
	     done)
	(cond
	 (closer
	  (setq done (syntactic-close-python-listclose closer force pps)))
	 ((and (not (char-equal ?: (char-before)))
	       (save-excursion
		 (funcall syntactic-close-beginning-of-statement)
		 (looking-at syntactic-close-beginning-of-block-re)))
	  (insert ":")
	  (setq done t))
	 ((and (nth 3 pps)(setq closer (syntactic-close-in-string-maybe))(setq done t))
	  (insert closer)))
	done)
      done)))

;; Ruby
(defun syntactic-close--generic-fetch-delimiter-maybe ()
  (save-excursion
    (and (< 0 (abs (skip-syntax-backward "\\sw")))
	 (or
	  (eq 1 (car (syntax-after (1- (point)))))
	  (eq 7 (car (syntax-after (1- (point))))))
	 (char-to-string (char-before)))))

(defun syntactic-close--ruby-insert-end ()
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

(defun syntactic-close-ruby-close (&optional closer pps)
  (let ((closer (or closer
		    (and pps (syntactic-close--fetch-delimiter-maybe pps))
		    (syntactic-close--generic-fetch-delimiter-maybe)))
	done)
    (if closer
	(progn
	  (insert closer)
	  (setq done t))
      (setq done (syntactic-close--ruby-insert-end))
      done)))

(defun syntactic-close--insert-string-concat-op-maybe ()
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

(defun syntactic-closer-forward-sexp-maybe (pos)
  (ignore-errors (forward-sexp))
  (when (< pos (point))(point)))

(defun syntactic-close--php-check (pps &optional closer)
  (let ((closer (or closer (syntactic-close--fetch-delimiter-maybe pps)))
	(orig (point))
	done)
    (cond ((and (eq closer ?})(syntactic-close-empty-line-p))
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

(defun syntactic-close--modes (pps orig closer &optional force)
  (let (done)
    (pcase major-mode
      (`python-mode
       (setq done (syntactic-close-python-close closer pps force nil nil )))
      (`emacs-lisp-mode
       (setq done (syntactic-close-emacs-lisp-close closer pps)))
      (`ruby-mode
       (setq done (syntactic-close-ruby-close closer pps)))
      (_
       (cond
	((member major-mode syntactic-close--ml-modes)
	(setq done (syntactic-close-ml)))
	((member major-mode (list 'php-mode 'js-mode 'web-mode))
	 (setq done (syntactic-close--php-check pps closer))))
       done))))

(defun syntactic-close-intern (beg iact &optional force)
  (let* ((orig (point))
	 (pps (parse-partial-sexp beg (point)))
	 (verbose syntactic-close-verbose-p)
	 (closer (syntactic-close--fetch-delimiter-maybe pps))
	 done)
    (cond
     ((setq done (when closer (syntactic-close--common closer))))
     ((setq done (syntactic-close--modes pps orig closer force)))
     ((setq done (syntactic-close--others orig closer pps))))
    (or (< orig (point)) (and iact verbose (message "%s" "nil")))))

(defun syntactic-close (&optional arg beg force)
  "Command will insert closing delimiter whichever needed.

With \\[universal-argument]: close everything at point. "
  (interactive "P*")
  (let ((beg (or beg (syntactic-close--point-min)))
	(iact (called-interactively-p 'any)))
    (pcase (prefix-numeric-value arg)
      (4 (syntactic-close-intern beg iact t))
      (_ (syntactic-close-intern beg iact force)))))

(provide 'syntactic-close)
;;; syntactic-close.el ends here
