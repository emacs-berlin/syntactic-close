;;; general-close.el --- Insert closing delimiter -*- lexical-binding: t; -*-

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

;;; Commentary: M-x general-close RET: close any syntactic element.

;; ['a','b' ==> ['a','b']

;; A first draft was published at emacs-devel list:
;; http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

;;; Code:

(require 'cl-lib)
(require 'sgml-mode)
(require 'comint)

(defgroup general-close nil
  "Insert closing delimiter whichever needed. "
  :group 'languages
  :tag "general-close"
  :prefix "general-close-")

(defcustom general-close-empty-line-p-chars "^[ \t\r]*$"
  "general-close-empty-line-p-chars"
  :type 'regexp
  :group 'convenience)

(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'general-close-empty-line-p))
(defun general-close-empty-line-p (&optional iact)
  "Returns t if cursor is at an empty line, nil otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at general-close-empty-line-p-chars)))
    (looking-at general-close-empty-line-p-chars)))

(defvar haskell-interactive-mode-prompt-start (ignore-errors (require 'haskell-interactive-mode) haskell-interactive-mode-prompt-start)
  "Defined in haskell-interactive-mode.el, silence warnings. ")

(defvar general-close--current-source-buffer nil
  "Set by `general-close--set-current-source-buffer' maybe.

Default is nil.
Comint-modes might want to load stuff from " )

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

(defvar general-close-list-separator-char 44)
(defcustom general-close-list-separator-char 44
  "Char separating elements of a list.

Default is `,'"

  :type 'character
  :tag "general-close-list-separator-char"
  :group 'general-close)
(make-variable-buffer-local 'general-close-list-separator-char)

(defcustom general-close-guess-p nil
  "When non-nil, guess default arguments, list-separators etc. "
  :type 'boolean
  :tag "general-close-guess-p"
  :group 'general-close)
(make-variable-buffer-local 'general-close-guess-p)

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

(defvar general-close-pre-assignment-re "[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]*[ \t]*$\\|[[:alpha:]][A-Za-z0-9_]*[ \t]*$")

(setq general-close-pre-assignment-re   "[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]*[ \t]*$\\|[[:alpha:]][A-Za-z0-9_]*[ \t]*$")

(defcustom general-close-pre-assignment-re
  "[[:alpha:]][A-Za-z0-9_]+[ \t]*[^=]*$"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-pre-assignment-re"
  :group 'general-close)

(defvar general-close-emacs-lisp-block-re
  (concat
   "[ \t]*\\_<"
   "(if\\|(cond\\|when\\|unless"
   "\\_>[ \t]*"))

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

(defcustom general-close-command-operator-chars
  "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*"
  "Matches most of syntactical meaningful characters, inclusive whitespaces around. "
  :type 'regexp
  :tag "general-close-command-operator-chars"
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

(defvar general-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode)
  "`parse-partial-sexp' must scan only from last prompt. ")
(setq general-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode))

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
	  ((and (eq closer ?\)) (eq (char-before) ?\)))
	   (insert general-close-command-separator-char)
	   closer)
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
    (cond
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
	 (ignore-errors haskell-interactive-mode-prompt-start))
	((save-excursion
	   (and (member major-mode general-close-known-comint-modes) comint-prompt-regexp
		(message "%s" (current-buffer))
		(re-search-backward comint-prompt-regexp nil t 1)
		(looking-at comint-prompt-regexp)
		(message "%s" (match-end 0))))
	 (match-end 0))
	(t (point-min))))

(defun general-close--common (pps)
  (let ((closer (general-close--fetch-delimiter-maybe pps))
	done)
    (when closer
      (unless (and (eq closer ?})(member major-mode general-close--semicolon-separator-modes))
	(insert closer)
	(setq done t)))
    done))

(defun general-close--cleanup-inserts ()
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

(defun general-close-python-listclose (closer force pps)
  "If inside list, assume another item first. "
  (let (done)
    (cond ((member (char-before) (list ?' ?\"))
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
	     (setq done t)))
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
     ;; ((and (eq 1 (nth 1 pps))
     ;; 	   (save-excursion
     ;; 	     (beginning-of-line)
     ;; 	     (looking-at general-close-emacs-lisp-function-re)))
     ;;  (general-close-insert-with-padding-maybe "()" nil t)
     ;;  (setq done t))
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

(defun general-close--raise-symbol-maybe (symbol)
  "Return the symbol following in asci decimal-values.

If at char `z', follow up with `a'
If arg SYMBOL is a string, return it unchanged"
  (cond
   ((stringp symbol)
    (cond ((string-match "^[0-9]+$" symbol)
	   (prin1-to-string (1+ (car (read-from-string symbol)))))
	  (t symbol)))
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
   (t (prin1-to-string (1+ (car (read-from-string (char-to-string symbol))))))))

(defun general-close-python-close (closer pps force b-of-st b-of-bl)
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
	 (closer
	  (setq done (general-close-python-listclose closer force pps)))
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

(defun general-close--modes (pps orig &optional closer force)
  (let ((closer (or closer (general-close--fetch-delimiter-maybe pps)))
	done)
    (pcase major-mode
      (`inferior-sml-mode
       (setq done (general-close-inferior-sml-close)))
      (`sml-mode
       (setq done (general-close-sml-close pps)))
      (`python-mode
       (setq done (general-close-python-close closer pps force nil nil )))
      (`emacs-lisp-mode
       (setq done (general-close-emacs-lisp-close closer pps)))
      (`ruby-mode
       (setq done (general-close-ruby-close closer pps)))
      (_
       (cond
	((member major-mode general-close--ml-modes)
	 (setq done (general-close-ml)))
	((member major-mode (list 'php-mode 'js-mode 'web-mode))
	 (setq done (general-close--php-check pps closer))))
       done))))

(defun general-close-intern (beg iact &optional force)
  (let* ((orig (point))
	 (pps (parse-partial-sexp beg (point)))
	 (verbose general-close-verbose-p)
	 done closer)
    (cond
     ((setq done (general-close--modes pps orig closer force)))
     ((setq done (general-close--others orig closer pps)))
     ((setq done (general-close--common pps))))
    (or (< orig (point)) (and iact verbose (message "%s" "nil")))))

(defun general-close (&optional arg beg force)
  "Command will insert closing delimiter whichever needed.

With \\[universal-argument]: close everything at point. "
  (interactive "P*")
  (let ((beg (or beg (general-close--point-min)))
	(iact (called-interactively-p 'any)))
    (pcase (prefix-numeric-value arg)
      (4 (general-close-intern beg iact t))
      (_ (general-close-intern beg iact force)))))

(provide 'general-close)
;;; general-close.el ends here
