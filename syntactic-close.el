;; syntactic-close.el --- Insert closing delimiter -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Version: 0.1
;; Keywords: languages, convenience

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

(defcustom syntactic-close-known-string-inpolation-opener  (list ?{ ?\( ?\[)
  "syntactic-close-known-string-inpolation-opener"
  :type 'list
  :group 'convenience)

(defcustom syntactic-close--paired-opening-delimiter "‘{<[("
"Specify the delimiter char."
:type 'string
:group 'werkstatt)

(defun syntactic-close-reverse-char (char)
  "Return reciproke char as \">\" for \"<\". "
  (let* ((cf char)
	 cn)
    (cond
     ((eq cf ?∧) ;; ?\>
      (setq cn "∨"))
     ((eq cf ?∨) ;; ?\<
      (setq cn "∧"))
     ((eq cf 62) ;; ?\>
      (setq cn "<"))
     ((eq cf 60) ;; ?\<
      (setq cn ">"))
     ((eq cf 187) ;; ?\»
      (setq cn "«"))
     ((eq cf 171) ;; ?\«
      (setq cn "»"))
     ((eq cf 41) ;; ?\)
      (setq cn "("))
     ((eq cf 40) ;; ?\(
      (setq cn ")"))
     ((eq cf 123) ;; ?\{
      (setq cn "}"))
     ((eq cf 125) ;; ?\}
      (setq cn "{"))
     ((eq cf ?`)
      (setq cn "´"))
     ((eq cf ?´)
      (setq cn "`"))
     ((eq cf 93) ;; ?\]
      (setq cn "["))
     ((eq cf 91) ;; ?\[
      (setq cn "]"))
     ((eq cf 92) ;; ?\\
      (setq cn "/"))
     ((eq cf 47) ;; ?\/
      (setq cn "\\")))
    (or cn cf)))


(defun syntactic-close-count-lines (&optional beg end)
  "Count lines in accessible part of buffer.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (interactive)
  (let ((beg (or beg (point-min)))
	(end (or end (point)))
	erg)
    (if (bolp)
	(setq erg (1+ (count-lines beg end)))
      (setq erg (count-lines beg end)))
    (when (called-interactively-p) (message "%s" erg))
    erg))

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
  "List of modes which commands must be closed by a separator. "

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

(defvar syntactic-close-emacs-lisp-block-re
  (concat
   "[ \t]*\\_<"
   "(if\\|(cond\\|when\\|unless"
   "\\_>[ \t]*"))

(defvar syntactic-close-verbose-p nil)

(defvar syntactic-close-assignment-re   "^[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]+[ \t]*=.*$\\|^[[:alpha:]][A-Za-z0-9_]*+[ \t]*=.*")

(setq syntactic-close-assignment-re   "^[[:alpha:]][A-Za-z0-9_]+[ \t]+[[:alpha:]][A-Za-z0-9_]+[ \t]*=.*$\\|^[[:alpha:]][A-Za-z0-9_]*+[ \t]*=.*")

(unless (boundp 'py-block-re)
  (defvar py-block-re "[ \t]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. "))

(defvar syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode)
  "`parse-partial-sexp' must scan only from last prompt. ")
(setq syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode))

(defvar syntactic-close-empty-line-p-chars "^[ \t\r]*$")
(defcustom syntactic-close-empty-line-p-chars "^[ \t\r]*$"
  "syntactic-close-empty-line-p-chars"
  :type 'regexp
  :group 'convenience)

(setq syntactic-close-known-delimiter-chars (list ?< ?` ?* ?\\ ?= ?_ ?$ ?% ?§ ?? ?! ?+ ?- ?# ?: ?\; ?,))

(defun syntactic-close-toggle-verbosity ()
  "If `syntactic-close-verbose-p' is nil, switch it on.

Otherwise switch it off. "
  (interactive)
  (setq syntactic-close-verbose-p (not syntactic-close-verbose-p))
  (when (called-interactively-p 'any) (message "syntactic-close-verbose-p: %s" syntactic-close-verbose-p)))

(defun syntactic-close--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\". "
  (pcase erg
    (?‘ ?’)
    (?` ?')
    (?< ?>)
    (?> ?<)
    (?\( ?\))
    (?\) ?\()
    (?\] ?\[)
    (?\[ ?\])
    (?} ?{)
    (?{ ?})
    (_ erg)))

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
    (when (nth 3 pps)
      ;; paren inside string maybe
      (cond ((and
	      (setq erg (nth 1 (setq last
				     ;; opener inside string?
				     (parse-partial-sexp (1+ (nth 8 pps)) (point)))))
	      (< (nth 8 pps) erg))
	     (setq erg (nth-1-pps-complement-char-maybe last)))
	    ((save-excursion (and (skip-chars-backward (concat "^" syntactic-close--paired-opening-delimiter) (nth 8 pps)) (not (eq (char-after) (nth 3 pps)))   (member (char-before) syntactic-close-known-string-inpolation-opener) (< (nth 8 pps) (1- (point))) (setq last (char-before))
	      (not (eq (char-before) (syntactic-close--return-complement-char-maybe last)))))
	     (setq erg (syntactic-close--return-complement-char-maybe last)))
	    (t
	     (setq erg (syntactic-close-in-string-maybe pps)))))
    erg))

(defun syntactic-close--list-inside-string-maybe (strg)
  (with-temp-buffer
    (insert strg)
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (when (nth 1 pps)
	(save-excursion
	  (goto-char (nth 1 pps))
	  (guess-what--return-complement-char-maybe (char-after)))))))

(defun syntactic-close--fetch-delimiter-maybe (pps)
  "Close the innermost list resp. string. "
  (let (erg closer strg padding)
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
	(when (looking-at "[\[{(][ \t]+")
	  (setq padding (substring (match-string-no-properties 0) 1)))
	(setq closer (syntactic-close--return-complement-char-maybe (char-after)))))
     (t (unless (member (char-before) syntactic-close-known-delimiter-chars)
	  (save-excursion
	    (backward-word)
	    (setq closer (syntactic-close-reverse-char (car-safe (member (char-before) syntactic-close-known-delimiter-chars))))))))
    (and closer (list closer padding))))

(defun syntactic-close-fix-whitespace-maybe (orig &optional padding)
  (save-excursion
    (goto-char orig)
    (when (and (not (looking-back "^[ \t]+" nil))
	       (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	       ;;  not in comment
	       (not (nth 4 (parse-partial-sexp (point-min) (point)))))
      (delete-region (point) orig))
  (when padding (insert padding))))

(defun syntactic-close--insert-delimiter-char-maybe (orig closer padding)
  (let (done)
    (when closer
      (cond
       ((and (eq closer ?}) (not (eq major-mode 'php-mode)))
	(syntactic-close-fix-whitespace-maybe orig padding)
	(insert closer)
	(setq done t))
       ((not (eq closer ?}))
	(syntactic-close-fix-whitespace-maybe orig padding)
	(insert closer)
	(setq done t))))
    done))

(defun syntactic-close-insert-with-padding-maybe (strg &optional nbefore nafter)
  "Takes a string. Insert a space before and after maybe. "
  (skip-chars-backward " \t\r\n\f")
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
		    nafter) (insert " ")))))

(defun syntactic-close--others (orig closer pps padding)
  (let (done erg)
    (cond
     ((nth 3 pps)
      (cond ((characterp (nth 3 pps))
	     (insert (nth 3 pps)))
	    ((setq erg (syntactic-close-in-string-interpolation-maybe pps))
	     (syntactic-close--return-complement-char-maybe erg))
	    (t (syntactic-close--return-complement-char-maybe (nth 8 pps))))
      (setq done t))
     (closer (setq done (syntactic-close--insert-delimiter-char-maybe orig closer padding))))
    done))

(defun syntactic-close--comments-intern (orig start end &optional padding)
  (if (looking-at start)
      (progn (goto-char orig)
	     (fixup-whitespace)
	     (syntactic-close-insert-with-padding-maybe end nil t))
    (goto-char orig)
    (newline-and-indent)))

(defun syntactic-close--insert-comment-end-maybe (pps)
  (let ((orig (point))
	done)
    (cond
     ((eq major-mode 'haskell-mode)
      (goto-char (nth 8 pps))
      (if (looking-at "{-# ")
	  (syntactic-close--comments-intern orig "{-#" "#-}")
	(syntactic-close--comments-intern orig "{-" "-}"))
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
	(setq done t)))
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

(defun syntactic-close--common (orig closer padding)
  (let (done)
    (unless (and (eq closer ?})(member major-mode syntactic-close--semicolon-separator-modes))
      (syntactic-close-fix-whitespace-maybe orig)
      ;; closer might  set
	(when padding (insert padding))
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

(defun syntactic-close-python-listclose (orig closer force pps)
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
	  (t (syntactic-close-fix-whitespace-maybe orig)
	     (insert closer)
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

(defun syntactic-close-python-close (b-of-st b-of-bl padding)
  "Might deliver equivalent to `py-dedent'"
  (interactive "*")
  (let* ((syntactic-close-beginning-of-statement
	  (or b-of-st
	      (if (ignore-errors (functionp 'py-backward-statement))
		  'py-backward-statement
		(lambda ()(beginning-of-line)(back-to-indentation)))))
	 (syntactic-close-beginning-of-block-re (or b-of-bl "[ 	]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n	]*"))
	 done)
    (cond
     ((and (not (char-equal ?: (char-before)))
	   (save-excursion
	     (funcall syntactic-close-beginning-of-statement)
	     (looking-at syntactic-close-beginning-of-block-re)))
      (insert ":")
      (setq done t)))
    done))

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

(defun syntactic-close-ruby-close (&optional closer pps padding)
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

(defun syntactic-close--semicolon-modes (pps &optional closer padding)
  (let ((closer (or closer (syntactic-close--fetch-delimiter-maybe pps)))
	(orig (point))
	done)
    (cond ((and (eq closer ?})(syntactic-close-empty-line-p))
	   (syntactic-close-fix-whitespace-maybe orig padding)
	   (insert closer)
	   (setq done t)
	   (indent-according-to-mode))
	  ((eq closer ?})
	   (cond ((member (char-before) (list ?\; ?}))
		  (if (eq (syntactic-close-count-lines (point-min) (point)) (save-excursion (progn (goto-char (nth 1 pps)) (syntactic-close-count-lines (point-min) (point)))))
		      ;; insert at newline, if opener is at a previous line
		      (progn
			(insert closer)
			(syntactic-close-fix-whitespace-maybe orig padding))
		    (newline)
		    (insert closer))
		  (indent-according-to-mode))
		 (t (insert ";")))
	   (setq done t))
	  ((eq closer ?\))
	   (syntactic-close-fix-whitespace-maybe orig padding)
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
	     (setq done t)))
	  ((save-excursion (beginning-of-line) (looking-at syntactic-close-assignment-re))
	   (insert ";")
	   (setq done t)))
    (unless done (goto-char orig))
    done))

(defun syntactic-close--modes (orig pps closer &optional force padding)
  (let (done)
    (pcase major-mode
      (`python-mode
       (setq done (syntactic-close-python-close nil nil padding)))
      (`emacs-lisp-mode
       (setq done (syntactic-close-emacs-lisp-close closer pps)))
      (`ruby-mode
       (setq done (syntactic-close-ruby-close closer pps padding)))
      (_
       (cond
	((member major-mode syntactic-close--ml-modes)
	(setq done (syntactic-close-ml)))
	((member major-mode (list 'php-mode 'js-mode 'web-mode))
	 (setq done (syntactic-close--semicolon-modes pps closer padding))))
       done))))

(defun syntactic-close-intern (beg iact &optional force pps)
  (let* ((orig (point))
	 (pps (or pps (parse-partial-sexp beg (point))))
	 (verbose syntactic-close-verbose-p)
	 (closer-raw (syntactic-close--fetch-delimiter-maybe pps))
	 (closer (car-safe closer-raw))
	 (padding (car-safe (cdr-safe closer-raw)))
	 done)
    (cond
     ((nth 4 pps)
      (setq done (syntactic-close--insert-comment-end-maybe pps)))
     ((and closer (setq done (when closer (syntactic-close--common orig closer padding)))))
     ((setq done (syntactic-close--modes orig pps closer force padding)))
     ((setq done (syntactic-close--others orig closer pps padding))))
    (or (< orig (point)) (and iact verbose (message "%s" "nil")))
    done))

;;;###autoload
(defun syntactic-close (&optional arg beg force)
  "Command will insert closing delimiter whichever needed.

With \\[universal-argument]: close everything at point. "
  (interactive "p*")
  (let ((beg (or beg (syntactic-close--point-min)))
	(iact arg))
    (pcase (prefix-numeric-value arg)
      (4 (syntactic-close-intern beg iact t))
      (_ (syntactic-close-intern beg iact force)))))

(provide 'syntactic-close)
;;; syntactic-close.el ends here
