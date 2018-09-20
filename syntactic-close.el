;;; syntactic-close.el --- Insert closing delimiter -*- lexical-binding: t; -*-

;; Author: Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>
;; Maintainer: Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Version: 0.1

;; URL: https://github.com/emacs-berlin/syntactic-close

;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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

;;; Commentary:

;; M-x syntactic-close RET: close any syntactic element.

;; ['a','b' ==> ['a','b']

;; A first draft was published at emacs-devel list:
;; http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(eval-when-compile
 (require 'nxml-mode)
 (require 'sgml-mode)
 (require 'comint))

(defgroup syntactic-close nil
  "Insert closing delimiter whichever needed. "
  :group 'languages
  :tag "syntactic-close"
  :prefix "syntactic-close-")

(defvar syntactic-close-empty-line-p-chars "^[ \t\r]*$")
(defcustom syntactic-close-empty-line-p-chars "^[ \t\r]*$"
  "Syntactic-close-empty-line-p-chars."
  :type 'regexp
  :group 'sytactic-close)

(defcustom syntactic-close-unary-delimiter-chars (list ?` ?\" ?' ?+ ?: ?$ ?#)
  "Permitted unary delimiters."
  :type '(repeat character)
  :group 'sytactic-close)
(make-variable-buffer-local 'syntactic-close-unary-delimiter-chars)

(defcustom syntactic-close-empty-line-p-chars "^[ \t\r]*$"
  "Syntactic-close-empty-line-p-chars."
  :type 'regexp
  :group 'sytactic-close)

(defcustom syntactic-close-known-string-inpolation-opener  (list ?{ ?\( ?\[)
  "Syntactic-close-known-string-inpolation-opener."
  :type '(repeat character)
  :group 'sytactic-close)

(defcustom syntactic-close-paired-openers (list ?‘ ?` ?< ?\( ?\[ ?{ ?\〈 ?\⦑ ?\⦓ ?\【 ?\⦗ ?\⸤ ?\「 ?\《 ?\⦕ ?\⸨ ?\⧚ ?\｛ ?\（ ?\［ ?\｟ ?\｢ ?\❰ ?\❮ ?\“ ?\‘ ?\❲ ?\⟨ ?\⟪ ?\⟮ ?\⟦ ?\⟬ ?\❴ ?\❪ ?\❨ ?\❬ ?\᚛ ?\〈 ?\⧼ ?\⟅ ?\⸦ ?\﹛ ?\﹙ ?\﹝ ?\⁅ ?\⦏ ?\⦍ ?\⦋ ?\₍ ?\⁽ ?\༼ ?\༺ ?\⸢ ?\〔 ?\『 ?\⦃ ?\〖 ?\⦅ ?\〚 ?\〘 ?\⧘ ?\⦉ ?\⦇)
  "Specify the delimiter char."
  :type '(repeat character)
  :group 'sytactic-close)

;; (setq syntactic-close-paired-openers
;;   (list ?‘ ?` ?< ?\( ?\[ ?{ ?\〈 ?\⦑ ?\⦓ ?\【 ?\⦗ ?\⸤ ?\「 ?\《 ?\⦕ ?\⸨ ?\⧚ ?\｛ ?\（ ?\［ ?\｟ ?\｢ ?\❰ ?\❮ ?\“ ?\‘ ?\❲ ?\⟨ ?\⟪ ?\⟮ ?\⟦ ?\⟬ ?\❴ ?\❪ ?\❨ ?\❬ ?\᚛ ?\〈 ?\⧼ ?\⟅ ?\⸦ ?\﹛ ?\﹙ ?\﹝ ?\⁅ ?\⦏ ?\⦍ ?\⦋ ?\₍ ?\⁽ ?\༼ ?\༺ ?\⸢ ?\〔 ?\『 ?\⦃ ?\〖 ?\⦅ ?\〚 ?\〘 ?\⧘ ?\⦉ ?\⦇))

(defcustom syntactic-close-paired-closers (list ?’ ?> ?\) ?\] ?} ?\〉 ?\⦒ ?\⦔ ?\】 ?\⦘ ?\⸥ ?\」 ?\》 ?\⦖ ?\⸩ ?\⧛ ?\｝ ?\） ?\］ ?\｠ ?\｣ ?\❱ ?\❯ ?\” ?\’ ?\❳ ?\⟩ ?\⟫ ?\⟯ ?\⟧ ?\⟭ ?\❵ ?\❫ ?\❩ ?\❭ ?\᚜ ?\〉 ?\⧽ ?\⟆ ?\⸧ ?\﹜ ?\﹚ ?\﹞ ?\⁆ ?\⦎ ?\⦐ ?\⦌ ?\₎ ?\⁾ ?\༽ ?\༻ ?\⸣ ?\〕 ?\』 ?\⦄ ?\〗 ?\⦆ ?\〛 ?\〙 ?\⧙ ?\⦊ ?\⦈)
  "Specify the delimiter char."
  :type '(repeat character)
  :group 'sytactic-close)

;; (setq syntactic-close-paired-closers
;;       (list ?’ ?> ?\) ?\] ?} ?\〉 ?\⦒ ?\⦔ ?\】 ?\⦘ ?\⸥ ?\」 ?\》 ?\⦖ ?\⸩ ?\⧛ ?\｝ ?\） ?\］ ?\｠ ?\｣ ?\❱ ?\❯ ?\” ?\’ ?\❳ ?\⟩ ?\⟫ ?\⟯ ?\⟧ ?\⟭ ?\❵ ?\❫ ?\❩ ?\❭ ?\᚜ ?\〉 ?\⧽ ?\⟆ ?\⸧ ?\﹜ ?\﹚ ?\﹞ ?\⁆ ?\⦎ ?\⦐ ?\⦌ ?\₎ ?\⁾ ?\༽ ?\༻ ?\⸣ ?\〕 ?\』 ?\⦄ ?\〗 ?\⦆ ?\〛 ?\〙 ?\⧙ ?\⦊ ?\⦈))

(defcustom syntactic-close--escape-char 92
  "Customize the escape char if needed."
  :type 'char
  :group 'sytactic-close)

(defun syntactic-close--escapes-maybe (limit)
  "Handle escaped parens.

Consider strings like
\"\\(^ *\\|^Passwort: *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th
Argument LIMIT lower border."
  (save-excursion
    (when (eq (char-before) syntactic-close--escape-char)
      (buffer-substring-no-properties (point) (progn (skip-chars-backward (char-to-string syntactic-close--escape-char) limit)(1- (point)))))))

(defun syntactic-close--padding-maybe (&optional pos)
  "Report padding to handle.

Optional argument POS position to start from."
  (save-excursion
    (when pos (goto-char pos))
    (when (member (char-after) (list 32 9))
      (buffer-substring-no-properties (point) (progn (skip-chars-forward " \t")(point))))))

(defun syntactic-close--multichar-closer (char limit &optional offset)
  "Opener and closer might be composed by more than one character.

construct and return the closing string
Argument CHAR the character to contruct the string.
Argument LIMIT the lower border.
Optional argument OFFSET already know offset."
  (if offset
      (make-string (+ offset (abs (skip-chars-backward (char-to-string char) limit))) (syntactic-close--return-complement-char-maybe char))
    (make-string (abs (skip-chars-backward (char-to-string char) limit)) (syntactic-close--return-complement-char-maybe char))))

(defun syntactic-close-pure-syntax-intern (pps)
  "Fetch from start of list to close.

Argument PPS is result of ‘parse-partial-sexp’"
  (let (closer padding)
    (cond
     ((and (nth 3 pps) (nth 1 pps) (< (nth 1 pps) (nth 3 pps)))
      (save-excursion
	(goto-char (nth 1 pps))
	(when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point)))))
	(setq closer (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))
	(when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point)))))))
     ((nth 3 pps)
      (goto-char (nth 8 pps))
      (backward-prefix-chars)
      (setq closer (buffer-substring-no-properties (point) (progn (skip-chars-forward (char-to-string (nth 3 pps) ))(point))))
      (when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point))))))
     ((nth 1 pps)
      (goto-char (nth 1 pps))
      (setq closer (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))
      (when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point)))))))
    (concat padding closer)))

(defun syntactic-close-pure-syntax (pps)
  "Insert closer found from beginning of list.

Argument PPS is result of a call to function ‘parse-partial-sexp’"
  (syntactic-close-pure-syntax-intern pps))

(defun syntactic-close-travel-comment-maybe (pps limit)
  "Leave commented section backward.

Argument PPS is result of a call to function ‘parse-partial-sexp’.
Argument LIMIT the lower bound."
  (let ((pps pps))
    (while (nth 4 pps)
      (goto-char (nth 8 pps))
      (skip-chars-backward " \t\r\n\f")
      (setq pps (parse-partial-sexp limit (point))))
    pps))

(defun syntactic-close-empty-line-p (&optional iact)
  "Return t if cursor is at an empty line, nil otherwise.
Optional argument IACT signaling interactive use."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at syntactic-close-empty-line-p-chars)))
    (looking-at syntactic-close-empty-line-p-chars)))

(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'syntactic-close-empty-line-p))

(defvar haskell-interactive-mode-prompt-start (ignore-errors (require 'haskell-interactive-mode) haskell-interactive-mode-prompt-start)
  "Defined in haskell-interactive-mode.el, silence warnings.")

(defvar syntactic-close-tag nil
  "Functions closing mode-specific might go here.")

(defcustom syntactic-close-electric-delete-whitespace-p nil
  "When t delete whitespace before point when closing.

Default is nil"

  :type 'boolean
  :group 'syntactic-close)

(defcustom syntactic-close-honor-padding-p t
  "Insert whitespace following opener before closer.

Default is t"
  :type 'boolean
  :tag "syntactic-close-honor-padding-p"
  :group 'syntactic-close)
(make-variable-buffer-local 'syntactic-close-honor-padding-p)

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
  "List of modes which commands must be closed by a separator."

  :type 'list
  :tag "syntactic-close--semicolon-separator-modes"
  :group 'syntactic-close)

(defcustom syntactic-close--ml-modes
  (list
   'html-mode
   'mhtml-mode
   'sgml-mode
   'xml-mode
   'xxml-mode
   )
  "List of modes using markup language."
  :type 'list
  :tag "syntactic-close--semicolon-separator-modes"
  :group 'syntactic-close)

(defvar syntactic-close-modes (list 'php-mode 'js-mode 'web-mode 'python-mode 'emacs-lisp-mode 'org-mode 'ruby-mode 'nxml-mode 'html-mode 'mhtml-mode 'sgml-mode 'xml-mode 'xxml-mode)
  "Programming modes dealt with non-generic maybe.")

(defvar syntactic-close-emacs-lisp-block-re
  (concat
   "[ \t]*\\_<"
   "(if\\|(cond\\|when\\|unless"
   "\\_>[ \t]*"))

(defvar syntactic-close-verbose-p nil)

(defvar syntactic-close-assignment-re   ".*[^ =\t]+[ \t]*=[^=]*")

(setq syntactic-close-assignment-re   ".*[^ =\t]+[ \t]*=[^=]*")

(unless (boundp 'py-block-re)
  (defvar py-block-re "[ \t]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. "))

(defvar syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode)
  "`parse-partial-sexp' must scan only from last prompt.")
(setq syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode))

(defun syntactic-close-toggle-verbosity ()
  "If `syntactic-close-verbose-p' is nil, switch it on.

Otherwise switch it off."
  (interactive)
  (setq syntactic-close-verbose-p (not syntactic-close-verbose-p))
  (when (called-interactively-p 'any) (message "syntactic-close-verbose-p: %s" syntactic-close-verbose-p)))

(defun syntactic-close--return-complement-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\".
Argument ERG character to complement."
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
    (?\〈 ?\〉)
    (?\⦑ ?\⦒)
    (?\⦓ ?\⦔)
    (?\【 ?\】)
    (?\⦗ ?\⦘)
    (?\⸤ ?\⸥)
    (?\「 ?\」)
    (?\《 ?\》)
    (?\⦕ ?\⦖)
    (?\⸨ ?\⸩)
    (?\⧚ ?\⧛)
    (?\｛ ?\｝)
    (?\（ ?\）)
    (?\［ ?\］)
    (?\｟ ?\｠)
    (?\｢ ?\｣)
    (?\❰ ?\❱)
    (?\❮ ?\❯)
    (?\“ ?\”)
    (?\‘ ?\’)
    (?\❲ ?\❳)
    (?\⟨ ?\⟩)
    (?\⟪ ?\⟫)
    (?\⟮ ?\⟯)
    (?\⟦ ?\⟧)
    (?\⟬ ?\⟭)
    (?\❴ ?\❵)
    (?\❪ ?\❫)
    (?\❨ ?\❩)
    (?\❬ ?\❭)
    (?\᚛ ?\᚜)
    (?\〈 ?\〉)
    (?\⧼ ?\⧽)
    (?\⟅ ?\⟆)
    (?\⸦ ?\⸧)
    (?\﹛ ?\﹜)
    (?\﹙ ?\﹚)
    (?\﹝ ?\﹞)
    (?\⁅ ?\⁆)
    (?\⦏ ?\⦎)
    (?\⦍ ?\⦐)
    (?\⦋ ?\⦌)
    (?\₍ ?\₎)
    (?\⁽ ?\⁾)
    (?\༼ ?\༽)
    (?\༺ ?\༻)
    (?\⸢ ?\⸣)
    (?\〔 ?\〕)
    (?\『 ?\』)
    (?\⦃ ?\⦄)
    (?\〖 ?\〗)
    (?\⦅ ?\⦆)
    (?\〚 ?\〛)
    (?\〘 ?\〙)
    (?\⧘ ?\⧙)
    (?\⦉ ?\⦊)
    (?\⦇ ?\⦈)
    (_ erg)))

(defun syntactic-close--string-delim-intern (pps)
  "Return the delimiting string.

Argument PPS delivering result of ‘parse-partial-sexp’."
  (goto-char (nth 8 pps))
  (buffer-substring-no-properties (point) (progn  (skip-chars-forward (char-to-string (char-after))) (point))))

(defun syntactic-close-in-string-maybe (&optional pps)
  "If inside a double- triple- or singlequoted string.

Return delimiting chars
Optional argument PPS should deliver the result of ‘parse-partial-sexp’."
  (interactive)
  (save-excursion
    (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
	   (erg (when (nth 3 pps)
		  (syntactic-close--string-delim-intern pps))))
      (unless erg
	(when (looking-at "\"")
	  (forward-char 1)
	  (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	  (when (nth 3 pps)
	    (setq erg (syntactic-close--string-delim-intern pps)))))
      (when (and syntactic-close-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun syntactic-close--nth-1-pps-complement-char-maybe (pps)
  "Return complement character from (nth 1 PPS).

Argument PPS is result of a call to function ‘parse-partial-sexp’"
  (save-excursion
    (goto-char (nth 1 pps))
    (syntactic-close--return-complement-char-maybe (char-after))))

(defun syntactic-close-fix-whitespace-maybe (orig)
  "Remove whitespace before point if called.

Argument ORIG start position.
Optional argument PADDING ."
  (save-excursion
    (goto-char orig)
    (when (and (not (looking-back "^[ \t]+" nil))
	       (< 0 (abs (skip-chars-backward " \t"))))
      (delete-region (point) orig))))

(defun syntactic-close-insert-with-padding-maybe (strg &optional nbefore nafter)
  "Takes a string. Insert a space before and after maybe.
Argument STRG the string to be padded maybe.
Optional argument NBEFORE read not-before string.
Optional argument NAFTER read not after string."
  (let (erg)
  (skip-chars-backward " \t\r\n\f")
      (cond ((looking-back "([ \t]*" (line-beginning-position))
	     (delete-region (match-beginning 0) (match-end 0))
	     (setq erg (concat strg " ")))
	    ((looking-at "[ \t]*)")
	     (delete-region (match-beginning 0) (1- (match-end 0)))
	     (setq erg (concat " " strg)))
	    (t (unless nbefore (setq erg " "))
	       (setq erg (concat erg strg))
	       (unless
		   (or
		    (eq 5 (car (syntax-after (point))))
		    ;; (eq (char-after) ?\))
		    nafter) (setq erg (concat erg " ")))))
	    erg))

(defun syntactic-close--comments-intern (orig start end)
  "Close comments.

ORIG the position where ‘syntactic-close’ was called
START the comment start
END the comment end"
  (if (looking-at start)
      (progn (goto-char orig)
	     (fixup-whitespace)
	     (syntactic-close-insert-with-padding-maybe end nil t))
    (goto-char orig)
    (newline-and-indent)))

(defun syntactic-close--insert-comment-end-maybe (pps)
  "Insert comment end.

Argument PPS should provide result of ‘parse-partial-sexp’."
  (let ((orig (point)))
    (cond
     ((eq major-mode 'haskell-mode)
      (goto-char (nth 8 pps))
      (if (looking-at "{-# ")
	  (syntactic-close--comments-intern orig "{-#" "#-}")
	(syntactic-close--comments-intern orig "{-" "-}"))
      t)
     ((or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
      (goto-char (nth 8 pps))
      (syntactic-close--comments-intern orig "/*" "*/")
      t)
     (t (if (string= "" comment-end)
	    (if (eq system-type 'windows-nt)
		 "\r\n"
	      "\n")
	  comment-end)
	comment-end))
    ))

(defun syntactic-close--point-min ()
  "Determine the lower position in buffer to narrow."
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

(defun syntactic-close-fetch-delimiter (pps)
  "In some cases in (nth 3 PPS only return t."
  (save-excursion
    (goto-char (nth 8 pps))
    (char-after)))

(defun syntactic-close--guess-from-string-interpolation-maybe (pps)
  "Return the character of innermost sexp in inside.

Argument PPS should provide result of ‘parse-partial-sexp’."
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

(defun syntactic-close-ml ()
  "Close in Standard ML."
  (interactive "*")
  (cond ((derived-mode-p 'sgml-mode)
	 (setq syntactic-close-tag 'sgml-close-tag)
	 (funcall syntactic-close-tag)
	 (font-lock-fontify-region (point-min)(point-max))
	 t)))

(defun syntactic-close-python-listclose (orig closer force pps)
  "If inside list, assume another item first.

Argument ORIG the start position.
Argument CLOSER the char which closes the list.
Argument FORCE to be done.
Argument PPS should provide result of ‘parse-partial-sexp’."
  (cond ((member (char-before) (list ?' ?\"))
	 (if force
	     (progn
	       (insert closer)
	       ;; only closing `"' or `'' was inserted here
	       (when (setq closer (syntactic-close--generic))
		 (insert closer))
	       t)
	   (if (nth 3 pps)
	       (insert (char-before))
	     (insert ","))
	   t))
	(t (syntactic-close-fix-whitespace-maybe orig)
	   (insert closer)
	   t)))

;; Emacs-lisp
(defun syntactic-close--org-mode-close ()
  "Org mode specific closes."
  (unless (empty-line-p)
    (end-of-line)
    (newline))
  ;; +BEGIN_QUOTE
  (when (save-excursion (and (re-search-backward "^#\\+\\([A-Z]+\\)_\\([A-Z]+\\)" nil t 1)(string= "BEGIN" (match-string-no-properties 1))))
    (insert (concat "#+END_" (match-string-no-properties 2)))))

(defun syntactic-close-emacs-lisp-close (pps &optional org)
  "Close in Emacs Lisp.

Argument CLOSER the char to close.
Argument PPS should provide result of ‘parse-partial-sexp’.
Optional argument ORG read ‘org-mode’."
  ;; (unary-delimiters-strg (cl-map 'string 'identity unary-delimiter-chars))
  ;; (delimiters (concat syntactic-close-paired-openers-strg syntactic-close-paired-closers-strg unary-delimiters-strg))
  (cond
   (org (syntactic-close--org-mode-close))
   ((and (not (nth 8 pps))(nth 1 pps))
    (syntactic-close-pure-syntax pps))
   ((syntactic-close--generic))))

(defun syntactic-close-python-close (b-of-st b-of-bl &optional pps)
  "Might deliver equivalent to `py-dedent'.

Argument B-OF-ST reaqd beginning-of-statement.
Argument B-OF-BL read beginning-of-block.
Optional argument PADDING to be done.
Optional argument PPS is result of a call to function ‘parse-partial-sexp’"
  (interactive "*")
  (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
	 (syntactic-close-beginning-of-statement
	  (or b-of-st
	      (if (ignore-errors (functionp 'py-backward-statement))
		  'py-backward-statement
		(lambda ()(beginning-of-line)(back-to-indentation)))))
	 (syntactic-close-beginning-of-block-re (or b-of-bl "[ 	]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n	]*")))
    (cond
     ((nth 8 pps)
      (syntactic-close-generic-forms pps))
     ((nth 1 pps)
      (syntactic-close-pure-syntax pps))
     (t (syntactic-close--generic)))))

;; Ruby
(defun syntactic-close--ruby-insert-end ()
  "Ruby specific close workhorse."
  (unless (or (looking-back ";[ \t]*" nil))
    (unless (and (bolp)(eolp))
      (newline))
    (unless (looking-back "^[^ \t]*\\_<end" nil)
       "end")))

(defun syntactic-close-ruby-close (pps)
  "Ruby specific close.

Argument PPS is result of a call to function ‘parse-partial-sexp’"
  (or ;; (syntactic-close--string-before-list-maybe pps)
      (and (or (nth 1 pps) (nth 3 pps)) (syntactic-close-pure-syntax-intern pps))
      (syntactic-close--generic)
      (syntactic-close--ruby-insert-end)))

(defun syntactic-close--semicolon-modes (pps)
  "Close specific modes.

Argument PPS, the result of ‘parse-partial-sexp’."
  (let ((closer
	 (cond ((nth 3 pps)
		(save-excursion (goto-char (nth 8 pps))
				(char-to-string (char-after))))
	       ((save-excursion
		  (and (nth 1 pps) (syntactic-close-pure-syntax-intern pps))))
	       (t (syntactic-close--generic)))))
    (cond
     ;; ((and closer (string-match "]" closer))
     ;;  (if (save-excursion (skip-chars-backward " \t\r\n\f") (member (char-before) (list ?, ?})))
     ;; 	  closer
     ;; 	","))
     ((and closer (string-match "}" closer))
      (if (save-excursion (skip-chars-backward " \t\r\n\f") (member (char-before) (list ?\; ?})))
	  closer
	";"))
     ((ignore-errors (and (< (nth 1 pps) (nth 8 pps))
			  (setq closer (syntactic-close--string-before-list-maybe pps)))))
     ((nth 3 pps)
      closer)
     ((and closer (string-match ")" closer))
      closer)
     ((save-excursion (beginning-of-line) (looking-at syntactic-close-assignment-re))
      (setq closer ";"))
     (t closer))))

(defun syntactic-close--return-closer (pps)
  "Argument PPS, the result of ‘parse-partial-sexp’."
  (pcase major-mode
    (`php-mode (syntactic-close--semicolon-modes pps))
    (`js-mode (syntactic-close--semicolon-modes pps))
    (`web-mode (syntactic-close--semicolon-modes pps))
    (_ 	(if
	    (ignore-errors (< (nth 1 pps) (nth 8 pps)))
	    (syntactic-close--string-before-list-maybe pps)
	  (syntactic-close--common-modes pps)))))

(defun syntactic-close--finish-element ()
  "Finish the current element by inserting an end-tag."
  (interactive "*")
  (let ((orig (point)))
    (nxml-finish-element-1 nil)
    (< orig (point))))

(defun syntactic-close--common-modes (pps)
  "No special treatment required.
Argument PPS result of ‘parse-partial-sexp’."
  (pcase major-mode
    (`python-mode
     (syntactic-close-python-close nil nil pps))
    (`emacs-lisp-mode
     (syntactic-close-emacs-lisp-close pps))
    (`org-mode
     (syntactic-close-emacs-lisp-close pps t))
    (`ruby-mode
     (syntactic-close-ruby-close pps))
    (`nxml-mode
     (syntactic-close--finish-element))
    (`html-mode
     (syntactic-close-ml))
    (`mhtml-mode
     (syntactic-close-ml))
    (`sgml-mode
     (syntactic-close-ml))
    (`xml-mode
     (syntactic-close-ml))
    (`xxml-mode
     (syntactic-close-ml))))

(defun syntactic-close-generic-forms (pps)
  "Argument PPS, the result of ‘parse-partial-sexp’."
  (cond
   ((syntactic-close--generic))
   ((nth 4 pps)
    ;; in comment
    (syntactic-close--insert-comment-end-maybe pps))))

(defun syntactic-close--generic (&optional orig stack limit)
  "Detect delimiters inside string or comment maybe.

Optional argument UNARY-DELIMITER-CHARS like quoting chara1cter,
a list.
Optional argument DELIMITERS-STRG composed of unary and paired delimiters,
a list.
Optional argument ORIG position.
Optional argument STACK used by recursive call maybe.
Optional argument LIMIT bound."
  (let* ((orig (or orig (point)))
	 (limit (or limit (point-min)))
	 (unary-delimiter-chars syntactic-close-unary-delimiter-chars)
	 (paired-delimiters-strg (concat (cl-map 'string 'identity syntactic-close-paired-openers)(cl-map 'string 'identity syntactic-close-paired-closers)))
	 (stack stack)
	 closer done escapes padding)
    (while (and (not (bobp)) (not done) (<= limit (1- (point))))
      (cond ((member (char-before)
		     ;; (list ?\) ?\] ?})
		     syntactic-close-paired-closers)
	     (push (char-before) stack)
	     (unless (bobp) (forward-char -1)))
	    (;; (list ?\( ?\" ?{ ?\[)
	     (member (char-before)
		     syntactic-close-paired-openers)
	     (if (eq (car stack) (syntactic-close--return-complement-char-maybe (char-before)))
		 (progn
		   (pop stack)
		   (unless (bobp)
		     (forward-char -1)))
	       (setq done t)
	       (when syntactic-close-honor-padding-p (save-excursion (setq padding (syntactic-close--padding-maybe))))
	       ;; maybe more then one char
	       (setq closer (syntactic-close--multichar-closer (char-before) limit))
	       (unless (bobp)
		 (setq escapes (syntactic-close--escapes-maybe limit)))))
	    ((and (member (char-before)
			  ;; (list ?` ?\" ?')
			  unary-delimiter-chars)
		  ;; $asdf$
		  (eq 0 (mod (count-matches (char-to-string (char-before)) limit
					    ;; (point)
					    orig) 2))
		  ;; (setq erg (char-before))
		  (skip-chars-backward (concat "^" (cl-map 'string 'identity (remove (char-before) unary-delimiter-chars)) paired-delimiters-strg) limit)))
	    ((and (member (char-before)
			  ;; (list ?` ?\" ?')
			  unary-delimiter-chars)
		  ;; $asdf$
		  (not (eq 0 (mod (count-matches (char-to-string (char-before)) limit
						 ;; (point)
						 orig) 2))))
	     (setq closer (buffer-substring (save-excursion (skip-chars-backward (char-to-string (char-before)))(point)) (point)))
	     (when syntactic-close-honor-padding-p (save-excursion (setq padding (syntactic-close--padding-maybe))))
	     (setq done t)
	     (setq escapes (syntactic-close--escapes-maybe limit)))
	    ((or (< 0 (abs (skip-chars-backward (concat "^" (cl-map 'string 'identity unary-delimiter-chars) paired-delimiters-strg) limit)))
		 (setq done t)))))
    (cond (closer
	   (when (characterp closer) (setq closer (char-to-string closer)))
	   (setq closer (concat padding escapes closer))))
    (goto-char orig)
    closer))

(defun syntactic-close--string-before-list-maybe (pps)
  "String inside a list needs to be closed first maybe.

Expects start of string behind start of list
Argument PPS result of ‘parse-partial-sexp’."
  ;; maybe close generic first
  (cond ((syntactic-close--generic (point) nil (nth 8 pps)))
	((syntactic-close-pure-syntax-intern pps))))

(defun syntactic-close-intern (beg iact &optional pps)
  "A first dispatch.

Argument PPS, the result of ‘parse-partial-sexp’.
Argument BEG the lesser border.
Argument IACT signals an interactive call."
  (let* ((orig (copy-marker (point)))
	 (pps (or pps (parse-partial-sexp beg (point))))
	 closer)
    (when syntactic-close-electric-delete-whitespace-p
      (delete-region orig (progn (skip-chars-backward " \t" (line-beginning-position))(point))))
    ;; (save-excursion
    (cond
     ((member major-mode syntactic-close-modes)
      (setq closer (syntactic-close--return-closer pps)))
     ((setq closer (syntactic-close--generic))))
    ;; insert might be hardcoded like in ‘nxml-finish-element-1’
    (when (and closer (stringp closer))
      (goto-char orig)
      (insert closer))
    (or (< orig (point)) (and iact (message "%s" "nil") nil))))

;;;###$autoload
(defun syntactic-close (&optional arg beg pps iact)
  "Insert closing delimiter.

With \\[universal-argument]: close everything at point.
For example
\"\\(^ *\\|^Some: *\\|\\( FOO\\|'s\\|Bar\\|BAZ\\|Outer\\(?: \\(?:\\(?:sim\\|zh

should end up as
\"\\(^ *\\|^Some: *\\|\\( FOO\\|'s\\|Bar\\|BAZ\\|Outer\\(?: \\(?:\\(?:sim\\|zh\\)\\)\\)\"

Optional argument ARG signals interactive use.
Optional argument BEG sets the lesser border.
Argument PPS, the result of ‘parse-partial-sexp’."
  (interactive "p*")
  (let ((orig (point))
	(beg (or beg (syntactic-close--point-min)))
	(iact (or iact arg))
	(arg (or arg 1)))
    (pcase (prefix-numeric-value arg)
      (4 (while (syntactic-close-intern beg iact (parse-partial-sexp beg (point))))
	 (< orig (point)))
      (_
       (dotimes (_ arg)
	 (syntactic-close-intern beg iact pps))
       (< orig (point))))))

(provide 'syntactic-close)
;;; syntactic-close.el ends here
