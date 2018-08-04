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

;; (setq syntactic-close-unary-delimiter-chars (list ?' ?` ?* ?\\ ?= ?$ ?% ?§ ?? ?! ?+ ?- ?# ?: ?\; ?,))
(setq syntactic-close-unary-delimiter-chars (list ?` ?\" ?'))

(setq syntactic-close-unary-delimiters-strg (cl-map 'string 'identity syntactic-close-unary-delimiter-chars))

(defcustom syntactic-close-empty-line-p-chars "^[ \t\r]*$"
  "Syntactic-close-empty-line-p-chars."
  :type 'regexp
  :group 'sytactic-close)

(defcustom syntactic-close-known-string-inpolation-opener  (list ?{ ?\( ?\[)
  "Syntactic-close-known-string-inpolation-opener."
  :type 'list
  :group 'sytactic-close)

(defcustom syntactic-close-paired-openers "‘{<[("
  "Specify the delimiter char."
  :type 'string
  :group 'sytactic-close)

(setq syntactic-close-paired-openers-strg
  (cl-map 'string 'identity (list ?‘ ?` ?< ?\( ?\[ ?{ ?\〈 ?\⦑ ?\⦓ ?\【 ?\⦗ ?\⸤ ?\「 ?\《 ?\⦕ ?\⸨ ?\⧚ ?\｛ ?\（ ?\［ ?\｟ ?\｢ ?\❰ ?\❮ ?\“ ?\‘ ?\❲ ?\⟨ ?\⟪ ?\⟮ ?\⟦ ?\⟬ ?\❴ ?\❪ ?\❨ ?\❬ ?\᚛ ?\〈 ?\⧼ ?\⟅ ?\⸦ ?\﹛ ?\﹙ ?\﹝ ?\⁅ ?\⦏ ?\⦍ ?\⦋ ?\₍ ?\⁽ ?\༼ ?\༺ ?\⸢ ?\〔 ?\『 ?\⦃ ?\〖 ?\⦅ ?\〚 ?\〘 ?\⧘ ?\⦉ ?\⦇)))

(setq syntactic-close-paired-openers
  (list ?‘ ?` ?< ?\( ?\[ ?{ ?\〈 ?\⦑ ?\⦓ ?\【 ?\⦗ ?\⸤ ?\「 ?\《 ?\⦕ ?\⸨ ?\⧚ ?\｛ ?\（ ?\［ ?\｟ ?\｢ ?\❰ ?\❮ ?\“ ?\‘ ?\❲ ?\⟨ ?\⟪ ?\⟮ ?\⟦ ?\⟬ ?\❴ ?\❪ ?\❨ ?\❬ ?\᚛ ?\〈 ?\⧼ ?\⟅ ?\⸦ ?\﹛ ?\﹙ ?\﹝ ?\⁅ ?\⦏ ?\⦍ ?\⦋ ?\₍ ?\⁽ ?\༼ ?\༺ ?\⸢ ?\〔 ?\『 ?\⦃ ?\〖 ?\⦅ ?\〚 ?\〘 ?\⧘ ?\⦉ ?\⦇))

(defcustom syntactic-close-paired-closers "’}>])"
  "Specify the delimiter char."
  :type 'string
  :group 'sytactic-close)

(setq syntactic-close-paired-closers-strg
      (cl-map 'string 'identity (list ?’ ?> ?\) ?\] ?} ?\〉 ?\⦒ ?\⦔ ?\】 ?\⦘ ?\⸥ ?\」 ?\》 ?\⦖ ?\⸩ ?\⧛ ?\｝ ?\） ?\］ ?\｠ ?\｣ ?\❱ ?\❯ ?\” ?\’ ?\❳ ?\⟩ ?\⟫ ?\⟯ ?\⟧ ?\⟭ ?\❵ ?\❫ ?\❩ ?\❭ ?\᚜ ?\〉 ?\⧽ ?\⟆ ?\⸧ ?\﹜ ?\﹚ ?\﹞ ?\⁆ ?\⦎ ?\⦐ ?\⦌ ?\₎ ?\⁾ ?\༽ ?\༻ ?\⸣ ?\〕 ?\』 ?\⦄ ?\〗 ?\⦆ ?\〛 ?\〙 ?\⧙ ?\⦊ ?\⦈)))

(setq syntactic-close-paired-closers
      (list ?’ ?> ?\) ?\] ?} ?\〉 ?\⦒ ?\⦔ ?\】 ?\⦘ ?\⸥ ?\」 ?\》 ?\⦖ ?\⸩ ?\⧛ ?\｝ ?\） ?\］ ?\｠ ?\｣ ?\❱ ?\❯ ?\” ?\’ ?\❳ ?\⟩ ?\⟫ ?\⟯ ?\⟧ ?\⟭ ?\❵ ?\❫ ?\❩ ?\❭ ?\᚜ ?\〉 ?\⧽ ?\⟆ ?\⸧ ?\﹜ ?\﹚ ?\﹞ ?\⁆ ?\⦎ ?\⦐ ?\⦌ ?\₎ ?\⁾ ?\༽ ?\༻ ?\⸣ ?\〕 ?\』 ?\⦄ ?\〗 ?\⦆ ?\〛 ?\〙 ?\⧙ ?\⦊ ?\⦈))

(defvar syntactic-close-delimiters (concat syntactic-close-paired-openers syntactic-close-paired-closers syntactic-close-unary-delimiters-strg)
  "All known delimiting chars.

Concatenates ‘syntactic-close-paired-openers’, ‘syntactic-close-paired-closers’ and  ‘syntactic-close-unary-delimiters-strg’")

;; make sure changes are reloaded
(setq syntactic-close-delimiters (concat syntactic-close-paired-openers syntactic-close-paired-closers syntactic-close-unary-delimiters-strg))

(defcustom syntactic-close--escape-char 92
  "Customize the escape char if needed."
  :type 'char
  :group 'sytactic-close)

(defun syntactic-close--escapes-maybe (limit)
  "Handle escaped parens, consider strings like

\"\\(^ *\\|^Passwort: *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th"
  (save-excursion
    (when (eq (char-before) syntactic-close--escape-char)
      (buffer-substring-no-properties (point) (progn (skip-chars-backward (char-to-string syntactic-close--escape-char) limit)(1- (point)))))))

(defun syntactic-close--padding-maybe (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (when (member (char-after) (list 32 9))
      (buffer-substring-no-properties (point) (progn (skip-chars-forward " \t")(point))))))

(defun syntactic-close--multichar-closer (char limit &optional offset)
  (if offset
      (make-string (+ offset (abs (skip-chars-backward (char-to-string char) limit))) (syntactic-close--return-complement-char-maybe char))
    (make-string (abs (skip-chars-backward (char-to-string char) limit)) (syntactic-close--return-complement-char-maybe char))))

(defun syntactic-close-pure-syntax-intern (pps)
  (let (closer padding)
    (save-excursion
      (goto-char (nth 1 pps))
      (when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point)))))
      (setq closer (syntactic-close--multichar-closer (char-after) (line-beginning-position) 1)))
    (concat  padding closer)))

(defun syntactic-close-pure-syntax (pps)
  (insert (syntactic-close-pure-syntax-intern pps)))

(defun syntactic-close--generic (pps)
  (let ((limit (or (nth 8 pps)(point-min)))
	(pps pps)
	closer stack done escapes padding)
    (save-restriction
      (save-excursion
	(while (and (not (bobp)) (not done))
	  (cond ((member (char-before)
			 ;; (list ?\) ?\] ?})
			 syntactic-close-paired-closers)
		 (push (char-before) stack)
		 (unless (bobp) (forward-char -1)))
		((member (char-before)
			 ;; (list ?\( ?\" ?{ ?\[)
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
			      syntactic-close-unary-delimiter-chars)
		      ;; maybe behind string or comma
		      (not (save-excursion (nth 8 (parse-partial-sexp (point-min) (1- (point)))))))
		 (setq closer (char-before))
		 (when syntactic-close-honor-padding-p (save-excursion (setq padding (syntactic-close--padding-maybe))))
		 (setq done t)
		 (setq escapes (syntactic-close--escapes-maybe limit)))
		(t (unless (< 0 (abs (skip-chars-backward (concat "^" syntactic-close-delimiters) limit)))
		     (setq done t)
		     (setq closer nil escapes nil padding nil))))))
      (cond (closer
	     (when (characterp closer) (setq closer (char-to-string closer)))
	     (concat padding escapes closer))
	    ;; the fence character closes
	    (t (ignore-errors (nth 3 pps)))))))

(defun syntactic-close-count-lines (&optional beg end)
  "Count lines in accessible part of buffer.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115
Optional argument BEG counts start.
Optional argument END counts end."
  (interactive)
  (let ((beg (or beg (point-min)))
	(end (or end (point)))
	erg)
    (if (bolp)
	(setq erg (1+ (count-lines beg end)))
      (setq erg (count-lines beg end)))
    (when (interactive-p) (message "%s" erg))
    erg))

(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'syntactic-close-empty-line-p))
(defun syntactic-close-empty-line-p (&optional iact)
  "Return t if cursor is at an empty line, nil otherwise.
Optional argument IACT signaling interactive use."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at syntactic-close-empty-line-p-chars)))
    (looking-at syntactic-close-empty-line-p-chars)))

(defvar haskell-interactive-mode-prompt-start (ignore-errors (require 'haskell-interactive-mode) haskell-interactive-mode-prompt-start)
  "Defined in haskell-interactive-mode.el, silence warnings.")

(defvar syntactic-close-tag nil
  "Functions closing mode-specific might go here.")

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
  "Programming modes dealt with non-generic maybe. ")

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
  "Return complement character from (nth 1 PPS)."
  (save-excursion
    (goto-char (nth 1 pps))
    (syntactic-close--return-complement-char-maybe (char-after))));

(defun syntactic-close--list-inside-string-maybe (strg)
  (with-temp-buffer
    (insert strg)
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (when (nth 1 pps)
	(save-excursion
	  (goto-char (nth 1 pps))
	  (syntactic-close--return-complement-char-maybe (char-after)))))))

(defun syntactic-close-fix-whitespace-maybe (orig &optional padding)
  (save-excursion
    (goto-char orig)
    (when (and (not (looking-back "^[ \t]+" nil))
	       (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	       ;;  not in comment
	       (not (nth 4 (parse-partial-sexp (point-min) (point)))))
      (delete-region (point) orig)))
  (when padding (insert padding)))

(defun syntactic-close--insert-delimiter-char-maybe (orig closer)
  (let (done)
    (when closer
      (cond
       ((and (eq closer ?}) (not (eq major-mode 'php-mode)))
	(syntactic-close-fix-whitespace-maybe orig)
	(insert closer)
	(setq done t))
       ((not (eq closer ?}))
	(syntactic-close-fix-whitespace-maybe orig)
	(insert closer)
	(setq done t))))
    done))

(defun syntactic-close-insert-with-padding-maybe (strg &optional nbefore nafter)
  "Takes a string. Insert a space before and after maybe.
Argument STRG the string to be padded maybe.
Optional argument NBEFORE read not-before string.
Optional argument NAFTER read not after string."
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

;; (defun syntactic-close--others (orig closer pps padding)
;;   (let (done)
;;     (cond
;;      ((nth 3 pps)
;;       (cond ((characterp (nth 3 pps))
;; 	     (insert (nth 3 pps)))
;; 	    ;; restrict to syntax
;; 	    ;; ((setq erg (syntactic-close-in-string-interpolation-maybe pps))
;; 	    ;;  (syntactic-close--return-complement-char-maybe erg))
;; 	    (t (syntactic-close--return-complement-char-maybe (nth 8 pps))))
;;       (setq done t))
;;      (closer (setq done (syntactic-close--insert-delimiter-char-maybe orig closer))))
;;     done))

(defun syntactic-close--comments-intern (orig start end)
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

;; (defun syntactic-close--common (orig padding pps)
;;   (let (done)
;;     (unless (and (eq closer ?})(member major-mode syntactic-close--semicolon-separator-modes))
;;       (unless (nth 3 pps)
;; 	(syntactic-close-fix-whitespace-maybe orig)
;; 	;; closer might set
;; 	(when padding (insert)))
;;       (insert closer)
;;       (save-excursion (indent-according-to-mode))
;;       (setq done t))
;;     done))

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
  "Close in Standard ML."
  (interactive "*")
  (let (done)
    ;; (when
    (cond ((derived-mode-p 'sgml-mode)
	   (setq syntactic-close-tag 'sgml-close-tag)
	   (funcall syntactic-close-tag)
	   (font-lock-fontify-region (point-min)(point-max))
	   (setq done t)))
    done))

(defun syntactic-close-python-listclose (orig closer force pps)
  "If inside list, assume another item first.
Argument ORIG the start position.
Argument CLOSER the char which closes the list.
Argument FORCE to be done.
Argument PPS should provide result of ‘parse-partial-sexp’."
  (let (done)
    (cond ((member (char-before) (list ?' ?\"))
	   (if force
	       (progn
		 (insert closer)
		 ;; only closing `"' or `'' was inserted here
		 (when (setq closer (syntactic-close--generic (parse-partial-sexp (point-min) (point))))
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
(defun syntactic-close--org-mode-close ()
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
  (cond ((and (nth 1 pps) (not (nth 3 pps)))
	 (syntactic-close-pure-syntax pps)
	 t)
	(t (let ((closer (syntactic-close--generic pps))
		 done)
	     (if closer
		 (progn
		   (insert closer)
		   (setq done t))
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
		 (setq done t))
		(org (setq done (syntactic-close--org-mode-close)))))
	     done))))

(defun syntactic-close-python-close (b-of-st b-of-bl &optional pps)
  "Might deliver equivalent to `py-dedent'.
Argument B-OF-ST reaqd beginning-of-statement.
Argument B-OF-BL read beginning-of-block.
Optional argument PADDING to be done."
  (interactive "*")
  (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
	 (syntactic-close-beginning-of-statement
	  (or b-of-st
	      (if (ignore-errors (functionp 'py-backward-statement))
		  'py-backward-statement
		(lambda ()(beginning-of-line)(back-to-indentation)))))
	 (syntactic-close-beginning-of-block-re (or b-of-bl "[ 	]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n	]*")))
    (cond
     ((and (not (bolp)) (not (char-equal ?: (char-before)))
	   (save-excursion
	     (funcall syntactic-close-beginning-of-statement)
	     (looking-at syntactic-close-beginning-of-block-re)))
      (insert ":"))
     ((nth 8 pps)
      (syntactic-close-generic-forms pps))
     ((nth 1 pps)
      (syntactic-close-pure-syntax pps))
     (t (syntactic-close--generic pps)))))

;; Ruby
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

(defun syntactic-close-ruby-close (pps)
  (let ((closer (syntactic-close--guess-delimiter pps))
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

(defun syntactic-close--guess-delimiter (pps)
  (cond ((nth 3 pps) (syntactic-close--generic pps))
	((nth 1 pps) (syntactic-close-pure-syntax-intern pps))
	(t (syntactic-close--generic pps))))

(defun syntactic-close--semicolon-modes (pps)
  "PPS, the result of ‘parse-partial-sexp’.

CLOSER, a string"
  (let ((closer
	 (syntactic-close--guess-delimiter pps))
	(orig (point))
	done)
    (cond ((nth 3 pps)
	   (insert closer) (setq done t))
	  ((and closer (string-match "}" closer)(syntactic-close-empty-line-p))
	   (syntactic-close-fix-whitespace-maybe orig)
	   (insert closer)
	   (setq done t)
	   (indent-according-to-mode))
	  ((and closer (string-match "}" closer))
	   (cond ((member (char-before) (list ?\; ?}))
		  (if (eq (syntactic-close-count-lines (point-min) (point)) (save-excursion (progn (goto-char (nth 1 pps)) (syntactic-close-count-lines (point-min) (point)))))
		      ;; insert at newline, if opener is at a previous line
		      (progn
			(syntactic-close-fix-whitespace-maybe orig)
			(insert closer))
		    (newline)
		    (insert closer))
		  (indent-according-to-mode))
		 (t (insert ";")))
	   (setq done t))
	  ((and closer (string-match ")" closer))
	   (insert closer)
	   (setq done t))
	  ;; closing asignement
	  ;; ((eq (char-before) ?\))
	  ;;  (backward-list)
	  ;;  (skip-chars-backward "^ \t\r\n\f")
	  ;;  (skip-chars-backward " \t")
	  ;;  (when (eq (char-before) ?=)
	  ;;    (goto-char orig)
	  ;;    (insert ";")
	  ;;    (setq done t)))
	  ((save-excursion (beginning-of-line) (looking-at syntactic-close-assignment-re))
	   (insert ";")
	   (setq done t))
	  (t (when closer
	       (insert closer)
	       (setq done t))))
    (unless done (goto-char orig))
    done))

(defun syntactic-close--modes (pps)
  (pcase major-mode
    (`php-mode (syntactic-close--semicolon-modes pps))
    (`js-mode (syntactic-close--semicolon-modes pps))
    (`web-mode (syntactic-close--semicolon-modes pps))
    (`python-mode
     (syntactic-close-python-close nil nil pps))
    (`emacs-lisp-mode
     (syntactic-close-emacs-lisp-close pps))
    (`org-mode
     (syntactic-close-emacs-lisp-close pps t))
    (`ruby-mode
     (syntactic-close-ruby-close pps))
    (`nxml-mode
     (nxml-finish-element))
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
  (cond
   ((syntactic-close--generic pps))
   ((nth 4 pps)
    ;; in comment
    (syntactic-close--insert-comment-end-maybe pps))))

(defun syntactic-close-intern (beg iact &optional pps)
  (let* ((orig (point))
	 (pps (or pps (parse-partial-sexp beg (point)))))
    (cond
     ((member major-mode syntactic-close-modes)
      (syntactic-close--modes pps))
     ((nth 8 pps)
      (syntactic-close-generic-forms pps))
     ((nth 1 pps)
      (syntactic-close-pure-syntax pps))
     (t (syntactic-close--generic pps)))
    (or (< orig (point)) (and iact (message "%s" "nil") nil))))

;;;###autoload
(defun syntactic-close (&optional arg beg pps)
  "Command will insert closing delimiter whichever needed.

With \\[universal-argument]: close everything at point. 

For example
\"\\(^ *\\|^Some: *\\|\\( FOO\\|'s\\|Bar\\|BAZ\\|Outer\\(?: \\(?:\\(?:sim\\|zh

should end up as
\"\\(^ *\\|^Some: *\\|\\( FOO\\|'s\\|Bar\\|BAZ\\|Outer\\(?: \\(?:\\(?:sim\\|zh\\)\\)\\)\"

Optional argument ARG signals interactive use.
Optional argument BEG sets the lesser border."
  (interactive "p*")
  (let ((beg (or beg (syntactic-close--point-min)))
	(iact arg)
	(arg (or arg 1))
	erg)
    (pcase (prefix-numeric-value arg)
      (4 (while (syntactic-close-intern beg iact pps)))
      (_
       (dotimes (_ arg)
	 (setq erg (syntactic-close-intern beg iact pps))
	 )))
    erg))

(provide 'syntactic-close)
;;; syntactic-close.el ends here
