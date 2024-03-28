;;; syntactic-close.el --- Insert closing delimiter -*- lexical-binding: t; -*-

;; Author: Andreas Röhler <andreas.roehler@online.de>
;;     Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; A first draft was published at emacs-devel list:
;; http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

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

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
;; (require 'ar-thingatpt-utils)
;; (require 'ar-sexp)

(eval-when-compile
 (require 'nxml-mode)
 (require 'sgml-mode)
 (require 'comint))

(defvar  syntactic-close-install-directory default-directory
  "Detect current .cask stuff.")

(defvar syntactic-close-debug-p nil
  "Avoid error")
;; (setq syntactic-close-debug-p t)

(and syntactic-close-debug-p (message "syntactic-close.el: default-directory: %s" default-directory))

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

(defvar syntactic-close-unary-delimiter-chars (list ?` ?\" ?' ?+ ?: ?$ ?#)
  "Permitted unary delimiters." )
(make-variable-buffer-local 'syntactic-close-unary-delimiter-chars)

(defcustom syntactic-close-empty-line-p-chars "^[ \t\r]*$"
  "Syntactic-close-empty-line-p-chars."
  :type 'regexp
  :group 'sytactic-close)

(defcustom syntactic-close-known-string-inpolation-opener  (list ?{ ?\( ?\[)
  "Syntactic-close-known-string-inpolation-opener."
  :type '(repeat character)
  :group 'sytactic-close)

(defcustom syntactic-close-paired-openers (list ?< ?\( ?\[ ?{ ?\〈 ?\⦑ ?\⦓ ?\【 ?\⦗ ?\⸤ ?\「 ?\《 ?\⦕ ?\⸨ ?\⧚ ?\｛ ?\（ ?\［ ?\｟ ?\｢ ?\❰ ?\❮ ?\“ ?\‘ ?\❲ ?\⟨ ?\⟪ ?\⟮ ?\⟦ ?\⟬ ?\❴ ?\❪ ?\❨ ?\❬ ?\᚛ ?\〈 ?\⧼ ?\⟅ ?\⸦ ?\﹛ ?\﹙ ?\﹝ ?\⁅ ?\⦏ ?\⦍ ?\⦋ ?\₍ ?\⁽ ?\༼ ?\༺ ?\⸢ ?\〔 ?\『 ?\⦃ ?\〖 ?\⦅ ?\〚 ?\〘 ?\⧘ ?\⦉ ?\⦇)
  "Specify the delimiter char."
  :type '(repeat character)
  :group 'sytactic-close)

;; (setq syntactic-close-paired-openers

;;    (list ?‘ ?< ?\( ?\[ ?{ ?\〈 ?\⦑ ?\⦓ ?\【 ?\⦗ ?\⸤ ?\「 ?\《 ?\⦕ ?\⸨ ?\⧚ ?\｛ ?\（ ?\［ ?\｟ ?\｢ ?\❰ ?\❮ ?\“ ?\‘ ?\❲ ?\⟨ ?\⟪ ?\⟮ ?\⟦ ?\⟬ ?\❴ ?\❪ ?\❨ ?\❬ ?\᚛ ?\〈 ?\⧼ ?\⟅ ?\⸦ ?\﹛ ?\﹙ ?\﹝ ?\⁅ ?\⦏ ?\⦍ ?\⦋ ?\₍ ?\⁽ ?\༼ ?\༺ ?\⸢ ?\〔 ?\『 ?\⦃ ?\〖 ?\⦅ ?\〚 ?\〘 ?\⧘ ?\⦉ ?\⦇))

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

(defun syntactic-close--escaped-p (&optional pos)
  "Return t if char at POS is preceded by an odd number of backslashes. "
  (save-excursion
    (when pos (goto-char pos))
    (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defun syntactic-close--escaped-in-string-p ()
  "Return t if char at POS is escaped. "
  (let ((orig (point)))
  (save-excursion
    (and (eq 0 (% (abs (skip-chars-backward "\\\\")) 2))(< (point) orig)))))

(defun syntactic-close--escapes-maybe (limit)
  "Handle escaped parens.

Consider strings like
Argument LIMIT lower border."
  (save-excursion
    (when (eq (char-before) syntactic-close--escape-char)
      (buffer-substring-no-properties (point) (progn (skip-chars-backward (char-to-string syntactic-close--escape-char) limit)(1- (point)))))))

(defun syntactic-close--string-before-list-maybe (pps)
  "String inside a list needs to be closed first maybe.

Expects start of string behind start of list
Argument PPS result of ‘parse-partial-sexp’."
  ;; maybe close generic first
  (syntactic-close-pure-syntax pps))

(defun syntactic-close--padding-maybe (&optional pos)
  "Report padding to handle.

Optional argument POS position to start from."
  (save-excursion
    (when pos (goto-char pos))
    (when (member (char-after) (list 32 9))
      (buffer-substring-no-properties (point) (progn (skip-chars-forward " \t")(point))))))

(defun syntactic-close--multichar-intern (char limit)
    (abs (skip-chars-backward (char-to-string char) limit)))

(defun syntactic-close--multichar-closer (char limit &optional offset)
  "Opener and closer might be composed by more than one character.

construct and return the closing string
Argument CHAR the character to contruct the string.
Argument LIMIT the lower border.
Optional argument OFFSET already know offset."
  (if offset
      (make-string (+ offset (syntactic-close--multichar-intern char limit)) (syntactic-close--return-complement-char-maybe char))
    (make-string (syntactic-close--multichar-intern char limit) (syntactic-close--return-complement-char-maybe char))))

(defun syntactic-close-pure-syntax  (pps)
  "Fetch from start to close.

Argument PPS is result of ‘parse-partial-sexp’"
  (save-excursion
    (let (closer padding)
      (cond
       ; syntactic-close-intern
       ;; ((nth 4 pps)
       ;;  (goto-char (nth 8 pps)))
       ((and (nth 3 pps) (nth 1 pps) (< (nth 1 pps) (nth 8 pps)))
	(goto-char (nth 8 pps))
	(when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point)))))
	(setq closer (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))
	(when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point))))))
       ((nth 3 pps)
	(goto-char (nth 8 pps))
	(backward-prefix-chars)
	(setq closer (buffer-substring-no-properties (point) (progn (skip-chars-forward (char-to-string (char-after)))(point))))
	(when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point))))))
       ((nth 1 pps)
	(goto-char (nth 1 pps))
	(setq closer (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))
	(when syntactic-close-honor-padding-p (setq padding (syntactic-close--padding-maybe (1+ (point)))))))
      (concat padding closer))))

;; (defun syntactic-close-pure-syntax (orig pps)
;;   "Insert closer found from beginning of list.

;; Argument PPS is result of a call to function ‘parse-partial-sexp’"
;;   (syntactic-close-pure-syntax pps))

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

(defvar syntactic-close--semicolon-separator-modes
  (list
   'inferior-sml-mode
   'js-mode
   'js2-mode
   'perl-mode
   'php-mode
   'sml-mode
   'web-mode
   )
  "syntactic-close--semicolon-separator-modes")

(defvar syntactic-close--ml-modes
  (list
   'html-mode
   'mhtml-mode
   'sgml-mode
   'xml-mode
   'xxml-mode
   )
  "syntactic-close--ml-modes")

(defvar syntactic-close-modes (list
			       'agda2-mode
			       ;; 'emacs-lisp-mode
			       'html-mode
                               'haskell-mode
			       'java-mode
			       'js-mode
			       'mhtml-mode
			       'nxml-mode
			       'org-mode
			       'php-mode
			       'python-mode
			       'ruby-mode
                               'prolog-mode
                               'scala-mode
                               'shell-mode
			       'sgml-mode
			       'web-mode
			       'xml-mode
			       'xxml-mode)
  "Programming modes dealt with explicitly in some way.")

(defvar syntactic-close-indent-modes (list
				      'ruby-mode)
  "Mode where ‘indent-according-to-mode’ shall be after closer is inserted. ")

(defvar syntactic-close-emacs-lisp-block-re
  (concat
   "[ \t]*\\_<"
   "(if\\|(cond\\|when\\|unless"
   "\\_>[ \t]*"))

(defvar syntactic-close-import-re "^[ \t]*import[ \t]+.+"
  "")

(defvar syntactic-close-for-re "^[ \t]*for([^)]+"
  "")

(defvar syntactic-close-verbose-p nil)

(defvar syntactic-close-assignment-re   "^[^:]*[^ =\t:]+[ \t]+=[ \t]+[^=]+" "")

(setq syntactic-close-assignment-re     "^[^:]*[^ =\t:]+[ \t]+=[ \t]+[^=]+")

(defvar syntactic-close-funcdef-re   "^[ \t]*def[ \t]+[[:alnum:]_]+([^()]+)" "")

(setq syntactic-close-funcdef-re     "^[ \t]*def[ \t]+[[:alnum:]_]+([^()]+)")

(defvar syntactic-close-if-re   "^[ \t]*if[ \t]+.+" "")
(setq syntactic-close-if-re   "^[ \t]*if[ \t]+.+")

(unless (boundp 'py-block-re)
  (defvar py-block-re "[ \t]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. "))

(defvar syntactic-close-type-re
      "\\([[:alpha:]][^\n \t:]*\\) +:: +\\([\[\(_]*[[:alnum:]]+\\)"
      "")

(defvar syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode)
  "`parse-partial-sexp' must scan only from last prompt.")
(setq syntactic-close-known-comint-modes (list 'shell-mode 'inferior-sml-mode 'inferior-asml-mode 'Comint-SML 'haskell-interactive-mode 'inferior-haskell-mode))

(defvar  syntactic-close-beg-delimiter "»‘“{<[("
  "Specify the delimiter char.")

(defvar  syntactic-close-end-delimiter "«’”}>])"
  "Specify the delimiter char.")

(defvar  syntactic-close-end-delimiter-list  (list ?« ?’ ?” ?} ?>  ?\] ?\))
   "Specify the delimiter char.")

(defvar syntactic-close-unary-delimiters-atpt "|\"'`#$/=?!:*+~§%_;@-"
"Specify the delimiter chars. ")

(defvar syntactic-close-match-in-string-p nil
  "Internal use only.")

(defvar syntactic-close-match-in-comment-p nil
  "Internal use only.")

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
    (?` ?`)
    (?» ?«)
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
		    nafter)
                 (setq erg (concat erg " ")))))
	    erg))

(defun syntactic-close--string-fence-intern (orig start end)
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

(defun syntactic-close--string-fence (orig)
  "Closing a string fence."
  (cond
   ((looking-at "(\\* *")
    comment-end)
   ((looking-at "\\/\\*")
    "*/")
   ((looking-at "{-# ")
    " #-}")
   ((looking-at "{- ")
    " -}")
   (t (match-string-no-properties 0))))

(defun syntactic-close--insert-comment-end-maybe (orig pps)
  "Insert comment end.

Argument PPS should provide result of ‘parse-partial-sexp’."
  (goto-char (nth 8 pps))
  (when (looking-at comment-start-skip)
    (if (< 1 (length (match-string-no-properties 0)))
        (syntactic-close--string-fence orig)
            (if (string= "" comment-end)
	       (if (eq system-type 'windows-nt)
		   "\r\n"
	         "\n")
        ))))

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

(defun syntactic-close-ml ()
  "Close in Standard ML."
  (interactive "*")
  (cond ((derived-mode-p 'sgml-mode)
	 (setq syntactic-close-tag 'sgml-close-tag)
	 (funcall syntactic-close-tag)
	 ;; (save-excursion (font-lock-fontify-region (point-min)(point-max)))
	 t)))

(defun syntactic-close--return-complement-string-maybe (erg)
  (cond
   ((string= erg "{-# ") " #-}")
   ((string= erg "{-") "-}")
   ))

;; (defun syntactic-close-haskell (orig pps)
;;   ;; (require 'ar-haskell-mode)
;;   (let (erg)
;;     ;; syntactic-close-intern
;;     ;; (if (nth 4 pps)
;;     ;;          (save-excursion
;;     ;;            (goto-char (nth 8 pps))
;;     ;;            (looking-at "{-")

;;     ;;                  (syntactic-close--return-complement-string-maybe "{-")
;;     ;;)
;;     (let ((splitter (and
;; 		     (eq 1 (count-matches "|" (line-beginning-position) (point)))
;; 		     (nth 1 pps) (save-excursion (goto-char (nth 1 pps)) (eq (char-after) ?\[)))))
;;       (if splitter
;;           "]"
;;         (cond

;;          ;; ((nth 1 pps)
;; 	 ;;             (syntactic-close--haskell-list-forms orig pps))
;;          ((nth 3 pps)
;;           (if (booleanp (nth 3 pps))
;;               (save-excursion (goto-char (nth 8 pps)) (looking-at "[\"']\\{1,3\\}") (match-string-no-properties 0))
;;             (char-to-string (nth 3 pps)))))))))

;; (defun delimited-atpt-intern--find-end (orig upper-bound beg)
;;   (let ((beg (cons (match-beginning 0) (match-end 0)))
;;         (this (point))
;;         (erg (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) upper-bound 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p)))
;;     (if
;;         (ignore-errors
;;           (and erg (< orig (cadr erg))))
;;         (progn
;;           (setq delimited-start beg)
;;           (setq delimited-end erg)
;;           beg)
;;           (goto-char this)
;;           nil)))

(defun syntactic-close-intern--repeat (orig pps limit)
  "Internal use only."
  (let ((begdel (concat syntactic-close-beg-delimiter syntactic-close-unary-delimiters-atpt)))
    (unless (bobp)
      (forward-char -1)
      (unless (looking-at (concat "[" begdel "]+"))
        (while (and (< 0 (abs (skip-chars-backward (concat "^" begdel) limit)))
                    (or (and (not syntactic-close-match-in-comment-p)
                             (nth 4 (setq pps (parse-partial-sexp limit (point)))))))
          (when
              (or (and (not syntactic-close-match-in-comment-p) (nth 4 pps))
                  (and (not syntactic-close-match-in-string-p (nth 3 pps))))
            (goto-char (nth 8 pps)))))
      (syntactic-close--generic orig pps limit))))

(defun syntactic-close--special-refuse (escaped)
  "We try applying a generic function.

However, there are some corner-cases.
In Emacs-lips ‘|’ might be a delimiter, but escaped ‘\\|’
being just part of a regexp. "
  (save-match-data
    (cond ((eq major-mode 'emacs-lisp-mode)
           (cond ((and (string= "|" (match-string-no-properties 0))
                       escaped)
                  t)
                 ((string= "'" (match-string-no-properties 0))
                  t)
                 ((and (string= ":" (match-string-no-properties 0))
                       (eq (char-before) ??))
                  t)
                 ((and (< 1 (length (match-string-no-properties 0)))
                     (not (syntactic-close--string-fence (point)))
                     t))
                 (t nil)))
          (t nil))))

(defun syntactic-close--generic-splitted (escaped padding)
  (progn
    (if (< 1 (length (match-string-no-properties 0)))
        (if escaped
            (concat "\\\\" (syntactic-close--string-fence (match-string-no-properties 0)))
          (syntactic-close--string-fence (match-string-no-properties 0)))
      (if padding
          (if escaped
              (concat "\\\\" (concat padding (char-to-string (syntactic-close--return-complement-char-maybe (char-after)))))
            (concat padding (char-to-string (syntactic-close--return-complement-char-maybe (char-after)))))
        (if escaped
            (concat "\\\\"
                    (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))
          (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))))))

(defun syntactic-close--generic (orig pps limit)
  (interactive)
  (let ((escaped (if (nth 3 pps)
                     (syntactic-close--escaped-in-string-p)
                   (syntactic-close--escaped-p)))
        (padding (when syntactic-close-honor-padding-p (syntactic-close--padding-maybe (1+ (point))))))
    (cond
     ((and (nth 3 pps) (ignore-errors (equal (char-after) (nth 3 pps)))(< (point) orig))
      (char-to-string (nth 3 pps)))
     ((and
       (< (point) orig)
       (looking-at (concat "[" syntactic-close-beg-delimiter "]"))
       (not (syntactic-close--special-refuse escaped)))
      (syntactic-close--generic-splitted escaped padding))
     ((and
       (< (point) orig)
       (looking-at (concat "[" syntactic-close-unary-delimiters-atpt "]+"))
       (not
        (or
         (eq (car-safe (syntax-after (point))) 6)
         ;; MULTIFORM=$(
         ;; curl -k -A http://foo.com |
         ;; grep -m1 multiform |
         ;; tr '=' '\n' |
         ;; tail -1 |
         ;; cut -d "'" -f 2
         ;; avvoid match of syntax-chars
         (eq (car-safe (syntax-after (point))) 7)
         ;; exclude ://
         ;; http://foo.com
         (save-excursion
           (skip-chars-backward syntactic-close-unary-delimiters-atpt)
           (looking-at "://"))
         (eq 0 (%  (count-matches (match-string-no-properties 0) limit orig) 2))))
       (progn (skip-chars-backward syntactic-close-unary-delimiters-atpt)
              ;; "[[:alpha:, refute last colon
              ;; (< (point) (1- orig))
              ;; "[[:alpha:, refute first colon
              (looking-at (concat "[" syntactic-close-unary-delimiters-atpt "]+")))
       ;; (not (syntactic-close--special-refuse escaped))
       (syntactic-close--generic-splitted escaped padding)))
     (t (unless (bobp)
          (save-restriction
            (when limit (narrow-to-region limit orig))
            (cond
             ((and (not (nth 3 pps)) (not (bobp)) (save-excursion (forward-char -1) (nth 3 (parse-partial-sexp limit (point)))))
              (backward-sexp)
              (syntactic-close--generic orig pps limit))
             ((member (char-before) syntactic-close-end-delimiter-list)
              (backward-sexp)
              (syntactic-close-intern--repeat orig pps limit))
             (t (unless (bobp) (syntactic-close-intern--repeat orig pps limit))))))))))

(defun syntactic-close--org-mode-close (orig pps limit)
  "Org mode specific closes."
  ;; +BEGIN_QUOTE
  (unless (or (nth 8 pps) (nth 3 pps) (nth 1 pps) (nth 4 pps))
    (if (save-excursion
          (and (re-search-backward "^#\\+\\([A-Z]+\\)_\\([A-Z]+\\)" limit t 1)
               (string= "BEGIN" (match-string-no-properties 1))))
        (progn
          (unless (empty-line-p)
            (end-of-line)
            (newline))
          (insert (concat "#+END_" (match-string-no-properties 2)))
          t)
      (goto-char orig)))
  )

;; (defun syntactic-close-emacs-lisp (orig pps limit)
;;   "Close in Emacs Lisp.

;; Argument CLOSER the char to close.
;; Argument PPS should provide result of ‘parse-partial-sexp’.
;; Optional argument ORG read ‘org-mode’."
;;   (let ((syntactic-close-unary-delimiter-chars (list ?` ?\" ?+)))
;;     ;; (unary-delimiters-strg (cl-map 'string 'identity unary-delimiter-chars))
;;     ;; (delimiters (concat syntactic-close-paired-openers-strg syntactic-close-paired-closers-strg unary-delimiters-strg))
;;     (cond
;;      ;; ((and (not (nth 8 pps))(nth 1 pps))
;;      ;;  (syntactic-close-pure-syntax pps))
;;      ((syntactic-close--generic orig pps limit)))))

(defun syntactic-close--paren-conditions (bracecounter bracketcounter parencounter)
  (cond
   ((eq (char-before) ?{)
    (setq bracecounter (1+ bracecounter)))
   ((eq (char-before) ?\[)
    (setq bracketcounter (1+ bracketcounter)))
   ((eq (char-before) 40)
    (setq parencounter (1+ parencounter)))
   ((eq (char-before) ?})
    (setq bracecounter (1- bracecounter)))
   ((eq (char-before) ?\])
    (setq bracketcounter (1- bracketcounter)))
   ((eq (char-before) 41)
    (setq parencounter (1- parencounter)))))

(defun syntactic-close--paren-inside-string (pos)
  "Return the brace(s) if existing inside a string at point."
  (let (
        (bracecounter 0)
        (bracketcounter 0)
        (parencounter 0))
    (while (and (or
                 (prog1
                     (and
                      (syntactic-close--paren-conditions bracecounter bracecounter parencounter)
                      (forward-char -1)))
                 (< 0 (abs (skip-chars-backward "^{}()[]'\"" (1+ pos))))))
      (syntactic-close--paren-conditions bracecounter bracecounter parencounter)
       (unless (or (< bracecounter 0) (< bracketcounter 0) (< parencounter 0))
         (forward-char -1)))
    (cond
     ((< 0 bracecounter)
      "}")
     ((< 0 bracketcounter)
      "]")
     ((< 0 parencounter)
      ")"))))

(defun syntactic-close--check-bracecounter (bracecounter doublequotecounter singlequotecounter)
  (when (and (< 0 bracecounter) (eq (char-after) ?{))
    (cond
     ((and (< 0 (% doublequotecounter 2)))
      "\"")
     ((< 0 (% singlequotecounter 2))
      "'")
     (t
      (char-to-string (syntactic-close--return-complement-char-maybe (char-after)))))))

(defun syntactic-close--check-bracketcounter (bracketcounter doublequotecounter singlequotecounter)
  (when (and (< 0 bracketcounter) (eq (char-after) ?\[))
    (cond
     ((and (< 0 (% doublequotecounter 2)))
      "\"")
     ((< 0 (% singlequotecounter 2))
      "'")
     (t
      (char-to-string (syntactic-close--return-complement-char-maybe (char-after)))))))

(defun syntactic-close--check-parencounter (parencounter doublequotecounter singlequotecounter)
  (when (and (< 0 parencounter) (eq (char-after) ?\())
    (cond
     ((and (< 0 (% doublequotecounter 2)))
      "\"")
     ((< 0 (% singlequotecounter 2))
      "'")
     (t
      (char-to-string (syntactic-close--return-complement-char-maybe (char-after)))))))

(defun syntactic-close--check-doublequotecounter (doublequotecounter bracecounter)
  (cond ((and (< 0 (% doublequotecounter 2)) (eq (char-after) ?\"))
         (if (<  bracecounter 0)
             "}"
           (char-to-string (syntactic-close--return-complement-char-maybe (char-after)))))))

(defun syntactic-close--check-singlequotecounter (singlequotecounter bracecounter)
  (cond ((and (< 0 (% singlequotecounter 2)) (eq (char-after) ?'))
         (if (<  bracecounter 0)
             "}"
           (char-to-string (syntactic-close--return-complement-char-maybe (char-after)))))))

(defun syntactic-close--discard-closed-braced ()
  (interactive)
  (save-excursion
    (let ((bracecounter 0))
      (while
          (and
           (not (< 0 bracecounter))
           (re-search-backward "[{}]" (line-beginning-position) t))
        (unless (syntactic-close--escaped-p)
          (cond
           ((eq (char-after) ?{)
            (setq bracecounter (1+ bracecounter)))
           ((eq (char-after) ?})
            (setq bracecounter (1- bracecounter)))
           ))
        (and (eq 0 bracecounter) (kill-sexp))))))

;; ((pps (parse-partial-sexp (point-min) (point))))
;; f\"{f\"{f\"{f\"{f\"{f\"{1+1
;; (nth 1 pps) tells all, (nth 8 pps) is zero
;; f\"{f\"{f\"{f\"{f\"{f\"{1+1}
;; (nth 1 pps) and (nth 8 pps) is zero, but ‘\\"’ required
;; f\"{f\"{f\"{f\"{f\"{f\"{f\"{1+1
;; (nth 1 pps) zero, < (nth 8 pps) {
;; f'Useless use of lambdas: { (lambda x: x*2
;; (nth 1 pps) zero, < (nth 8 pps) (, < (nth 8 pps) {
(defun syntactic-close--python-f-string (strg &optional bracecounter bracketcounter parencounter doublequotecounter singlequotecounter erg)
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert strg)
    (let ((bracecounter (or bracecounter 0))
          (bracketcounter (or bracketcounter 0))
          (parencounter (or parencounter 0))
          (doublequotecounter (or doublequotecounter 0))
          (singlequotecounter (or singlequotecounter 0))
          (erg (or erg nil))
          padding)
      (syntactic-close--discard-closed-braced)
      (while (syntactic-close--escaped-p)
        (forward-char -1))
      (when
          (or
           (prog1 (< 0 (abs (skip-chars-backward "^[](){}\"'")))
             ;; (re-search-backward "[\\\\[(){\"'}\]]" nil t 3)

             (while (syntactic-close--escaped-p)
               (forward-char -1)))
           (prog1 (member (char-before) (list 40 41 91 93 ?{ ?} ?\" ?'))
             (while (syntactic-close--escaped-p)
               (forward-char -1))))
        (cond
         ((eq (char-before) ?{)
          (setq bracecounter (1+ bracecounter))
          (forward-char -1))
         ((eq (char-before) ?\[)
          (setq bracketcounter (1+ bracketcounter))
          (forward-char -1))
         ((eq (char-before) 40)
          (setq parencounter (1+ parencounter))
          (forward-char -1))
         ((eq (char-before) ?})
          (setq bracecounter (1- bracecounter))
          (forward-char -1))
         ((eq (char-before) ?\])
          (setq bracketcounter (1- bracketcounter))
          (forward-char -1))
         ((eq (char-before) 41)
          (setq parencounter (1- parencounter))
          (forward-char -1))
         ((eq (char-before) ?\")
          (setq doublequotecounter (1+ doublequotecounter))
          (forward-char -1))
         ((eq (char-before) ?')
          (setq singlequotecounter (1+ singlequotecounter))
          (forward-char -1)))
        (when (or (and (member (char-after) (list 40 91 ?{))
                       (not (syntactic-close--escaped-p)))
                  (bobp))
          (when (looking-at ".\\([ \t]+\\)")
            (setq padding (match-string-no-properties 1)))
          (setq erg (or (syntactic-close--check-bracecounter bracecounter doublequotecounter singlequotecounter)
                        (syntactic-close--check-bracketcounter bracketcounter doublequotecounter singlequotecounter)
                        (syntactic-close--check-parencounter parencounter doublequotecounter singlequotecounter)
                        (syntactic-close--check-doublequotecounter doublequotecounter bracecounter)
                        (syntactic-close--check-singlequotecounter singlequotecounter bracecounter)
                        (cond
                         ((and (looking-back "[fF]r?" (line-beginning-position)) (eq (char-after) ?\")(< 0 (% doublequotecounter 2)))
                          (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))
                         ((and (looking-back "[fF]r?" (line-beginning-position)) (eq (char-after) ?')(< 0 (% singlequotecounter 2)))
                          (char-to-string (syntactic-close--return-complement-char-maybe (char-after))))))))
        (if erg
            (if padding
                (setq erg (concat padding erg))
              erg)
          (syntactic-close--python-f-string (buffer-substring-no-properties (line-beginning-position) (point)) bracecounter bracketcounter parencounter doublequotecounter singlequotecounter erg))))))

(defun syntactic-close-python-close (orig limit pps)
  "Might deliver equivalent to `py-dedent'.

Argument B-OF-ST read beginning-of-statement.
Argument B-OF-BL read beginning-of-block.
Optional argument PADDING to be done.
Optional argument PPS is result of a call to function ‘parse-partial-sexp’"
  (let* ((f-pos (progn (while (re-search-backward "\\(.*\\)[fF]r?\\(['\"]\\{1,3\\}\\).*" (line-beginning-position) t)) (match-beginning 2))))
    (cond
     (f-pos
      (syntactic-close--python-f-string (buffer-substring-no-properties f-pos orig)))
     ((and (nth 8 pps) (nth 3 pps))
      (or (save-excursion (syntactic-close--paren-inside-string (nth 8 pps)))
	  (save-excursion (goto-char (nth 8 pps)) (looking-at "[\"']\\{1,3\\}") (match-string-no-properties 0))))
     ((nth 1 pps)
      (syntactic-close-pure-syntax pps))
     ;; ((looking-back "^[ 	]+if[ 	]+.*" (line-beginning-position))
     ;; py-block-or-clause-re
     ((looking-back "[ 	]*\\_<\\(async \\(?:class\\|def\\|for\\|with\\)\\|c\\(?:ase\\|lass\\)\\|def\\|e\\(?:l\\(?:if\\|se\\)\\|xcept\\)\\|f\\(?:inally\\|or\\)\\|if\\|match\\|try\\|w\\(?:hile\\|ith\\)\\)\\_>[( 	]*.*" (line-beginning-position))
      ":")
     (t (syntactic-close--generic orig pps limit)))))

;; nil
;; 				  (if (functionp 'py--beginning-of-statement-position)
;; 				      (py--beginning-of-statement-position)
;; 				    (save-excursion (python-nav-beginning-of-statement)))
;; 				  t)))))

;; Haskell
(defun syntactic-close-haskell (orig pps limit)
  "Optional argument PPS is result of a call to function ‘parse-partial-sexp’"
  (interactive "*")
  (let* ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 8 pps)
      (syntactic-close-generic-forms orig limit pps))
     ((nth 1 pps)
      (syntactic-close-pure-syntax pps))
     (t (syntactic-close--generic orig pps limit)))))

;; Ruby
(defun syntactic-close--ruby ()
  (unless (or (looking-back ";[ \t]*" nil))
    (if (and (bolp) (eolp))
        "end"
      "\nend")))

(defun syntactic-close-ruby-close (orig pps)
  "Ruby specific close.

Argument PPS is result of a call to function ‘parse-partial-sexp’"
  (cond ((looking-back "#{[_[:alpha:]][_[:alnum:]]*" (line-beginning-position))
         "}")
        ((ignore-errors (and (< (nth 1 pps) (nth 8 pps))(syntactic-close--string-before-list-maybe  pps))))
	((and (or (nth 1 pps) (nth 3 pps)) (syntactic-close-pure-syntax pps)))
	(t (syntactic-close--ruby))))

(defun syntactic-close-java-another-filter-clause (pps)
  ""
  (let ((pps pps))
    (cond ((save-excursion
             ;; (goto-char (nth 1 pps))
             (while (nth 1 (setq pps (parse-partial-sexp (point-min) (point))))
               (goto-char (nth 1 pps)))
             (progn (ignore-errors (forward-sexp)) (and (not (nth 1 (parse-partial-sexp (point-min) (point))))(eq (char-after) ?{)))))
          ((looking-back syntactic-close-assignment-re (line-beginning-position))))))

(defun syntactic-close-java (orig pps limit)
  "Optional argument PPS is result of a call to function ‘parse-partial-sexp’"
  (interactive "*")
  (cond
   ((nth 8 pps)
    (syntactic-close-generic-forms orig limit pps))
   ((nth 1 pps)
    (cond ((and (looking-back syntactic-close-for-re (line-beginning-position)) (not (eq (char-before) ?\;)) (not (string-match "\\+\\+" (buffer-substring-no-properties (line-beginning-position) (point)))))
           "; ")
          (t (save-excursion (goto-char (nth 1 pps))
                             (if (eq (char-after) 40)
                                 ")"
                               (goto-char orig)
                               (cond ((looking-back "^[ \t]*" (line-beginning-position))
                                      "}")
                                     (t (unless (eq (char-before) ?\;) ";"))))))))
   ((looking-back syntactic-close-assignment-re (line-beginning-position))
    (unless (eq (char-before) ?\;) ";"))
   ((and (looking-back syntactic-close-funcdef-re (line-beginning-position))
         (eq (char-before) 41))
    ":")
   ((looking-back "^[ \t]*" (line-beginning-position))
    "}")
   (t
    ";"
    ;; (syntactic-close--generic nil nil pps)
    )))

(defun syntactic-close-scala-another-filter-clause (pps)
  "Semicolon only required with inside parentized

If you prefer, you can use curly braces instead of parentheses to
surround the generators and filters. One advantage to using curly
braces is that you can leave off some of the semicolons that are
needed when you use parentheses.

Source: Odersky, Spoon, Venners: Programming in Scala"
  (let ((indent (current-indentation))
        done)
    (cond ((looking-back "^[ \t]+if[ \t].*" (line-beginning-position))
           (unless (save-excursion (goto-char (nth 1 pps)) (eq (char-after) ?{))

             (back-to-indentation)
             (while (and (forward-line 1)
                         (progn (back-to-indentation) (eq (current-column) indent)))
               (when (looking-at "if[ \t]") (setq done t)))))
          ((looking-back syntactic-close-assignment-re (line-beginning-position))
           (setq done t)))
    done))

(defun syntactic-close-scala-close (orig limit pps)
  "Optional argument PPS is result of a call to function ‘parse-partial-sexp’"
  (interactive "*")
  (let* ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 8 pps)
      (syntactic-close-pure-syntax pps)
      )
     ((nth 1 pps)
      (if (save-excursion (syntactic-close-scala-another-filter-clause pps))
          (if (and (not (eq (char-before) ?\;))
                    ;; (eq (char-before) 41)
                    ;; val foo =  this.totalFoo(Seq(("apple", 2))
                    ;; select ";" if opening paren is not at same line
                   (< (save-excursion (goto-char (nth 1 pps))(count-lines (point-min) (line-end-position))) (count-lines (point-min) (line-end-position)))
                   (or
                    (looking-back syntactic-close-if-re (line-beginning-position))
                    (looking-back syntactic-close-assignment-re (line-beginning-position))
                   ))
              ";"
            (syntactic-close-pure-syntax pps))
        (syntactic-close-pure-syntax pps)))
     ((looking-back syntactic-close-assignment-re (line-beginning-position))
      (unless (eq (char-before) ?\;) ";"))
     ((and (looking-back syntactic-close-funcdef-re (line-beginning-position))
           (eq (char-before) 41))
      ":")

     (t (syntactic-close--generic orig pps limit)))))

(defun syntactic-close-shell-close (orig pps limit)
  "Optional argument PPS is result of a call to function ‘parse-partial-sexp’"
  (interactive "*")
  (let* ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 8 pps)
      (syntactic-close-generic-forms orig limit pps))
     ((nth 1 pps)
      (syntactic-close-pure-syntax pps))
     (t (syntactic-close--generic orig pps limit))
     )))

(defun syntactic-close--semicolon-modes (orig pps limit)
  "Close specific modes.

Argument PPS, the result of ‘parse-partial-sexp’."
  (let ((closer
	 (cond ((nth 3 pps)
		(save-excursion (goto-char (nth 8 pps))
				(char-to-string (char-after))))
	       ((save-excursion
		  (and (nth 1 pps) (syntactic-close-pure-syntax pps))))
               ((looking-back syntactic-close-import-re (line-beginning-position))
                ";")
	       (t (syntactic-close--generic orig pps limit)))))
    (cond
     ((and closer (string-match "}" closer))
      (if (save-excursion (skip-chars-backward " \t\r\n\f" limit) (member (char-before) (list ?\; ?})))
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

(defun syntactic-close--specific-modes (orig pps limit)
  "Argument PPS result of ‘parse-partial-sexp’."
  (pcase major-mode
    (`agda2-mode
     (syntactic-close-haskell orig pps limit))
    (`emacs-lisp-mode
     (syntactic-close--generic orig pps limit))
    ;; (syntactic-close-emacs-lisp orig pps limit))
    (`html-mode
     (syntactic-close-ml))
    (`haskell-mode
     (syntactic-close-haskell orig pps limit))
    (`java-mode
     (syntactic-close-java orig pps limit))
    (`mhtml-mode
     (syntactic-close-ml))
    (`nxml-mode
     (syntactic-close--finish-element))
    (`org-mode
     (or
      (syntactic-close--org-mode-close orig pps limit)
      (syntactic-close--generic orig pps limit)
      ))
    (`python-mode
     (syntactic-close-python-close orig limit pps))
    (`prolog-mode
     (syntactic-close--generic orig pps limit))
    (`ruby-mode
     (syntactic-close-ruby-close orig pps))
    (`scala-mode
     (syntactic-close-scala-close orig limit pps))
    (`shell-mode
     (syntactic-close-shell-close orig pps limit))
    (`sgml-mode
     (syntactic-close-ml))
    (`xml-mode
     (syntactic-close-ml))
    (`xxml-mode
     (syntactic-close-ml))))

(defun syntactic-close--modes (orig pps limit)
  "Argument PPS, the result of ‘parse-partial-sexp’."
  (cond ((member major-mode syntactic-close--semicolon-separator-modes)
         (syntactic-close--semicolon-modes orig pps limit))
        ;; (pcase major-mode
        ;;   (`java-mode (syntactic-close--semicolon-modes pps))
        ;;   (`js-mode (syntactic-close--semicolon-modes pps))
        ;;   (`php-mode (syntactic-close--semicolon-modes pps))
        ;;   (`python-mode (syntactic-close-python-close pps))
        ;;   (`web-mode (syntactic-close--semicolon-modes pps))
        ;;   (_
        (t
         ;; (if
	 ;;       (ignore-errors (and (nth 3 pps) (< (nth 1 pps) (nth 8 pps))))
	 ;;       (syntactic-close--string-before-list-maybe pps)
	 (syntactic-close--specific-modes orig pps limit))))

(defun syntactic-close--finish-element ()
  "Finish the current element by inserting an end-tag."
  (interactive "*")
  (let ((orig (point)))
    (nxml-finish-element-1 nil)
    (< orig (point))))

(defun syntactic-close-generic-forms (orig limit pps)
  "Argument PPS, the result of ‘parse-partial-sexp’."
  (cond
   ((syntactic-close--generic orig pps limit))
   ((nth 4 pps)
    ;; in comment
    (syntactic-close--insert-comment-end-maybe orig pps))))

  ;; (cond
  ;;       ((syntactic-close-pure-syntax pps))
  ;;       ((syntactic-close--generic orig pps limit))
  ;;       ))

(defun syntactic-close-intern (orig iact pps limit)
  "A first dispatch.

Argument ORIG the position command was called from.
Argument BEG the lesser border.
Argument IACT signals an interactive call.
Optional argument PPS, the result of ‘parse-partial-sexp’."
  (when syntactic-close-electric-delete-whitespace-p
    (delete-region orig (progn (skip-chars-backward " \t" (line-beginning-position))(point))))
  (let ((closer
    	 (cond ((nth 4 pps)
                (syntactic-close--insert-comment-end-maybe orig pps))
	       ((member major-mode syntactic-close-modes)
	        (syntactic-close--modes orig pps limit))
	       (t (syntactic-close--generic orig pps limit)))))
    ;; insert might be hardcoded like in ‘nxml-finish-element-1’
    (when (and closer (stringp closer))
      (goto-char orig)
      (insert closer)
      ;; ruby end
      (when (member major-mode syntactic-close-indent-modes)
	(save-excursion (indent-according-to-mode))))
    (or (< orig (point)) (and iact (message "%s" "nil") nil))))

(defun syntactic-close-update-delimiters ()
  (cond ((eq major-mode 'emacs-lisp-mode)
         "\"':")
        (t syntactic-close-unary-delimiters-atpt)))

;;;###autoload
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
  (let* ((orig (copy-marker (point)))
	 (beg (or beg (syntactic-close--point-min)))
	 (pps (or pps (parse-partial-sexp beg (point))))
         (limit (cond
                 ((and (nth 8 pps) (nth 1 pps))
                  (max (nth 8 pps) (nth 1 pps)))
                 ((nth 8 pps))
                 ((nth 1 pps))
                 (t (point-min))))
         (syntactic-close-match-in-string-p (when (nth 3 pps) (nth 8 pps)))
         (syntactic-close-match-in-comment-p (nth 4 pps))
	 (iact (or iact arg))
	 (arg (or arg 1))
         (syntactic-close-unary-delimiters-atpt (syntactic-close-update-delimiters)))
    (pcase (prefix-numeric-value arg)
      (4 (while (syntactic-close))
	 (unless (< orig (point)) (progn (goto-char orig) nil)))
      (_ (syntactic-close-intern orig iact pps limit)
         (if (< orig (point)) (point) (progn (goto-char orig) nil))))))

(provide 'syntactic-close)
;;; syntactic-close.el ends here
