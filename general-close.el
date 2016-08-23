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

;;; Commentary: A still naive implementation of a general-close
;;  command. A first draft was published at emacs-devel list:
;;
;; http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

;;

;;; Code:

;; Some valid Emacs Lisp suitable for testing
;; (setq foo (list "([{123}])"))

(require 'general-close-modes)
(require 'sgml-mode)

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

(defcustom general-close-electric-indent-p nil
  "When `t', after insert at empty line indent according to mode.

Default is nil"

  :type 'boolean
  :tag "general-close-electric-indent-p"
  :group 'general-close)

(defcustom general-close--semicolon-separator-modes
  (list
   'js-mode
   'js2-mode
   'perl-mode
   'php-mode
   'web-mode
   )
  "List of modes which commands must be closed by `general-close-command-separator-char. "

  :type 'list
  :tag "general-close--semicolon-separator-modes"
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

(defvar general-close-comint-pre-assignment-re   "let [alpha][A-Za-z0-9_]")
(defcustom general-close-comint-pre-assignment-re
  "let [alpha][A-Za-z0-9_]"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-comint-pre-assignment-re"
  :group 'general-close)

(defvar general-close-pre-assignment-re   "[alpha][A-Za-z0-9_]")
;; (setq general-close-pre-assignment-re   "[alpha][A-Za-z0-9_]")
(defcustom general-close-pre-assignment-re
  "[alpha][A-Za-z0-9_]"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-pre-assignment-re"
  :group 'general-close)

(defvar general-close-comint-pre-right-arrow-re   "let [alpha][A-Za-z0-9_]+ +::")
;; (setq general-close-comint-pre-right-arrow-re   "let [alpha][A-Za-z0-9_]+ +::")
(defcustom general-close-comint-pre-right-arrow-re
  "let [alpha][A-Za-z0-9_]+ +::"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-comint-pre-right-arrow-re"
  :group 'general-close)

(defvar general-close-pre-right-arrow-re   "[alpha][A-Za-z0-9_]+ +::")
;; (setq general-close-pre-right-arrow-re   "[alpha][A-Za-z0-9_]+ +::")
(defcustom general-close-pre-right-arrow-re
  "[alpha][A-Za-z0-9_]+ +::"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-pre-right-arrow-re"
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

(defun general-close--return-compliment-char-maybe (erg)
  "For example return \"}\" for \"{\" but keep \"\\\"\". "
  (cond ((eq erg ?\")
	 erg)
	((eq erg ?')
	 erg)
	((eq erg ?\[)
	 ?\])
	((eq erg ?{)
	 ?\})
	((eq erg ?\()
	 ?\))))

(defun general-close--in-string-p-intern (pps)
  "Returns start-position, delimiter-char, delimiter-lenth a list. "
  (goto-char (nth 8 pps))
  (list (point) (char-after)(skip-chars-forward (char-to-string (char-after)))))

(defun general-close-in-string-p ()
  "if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 "
  (interactive)
  (save-excursion
    (let* ((pps (parse-partial-sexp (point-min) (point)))
	   (erg (when (nth 3 pps)
		  (general-close--in-string-p-intern pps))))
      (unless erg
	(when (looking-at "\"\\|'")
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
	       (setq closer (general-close--return-compliment-char-maybe (char-before)))
	       (if (eq (car stack) closer)
		   (progn
		     (pop stack)
		     (forward-char -1))
		 (setq done t)))
	      (t (skip-chars-backward "^\"{\(\[\]\)}")))))
    (insert closer)))

(defun general-close--in-string-interpolation-maybe (pps)
  (and (< 0 (abs (skip-syntax-backward "\\sw")))
       (member (char-before) (list ?\( ?{ ?\[))
       (member (char-before (1- (point))) (list ?% ?#))
       (< (nth 8 pps) (point))
       (general-close--return-compliment-char-maybe (char-before))))

;; (defun general-close--in-string-interpolation-maybe (pps)
;;   (cond ((and (ignore-errors (< (nth 1 pps) (nth 8 pps))))
;; 	 (save-excursion
;; 	   (goto-char (nth 8 pps))

;; 	   (general-close--return-compliment-char-maybe (char-after))))))

(defun general-close--fetch-delimiter-maybe (pps)
  "Close lists resp. strings if inside. "
  (let (erg closer)

    (cond ((nth 3 pps)
	   (save-excursion
	     (or
	      ;; sets closer to compliment character
	      (setq closer (general-close--in-string-interpolation-maybe pps))
	      ;; returns a list to construct TQS maybe
	      (and (setq erg (general-close--in-string-p-intern pps))
		   (setq closer (make-string (nth 2 erg)(nth 1 erg))))))
	   closer)
	  ((nth 1 pps)
	   (save-excursion
	     (goto-char (nth 1 pps))
	     (general-close--return-compliment-char-maybe (char-after)))))))

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
  (cond ((eq closer ?})
	 (if
	     (save-excursion
	       (skip-chars-backward " \t\r\n\f")
	       (or (eq (char-before) general-close-command-separator-char)
		   (eq (char-before) closer)))
	     (progn
	       (unless (looking-back "^[ \t]+" nil)
		 (newline-and-indent))
	       (insert closer))
	   (insert general-close-command-separator-char))
	 closer)
	((and (eq closer ?\)) (eq (char-before) ?\;))
	 (newline-and-indent)
	 (insert closer)
	 closer)
	;; Semicolon inserted where it probably shouldn't be? #12
	;; ((and (eq closer ?\)) (eq (char-before) ?\)))
	;;  (insert general-close-command-separator-char)
	;;  closer)
	(closer
	 (insert closer)
	 closer)

	(t (general-close--insert-separator-maybe orig))))

(defun general-close--intern (orig closer pps)
  (let (done)
    (cond
     ;; a command separator may precede closing delimiter
     ((member major-mode general-close--semicolon-separator-modes)
      (setq general-close-command-separator-char ?\;)
      (setq done (general-close--handle-separator-modes orig closer)))
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

(defun general-close--insert-and-fixup (char)
  (fixup-whitespace)
  (if (eq (char-before) ?\ )
      (insert (concat char " "))
      (insert (concat " " char " "))))

(defun general-close--insert-assignment-maybe (beg regexp)
  (let (done)
    (when (save-excursion
	    (goto-char beg)
	    (skip-chars-forward " \t\r\n\f")
	    (looking-at regexp))
      (general-close--insert-and-fixup "=")
      (setq done t))
    done))

(defun general-close--right-arrow-maybe (beg regexp)
  (let (done)
    (when (save-excursion
	    (goto-char beg)
	    (skip-chars-forward " \t\r\n\f")
	    (looking-at regexp))
      (general-close--insert-and-fixup "->")
      (setq done t))
    done))

(defun general-close--which-right-arrow-regex ()
  (cond ((member major-mode  (list 'haskell-interactive-mode 'inferior-haskell-mode))
	 general-close-comint-haskell-pre-right-arrow-re)
	(t general-close-comint-pre-right-arrow-re)))

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

(defun general-close--modes (beg pps orig &optional closer)
  (let (done)
    (cond
     ((eq major-mode 'php-mode)
      (setq done (general-close--php-check closer)))
     ((eq major-mode 'python-mode)
      (setq done (general-close-python-close closer)))
     ((eq major-mode 'ruby-mode)
      (setq done (general-close-ruby-close closer)))
     ((member major-mode general-close--ml-modes)
      (setq done (general-close-ml)))
     ((eq major-mode 'haskell-mode)
      (setq done (general-close-haskell-close beg closer)))
     (t (setq done (general-close--intern orig closer pps))))
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
  (cond ((and (member major-mode (list 'haskell-interactive-mode 'inferior-haskell-mode)) haskell-interactive-mode-prompt-start))
	((save-excursion
	   (and (member major-mode general-close-known-comint-modes) comint-prompt-regexp
		(message "%s" (current-buffer))
		(re-search-backward comint-prompt-regexp nil t 1)
		(looking-at comint-prompt-regexp)
		(message "%s" (match-end 0))))
	 (match-end 0))
	(t (point-min))))

(defun general-close ()
  "Command will insert closing delimiter whichever needed. "
  (interactive "*")
  (let* ((beg (general-close--point-min))
	 (pps (parse-partial-sexp beg (point)))
	 done orig closer)
    ;; ml-modes use sgml-close-tag
    (setq done (general-close--travel-comments-maybe pps))
    (unless done
      (setq orig (point))
      ;; in string or list?
      (setq closer (general-close--fetch-delimiter-maybe pps))
      (if (member major-mode general-close-known-comint-modes)
	  (setq done (general-close-comint beg closer))
	(setq done (general-close--modes beg pps orig closer)))
      (unless done (setq done (when closer (progn (insert closer) t))))
      (unless done
	(member major-mode general-close-known-comint-modes)
	(comint-send-input)
	(newline))))
  (when general-close-electric-indent-p
    (indent-according-to-mode)))

(provide 'general-close)
;;; general-close.el ends here
