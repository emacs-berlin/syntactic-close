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

(defvar general-close-verbose-p nil)

(defvar general-close-keywords nil
  "Knowing keywords avoids call for face-at-point:

conditionals closed by a colon for example. ")

(defvar general-close-command-separator-char ?\;
  "This char might be modified internally. ")

(defvar general-close-list-separator-char ?,
  "This char might be modified internally. ")

(defvar general-close-known-comint-modes (list 'inferior-sml-mode)
  "`parse-partial-sexp' must scan only from last prompt. ")

(defvar general-closs-empty-line-p-chars nil)
(defcustom general-closs-empty-line-p-chars "^[ \t\r]*$"
  "empty-line-p-chars"
  :type 'regexp
  :group 'convenience)

(defun general-close-empty-line-p (&optional iact)
  "Returns t if cursor is at an empty line, nil otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at empty-line-p-chars)))
    (looking-at empty-line-p-chars)))

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
  (let (erg closer)
    (cond ((nth 4 pps)
	   (if (string= "" comment-end)
	       (if (eq system-type 'windows-nt)
		   "\r\n"
		 "\n")
	     comment-end))
	  ((nth 3 pps)
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
     (t (setq done (general-close--insert-delimiter-char-maybe orig closer))))
    done))

(defun general-close-sml (pps closer orig)
  (let (done)
    (cond (closer
	   (insert closer)
	   (setq done t))
	  ((eq (char-before) general-close-command-separator-char)
	   (comint-send-input)
	   (goto-char (point-max))
	   (setq done t))
	  ;; (general-close--intern (orig closer pps))
	  ;; (t (setq done (general-close-insert-closing-char pps)))
)
    done))

(defun general-close--modes (pps closer orig)
  (let (done)
    (cond
     ((eq major-mode 'inferior-sml-mode)
      (setq done (general-close-sml pps closer orig)))
     ((eq major-mode 'php-mode)
      (setq done (general-close--php-check pps closer)))
     ((eq major-mode 'python-mode)
      (setq done (general-close-python-close closer)))
     ((eq major-mode 'ruby-mode)
      (setq done (general-close-ruby-close closer)))
     ((member major-mode general-close--ml-modes)
      (setq done (general-close-ml)))
     (t (setq done (general-close--intern orig closer pps))))
    done))


(defun general-close--travel-comments-maybe (pps)
  (end-of-line)
  (skip-chars-backward " \t")
  (when (ignore-errors (looking-at comment-start))
    (goto-char (match-end 0))
    ;; travel comments
    (while (and (setq pps (parse-partial-sexp (point-min) (point))) (nth 4 pps) (nth 8 pps))
      (goto-char (nth 8 pps))
      (skip-chars-backward " \t\r\n\f"))))

(defun general-close ()
  "Command will insert closing delimiter whichever needed. "
  (interactive "*")
  (let* ((beg (if (member major-mode general-close-known-comint-modes)
		  (save-excursion
		    (when comint-prompt-regexp
		      (re-search-backward comint-prompt-regexp nil t 1)))
		(point-min)))
	 (pps (parse-partial-sexp beg (point)))
	 done orig closer)
    (general-close--travel-comments-maybe pps)
    (setq orig (point))
    ;; in string or list?
    (setq closer (general-close--fetch-delimiter-maybe pps))
    (setq done (general-close--modes pps closer orig))
    (unless done (setq done (when closer (progn (insert closer) t))))
    (unless done (newline))
    (when general-close-electric-indent-p
      (indent-according-to-mode))))

(provide 'general-close)
;;; general-close.el ends here
