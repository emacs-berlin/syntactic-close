;;; general-close.el --- Insert closing delimiter

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

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

(defgroup general-close nil
  "Insert closing delimiter whichever needed. "
  :group 'languages
  :prefix "gen-")

(defcustom gen-delete-whitespace-backward-p nil
  "If whitespace characters before point should be deleted.

Default is nil"

  :type 'boolean
  :group 'general-close)

(defcustom gen-electric-indent-p nil
  "When `t', after insert at empty line indent according to mode.

Default is nil"

  :type 'boolean
  :group 'general-close)

(defcustom gc--separator-modes
  (list
   'js-mode
   'js2-mode
   'perl-mode
   'php-mode
   )
  "List of modes which commands must be closed by `gen-command-separator-char. "

  :type 'list
  :group 'general-close)

(defvar gen-verbose-p nil)

(defvar gen-command-separator-char ?\;)
(setq gen-command-separator-char ?\;)

(defun gen-toggle-verbosity ()
  "If `gen-verbose-p' is nil, switch it on.

Otherwise switch it off. "
  (interactive)
  (setq gen-verbose-p (not gen-verbose-p))
  (when (called-interactive-p 'any) (message "gen-verbose-p: %s" gen-verbose-p)))


(defun gen--return-compliment-char (erg)
  (cond ((eq erg ?\")
	 erg)
	((eq erg ?\[)
	 ?\])
	((eq erg ?\{)
	 ?\})
	((eq erg ?\()
	 ?\))))

(defun gen--in-string-p-intern (pps)
  (goto-char (nth 8 pps))
  (list (point) (char-after)(skip-chars-forward (char-to-string (char-after)))))

(defun gen-in-string-p ()
  "if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 "
  (interactive)
  (save-excursion
    (let* ((pps (parse-partial-sexp (point-min) (point)))
	   (erg (when (nth 3 pps)
		  (gen--in-string-p-intern pps))))
      (unless erg
	(when (looking-at "\"\\|'")
	  (forward-char 1)
	  (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	  (when (nth 3 pps)
	    (setq erg (gen--in-string-p-intern pps)))))

    ;; (list (nth 8 pps) (char-before) (1+ (skip-chars-forward (char-to-string (char-before)))))
    (when (and gen-verbose-p (called-interactive-p 'any)) (message "%s" erg))
    erg)))

(defun general-close-stack-based ()
  "Command will insert closing delimiter whichever needed.

Does not require parenthesis syntax WRT \"{[(\" "
  (interactive "*")
  (let (closer stack done erg pps-list)
    (save-excursion
      (while (and (not (bobp)) (not done))
	(cond ((member (char-before) (list ?\) ?\] ?}))
	       (push (char-before) stack)
	       (forward-char -1))
	      ((member (char-before) (list ?\( ?\" ?{ ?\[))
	       (setq closer (gen--return-compliment-char (char-before)))
	       (if (eq (car stack) closer)
		   (progn
		     (pop stack)
		     (forward-char -1))
		 (setq done t)))
	      (t (skip-chars-backward "^\"{\(\[\]\)}")))))
    (insert closer)))

(defun gen--in-string-interpolation-maybe ()
  (and (< 0 (abs (skip-syntax-backward "\\sw")))
       (member (char-before) (list ?\( ?{ ?\[))
       (gen--return-compliment-char (char-before))))

(defun gc--fetch-delimiter-char-maybe (pps-list)
  (let (erg)
    (cond ((nth 3 pps-list)
	   (save-excursion
	     (or
	      ;; sets closer to compliment character
	      (setq closer (gen--in-string-interpolation-maybe))
	      (and (setq erg (gen--in-string-p-intern pps-list))
		   (setq closer (make-string (nth 2 erg)(nth 1 erg)))))))
	  ((nth 1 pps-list)
	   (save-excursion
	     (goto-char (nth 1 pps-list))
	     (gen--return-compliment-char (char-after)))))))

(defun gc--insert-delimiter-char-maybe (orig closer)
  (when closer
    (when (and (not (looking-back "^[ \t]+")) gen-delete-whitespace-backward-p
	       (< 0 (abs (skip-chars-backward " \t\r\n\f"))))
      (delete-region (point) orig))
    (cond
     ((and (eq closer ?}) (not (eq major-mode 'php-mode)))
      (insert closer)
      closer)
     ((not (eq closer ?}))
      (insert closer)
      closer))))

(defun gc--handle-separator-modes ()
  "Some languages close a command with a special char, often `;'

See `gen-command-separator-char'"
  (cond ((eq closer ?})
	 (if
	     (save-excursion
	       (skip-chars-backward " \t\r\n\f")
	       (or (eq (char-before) gen-command-separator-char)
		   (eq (char-before) closer)))
	     (progn
	       (unless (looking-back "^[ \t]+")
		 (newline-and-indent))
	       (insert closer))
	   (insert gen-command-separator-char))
	 (setq done t))
	((and (eq closer ?\)) (eq (char-before) ?\;))
	 (newline-and-indent)
	 (insert closer)
	 (setq done t))
	;; Semicolon inserted where it probably shouldn't be? #12
	;; ((and (eq closer ?\)) (eq (char-before) ?\)))
	;;  (insert gen-command-separator-char)
	;;  (setq done t))
	(closer
	 (insert closer)
	 (setq done t))
	((not (eq (char-before) gen-command-separator-char))
	 (insert gen-command-separator-char)
	 (setq done t))))

(defun general-close ()
  "Command will insert closing delimiter whichever needed. "
  (interactive "*")
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (when (looking-at (ignore-errors comment-start))
      (goto-char (match-end 0))
      ;; travel comments
      (while (and (setq pps (parse-partial-sexp (point-min) (point))) (nth 4 pps) (nth 8 pps))
	(goto-char (nth 8 pps))
	(skip-chars-backward " \t\r\n\f")))
    (let ((orig (point))
	  (gen-empty-line (and (looking-back "^[ \t]*")(eolp)))
	  ;; in string or list?
	  (closer (gc--fetch-delimiter-char-maybe pps))
	  done erg)
      (if (member major-mode gc--separator-modes)
	  (gc--handle-separator-modes)
	(setq done (gc--insert-delimiter-char-maybe orig closer)))
      ;; other delimiter?
      (unless done
	(cond
	 ((eq major-mode 'python-mode)
	  (gen-python-close)
	  (setq done t))
	 ((eq major-mode 'ruby-mode)
	  (gen-ruby-close)
	  (setq done t))))
      (when (and gen-electric-indent-p gen-empty-line)
	(indent-according-to-mode)))))

(provide 'general-close)
;;; general-close.el ends here
