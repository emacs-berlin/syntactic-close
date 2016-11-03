;;; beg-end.el

;; Copyright (C) 2009-2016  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Keywords: lisp, languages

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

;; Routine detecting nested forms - for all kind of
;; things which have a start- and a end-string.
;; End-string might be the empty string.  Jump to the
;; beginning or end of a form.  Repeated execution
;; forward or backward.  With argument as many times.

;; FixMe: `ar-in-comment-p-atpt' uses
;; thingatpt-utils-base while required from

;;; Code:

;; (require 'ar-subr)

(defcustom thing-inside-comments nil
  "If text inside comments matches determining the border of a THING. "
  :type 'boolean
  :group 'convenience)

(defun ar-toggle-thing-inside-comments ()
  "If thing-at-point forms should match inside comments.

Toggles value of `thing-inside-comments'. Default is nil "
  (interactive)
  (setq thing-inside-comments (not thing-inside-comments)))

(defun mark-form (begstr endstr &optional bound noerror count permit-comment)
  (beginning-of-form-base begstr endstr bound noerror count permit-comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count permit-comment)
  (kill-new (buffer-substring-no-properties (mark) (point))))

(defun kill-form (begstr endstr &optional bound noerror count permit-comment)
  (beginning-of-form-base begstr endstr bound noerror count permit-comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count permit-comment)
  (kill-region (mark) (point)))

(defun beg-end-match-paren-base (begstr endstr &optional iact)
  "Switch between opening and closing programming constructs.
If not at the beginning or the end, to opening first. "
  (cond ((ignore-errors (looking-at begstr))
	 (end-of-form))
	((progn (backward-word)
		(ignore-errors (looking-at endstr)))
	 (beginning-of-form)
	 (when iact (message "%s" (point))) (point))
	(t (beginning-of-form)
	   (when iact (message "%s" (point))) (point))))

(defun beginning-of-form (&optional bound noerror count permit-comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
 Don't use this from a program, use `beginning-of-form-base' instead. "
  (interactive)
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (beginning-of-form-base begstr endstr bound noerror count permit-comment))
  (when (interactive-p) (message "%s" (point))) (point))

(defun end-of-form (&optional iact bound noerror count permit-comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
 Don't use this from a program, use `end-of-form-base' instead. "
  (interactive "p")
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (end-of-form-base begstr endstr bound noerror count permit-comment))
  (when iact (message "%s" (point))) (point))

(defun ar-leave-begstr-backward (begstr unquoted-beg)
  (let* ((stringcount (length unquoted-beg))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-beg)))
    (while (and indx (not (ignore-errors (looking-at begstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-beg))
      (setq stringcount (1- stringcount)))))

(defun ar-leave-endstr-backward (endstr unquoted-end)
  (let* ((stringcount (length unquoted-end))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-end)))
    (while (and indx (not (ignore-errors (looking-at endstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-end))
      (setq stringcount (1- stringcount)))))

(defun ar-escaped ()
  "Return t if char is preceded by an odd number of backslashes. "
  (save-excursion (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defun ar-syntax (&optional arg)
  "Return t if char meant as syntax-symbol. "
  (interactive "p")
  (let ((orig (point))
        erg)
    (goto-char (match-beginning 0))
    (setq erg (looking-back "\\\\s"))
    (goto-char orig)
    (when arg (message "%s" erg))
    erg))

(defun beginning-of-form-base-intern (begstr endstr permit-comment permit-string)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    ;; in string not permitted, leave it
    (if (and (not permit-string) (nth 3 pps)(nth 8 pps))
        (goto-char (nth 8 pps))
      (unless (save-match-data (and condition (funcall condition)))
        (save-match-data
          (if
              (ignore-errors (string-match begstr (match-string-no-properties 0)))
              (if permit-comment
		  (progn
		    (setq first nil)
		    (setq nesting (1- nesting)))
                (unless (ar-in-comment-p)
		  (setq first nil)
                  (setq nesting (1- nesting))))
            (when
                (ignore-errors (string-match endstr (match-string-no-properties 0))))
            (if permit-comment
                ;; non-nesting comments don't matter
                (unless (or (string= "" comment-end)(eq 10 comment-end))
		  (if first
		      (setq first nil)
		    (setq nesting (1+ nesting))))
              (unless (ar-in-comment-p)
		(if first
		    (setq first nil)
		  (setq nesting (1+ nesting)))))))
        (setq beg-pos-delimiter (match-beginning 0))
        (setq end-pos-delimiter (match-end 0))))))

;; should `parse-sexp-ignore-comments' be usedq?
(defun beginning-of-form-base (begstr &optional endstr bound noerror nesting permit-comment regexp condition permit-string)
  "Assume being inside ary delimiters, go to start.

Bound limits search.
If NOERROR is non-nil, no error message is raised if not successful.
NESTING, a number, enables match of nested forms.
Set IN-COMMENT to `t' if forms inside comments should match - also for processing comments itself.
Set 7th argument REGEXP t, if beg/end-str are regular expressions.
CONDITION takes a function as argument perfoming the match.
If IN-STRING is non-nil, forms inside string match.
"
  (let* ((searchform (if (stringp begstr)
			 (cond ((and (string= begstr endstr))
				begstr)
			       ((and begstr endstr)
				(progn
				  (setq regexp t)
				  (concat begstr "\\|" endstr)))
			       (t begstr))
		       begstr))
         (nesting (or  nesting 0))
         (orig (point))
	 (first t)
         (permit-comment (or permit-comment thing-inside-comments))
         beg-pos-delimiter end-pos-delimiter )
    (while
        (and
         (or first (< 0 nesting)) (not (bobp)))
      (cond
       ((and (looking-back searchform)
             (goto-char (match-beginning 0)))
        (beginning-of-form-base-intern begstr endstr permit-comment permit-string))
       ((and (or regexp (and begstr endstr))
             (re-search-backward searchform bound noerror nesting))
        (beginning-of-form-base-intern begstr endstr permit-comment permit-string))
       ((and (not regexp) (not (and begstr endstr))
             (search-backward searchform bound noerror nesting)
             (goto-char (match-beginning 0)))
        (beginning-of-form-base-intern begstr endstr permit-comment permit-string))
       (t (goto-char (point-min)))))
    (when (and beg-pos-delimiter end-pos-delimiter)
      (list beg-pos-delimiter end-pos-delimiter))))

(defun end-of-form-base-intern (begstr endstr permit-comment permit-string)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    ;; in string
    (if (and (not permit-string) (nth 3 pps)(nth 8 pps))
        (progn
          (forward-char 1)
          (while (and (setq pps (parse-partial-sexp (point-min) (point)))(nth 3 pps)(nth 8 pps))
            (forward-char 1)))
      (unless (save-match-data
                (and condition (funcall condition)))
        (save-match-data
          (if
              (string-match endstr
                            (if regexp (match-string-no-properties 0)
                              (match-string-no-properties 0)))
              (if permit-comment
                  (setq nesting (1- nesting))
                (unless (ar-in-comment-p)
                  (setq nesting (1- nesting))))
            ;; got another beginning while moving down
            (if permit-comment
                (setq nesting (1+ nesting))
              (unless (ar-in-comment-p)
                (setq nesting (1+ nesting))))))
        (setq beg-pos-delimiter (match-beginning 0))
        (setq end-pos-delimiter (match-end 0))))))

(defun end-of-form-base (begstr endstr &optional bound noerror nesting permit-comment regexp condition permit-string)
  "Goto closing of a programming structure in this level.
As it stops one char after form, go one char back onto the last char of form.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
If SHOW, display nesting and point in message buffer.
Set 7th argument REGEXP t, if beg/end-strings are regular expressions.
Optional arg CONDITION expects a function whose return value - `t' or a number - is checked for success, otherwise search continues.
If IN-STRING is non-nil, forms inside string match.
"
  (let* ((searchform (cond ((string= begstr endstr)
                            endstr)
                           ((and begstr endstr)
                            (progn
                              (setq regexp t)
                              (concat begstr "\\|" endstr)))
                           (t endstr)))
         (nesting (or nesting 1))
         (orig (point))
         (permit-comment (or permit-comment thing-inside-comments))
         beg-pos-delimiter end-pos-delimiter)
    (while
        (and
         (< 0 nesting) (not (eobp)))
      (if (< 1 (length endstr))
	  (and (looking-at searchform)
	       (goto-char (match-end 0)))
	(and (string= (prin1-to-string (char-after)) endstr)
	     (forward-char 1)))

      ;; (end-of-form-base-intern begstr endstr permit-comment permit-string)
      (if
	  (or (and regexp (re-search-forward searchform bound noerror))
	      (search-forward searchform bound noerror))
	  (end-of-form-base-intern begstr endstr permit-comment permit-string)
	;; if search wasn't successful, reduce
        (setq nesting (1- nesting))))
    (if (and beg-pos-delimiter end-pos-delimiter)
        (list beg-pos-delimiter end-pos-delimiter)
      (goto-char orig)
      nil)))

(defvar match-paren-key-char "%")

(defvar be-match-paren-mode nil)

(defun be-match-paren-mode (&optional iact)
  "Toggle be-match-paren-mode.
If on, inserting of `be-match-paren-char', default is \"%\", moves to the matching opening/closing.
With arg, insert the charakter the key is on
Key per default is \"%\" as with elisp's `match-paren'. "
  (interactive "p")
  (if be-match-paren-mode
      (progn
        (setq be-match-paren-mode nil)
        ;;        (define-key be-mode-map "%" 'self-insert-command)
        (when iact (message "be-match-paren-mode: %s" be-match-paren-mode)))
    (setq be-match-paren-mode t)
    ;;    (define-key be-mode-map "%" 'be-match-paren)
    (when iact (message "be-match-paren-mode: %s" be-match-paren-mode))))

(defun beg-end-match-paren (begstr endstr &optional ins)
  "Go to the matching opening/closing.
First to opening, unless cursor is already there.
With arg, insert the charakter of `sh-match-paren-char'.
Key per default is \"%\" as with elisp's `match-paren'. "
  (if ins
      (insert match-paren-key-char)
    (cond ((ignore-errors (looking-at begstr))
           (end-of-form-base begstr endstr bound noerror count))
          ((ignore-errors (looking-at "\\s("))
           (match-paren arg))
          ((ignore-errors (looking-at "\\s)"))
           (match-paren arg))
          (t (beginning-of-form-base begstr endstr bound noerror count permit-comment)))))

;; Fixme: Instead use of condition `ar-in-comment-p-atpt' is hard-corded for the moment
(defun ar-in-delimiter-base (regexp &optional condition guess-delimiter)
  "REGEXP expected of an unary delimiter, for example
\"\\\\\\\"\\\\\\\"\\\\\\\"\\\\|'''\\\\|\\\\\\\"\\\\|'\" indicating string delimiters in Python.
Optional second arg --a number, nil or `t'-- if interactively called. "
  (let ((orig (point))
        (count 0)
        ;;        (regexp (replace-regexp-in-string "\"" "\\\\\""  regexp))
        tell beglist)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((delimiter (when
                              (looking-at regexp)
                            (setq count (1+ count))
                            (match-string-no-properties 0)))
               (delimiter-p (stringp delimiter)))
          (if delimiter-p
              (progn
                (setq delimiter (concat "\\([^\\]\\)" (replace-regexp-in-string "\"" "\\\\\""  delimiter)))
                (setq beglist (list (match-beginning 0) (match-end 0)))
                (goto-char (match-end 0))
                (ar-in-delimiter-intern count orig beglist delimiter-p delimiter))
            (setq regxep (concat "[^\\]" regexp))
            (ar-in-delimiter-intern count orig beglist nil regexp)))))))

(defun ar-in-delimiter-intern (count orig beglist &optional first old)
  (let (done name this pps)
    (while (and (not done)(< (point) orig))
      (while (and (re-search-forward regexp orig t 1)
		  (if (save-excursion
			(goto-char (match-beginning 0))
			(ar-escaped))
		      (progn
			(while
			    (and
			     (re-search-forward regexp orig t 1)
			     (save-excursion
			       (goto-char (match-beginning 0))
			       (ar-escaped))))
			t)
		    t)
		  (setq first (point))
		  (not thing-inside-comments)
		  (save-match-data
		    ;; (ar-in-comment-p-atpt)
		    (or (progn (setq pps (syntax-ppss)) (nth 4 pps)) (nth 7 pps)))))
      (if first
          (progn
            (setq count (1+ count))
            (setq beglist (list (match-beginning 0) (match-end 0)))
            (when (eq 1 (% count 2))
              (goto-char (match-beginning 0))
              (setq delimiter (concat "\\([^\\]\\)" (match-string-no-properties 0) "\\|\\(\\\\\\\\\\)" (match-string-no-properties 0)))
              (setq old delimiter)
              (while (and (setq this (re-search-forward delimiter orig t 1))
                          (not thing-inside-comments)
                          (save-match-data
                            ;; (ar-in-comment-p-atpt)
                            (or (progn (setq pps (syntax-ppss)) (nth 4 pps)) (nth 7 pps)))))
              (if this (setq count (1+ count))
                (setq done t)))
            (setq first nil))
        (setq done t)))
    (setq erg (and (< 0 count)(eq 1 (% count 2))))
    (when (and (< 0 count) erg)
      beglist)))

(defun ar--char-delimiters-beginning-push (char)
  "Changes stack. "
  (and (< 0 (skip-chars-forward (concat "^" (regexp-quote (char-to-string char))) orig))
       (char-equal char (char-after))
       (push (point) stack)))

(defun ar--char-delimiters-beginning-whitespaced-form (char)
  (cond ((eq (char-after) ?\ )
	 (point))
	((eq (char-before) ?\ )
	 (1- (point)))
	(t (and (< 0 (abs (skip-chars-backward (concat "^" (char-to-string char)))))
		(eq (char-before) ?\ )
		(1- (point))))))

(defun ar--char-delimiters-beginning-intern (char orig &optional escaped comment first)
  (if (eq char ?\ )
      (ar--char-delimiters-beginning-whitespaced-form char)
    (unless first (goto-char (point-min)))
    (let ((stack nil)
	  (push-p t)
	  pps)
      (when (char-equal char (char-after))
	(push (point) stack)
	(setq push-p nil)
	;; pusp-p stays nil first time
	(unless (eobp) (forward-char 1)))
      (cond ((and escaped comment)
	     (while
		 (ar--char-delimiters-beginning-push char)))
	    (escaped
	     (while
		 (and (ar--char-delimiters-beginning-push char)
		      (setq pps (parse-partial-sexp (point-min) (point)))
		      (nth 4 pps)
		      (pop stack))))
	    (comment
	     (while
		 (and
		  (ar--char-delimiters-beginning-push char)
		  (ar-escaped)
		  (pop stack))))
	    (t
	     (while
		 (and (not (eobp))(progn (skip-chars-forward (concat "^" (regexp-quote (char-to-string char))) orig)(<= (point) orig)))
	       (cond ((nth 4 (setq pps (parse-partial-sexp (point-min) (point)))))
		     ((and (nth 3 pps) (eq 7 (car (syntax-after (point)))))
		      (unless (or (eq (point) orig) (ar--escaped))
			(pop stack) (setq push-p t)))
		     ((eq (char-after) char)
		      (unless (ar--escaped)
			(if
			    push-p
			    (progn
			      (push (point) stack)
			      (setq push-p nil))
			  (unless (eq (point) orig) (pop stack))
			  (setq push-p t)))))
	       (unless (eobp) (forward-char 1)))))
      (when stack
	(pop stack)))))

(defun ar-char-delimiters-beginning (char &optional escaped comment first)
  "If ESCAPED, match also chars which are backslashed.

With COMMENT much also in comments
With FIRST don't check from BOB "
  (let* ((orig (point))
    	 (erg (ar--char-delimiters-beginning-intern char orig escaped comment)))
    (if erg
	(goto-char erg)
      (progn
	(goto-char orig)
	nil))))

(defun ar-char-delimiters-end-raw (&optional escaped comment)
  (let (erg)
    (cond ((and escaped comment)
	   (skip-chars-forward (concat "^" (regexp-quote (char-to-string char))))
	   (when (eq (char-after) char)
	     (setq erg (point))))
	  (comment
	   (while (and (< 0 (skip-chars-forward (concat "^" (regexp-quote (char-to-string char)))))
		       (setq erg (point))
		       (ar--escaped)
		       (setq erg nil))))
	  ;; ((eq orig (point))
	  ;;  (and (setq pps (parse-partial-sexp (point-min) (point)))
	  ;; 	(nth 3 pps)
	  ;; 	(unless (ar--escaped) (setq erg (point)))))
	  (t (while (and (not (eobp))
			 (< 0 (skip-chars-forward (concat "^" (regexp-quote (char-to-string char)))))
			 (setq pps (parse-partial-sexp (point-min) (point)))
			 (when (or (ar--escaped) (nth 4 pps))
			   (forward-char 1)
			   t)))
	     (when (eq (char-after) (setq erg (point))))))
    erg))

(defun ar-char-delimiters-end (char &optional escaped comment)
  "Expects to be called from delimiters start.

With COMMENT, match inside comments.
If ESCAPED, also match chars which are backslashed. "
  (let ((orig (point)))
    (ar-char-delimiters-end-raw escaped comment)
    (unless (eobp)
      (when (and (char-equal char (char-after))(< orig (point)))
	(unless (eobp) (forward-char 1))
	(point)))))

(provide 'beg-end)

;;; beg-end.el ends here
