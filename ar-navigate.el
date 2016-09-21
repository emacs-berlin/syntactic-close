;;; ar-navigate.el --- Some basic navigation         -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Röhler

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

;;

;;; Code:

(defvar ar-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string. ")

(defvar ar-labelled-re "[ \\t]*:[[:graph:]]+"
  "When looking at label. ")
;; (setq ar-labelled-re "[ \\t]*:[[:graph:]]+")

(defvar ar-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "ar-expression assumes chars indicated possible composing a ar-expression, skip it. ")

(defvar ar-expression-skip-chars "^ (:=#\t\r\n\f"
  "ar-expression assumes chars indicated possible composing a ar-expression, skip it. ")

(defvar ar-expression-re "[^ =#\t\r\n\f]+"
  "ar-expression assumes chars indicated possible composing a ar-expression, when looking-at or -back. ")

(defvar ar-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "ar-expression assumes chars indicated probably will not compose a ar-expression. ")

(defvar ar-not-expression-chars " #\t\r\n\f"
  "ar-expression assumes chars indicated probably will not compose a ar-expression. ")

(defvar ar-partial-expression-backward-chars "^ =,\"'()[]{}:#\t\r\n\f"
  "ar-partial-expression assumes chars indicated possible composing a ar-partial-expression, skip it. ")

(defvar ar-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")

(defvar ar-operator-regexp "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\)[ \t]*"
  "Matches most of operators inclusive whitespaces around.

See also `ar-assignment-regexp' ")

(defvar ar-assignment-regexp "[ \t]*=[^=]"
  "Matches assignment operator inclusive whitespaces around.

See also `ar-operator-regexp' ")

(defvar ar-delimiter-regexp "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs. ")

(defvar ar-line-number-offset 0
  "When an exception occurs as a result of ar-execute-region, a
subsequent ar-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in ar-execute-region and used in ar--jump-to-exception.")

(defvar ar-match-paren-no-use-syntax-pps nil)

(defvar ar-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defvar ar-bol-forms-last-indent nil
  "For internal use. Stores indent from last ar-forward-of-FORM-bol command.
When this-command is ar-backward-FORM-bol, last-command's indent will be considered in order to jump onto right beginning position.")

(defvar ar-XXX-tag-face 'ar-XXX-tag-face)

(defvar ar-pseudo-keyword-face 'ar-pseudo-keyword-face)

(defvar ar-variable-name-face 'ar-variable-name-face)

(defvar ar-number-face 'ar-number-face)

(defvar ar-decorators-face 'ar-decorators-face)

(defvar ar-object-reference-face 'ar-object-reference-face)

(defvar ar-builtins-face 'ar-builtins-face)

(defvar ar-class-name-face 'ar-class-name-face)

(defvar ar-exception-name-face 'ar-exception-name-face)

(defvar ar-import-from-face 'ar-import-from-face)

(defvar ar-def-class-face 'ar-def-class-face)

(defvar ar-try-if-face 'ar-try-if-face)


(require 'ar-subr)

(defconst ar-beginning-of-defun-re
  (concat
   "[ \t]*\\("
   (mapconcat 'identity
              (list
	       "(defgroup"
	       "(defconst"
	       "(defcustom"
	       "(defface"
	       "(define-.+-mode"
	       "(defmacro"
	       "(defsubst"
	       "(deftheme"
	       "(defun"
	       "(defvar"
	       "(ert-deftest"
	       )

              "\\|")
   "\\)")
  "Regular expression matching beginning of defun. ")

(defun ar-beginning-of-defun (&optional arg done)
  "Move to the beginning of a function definition.

When `beginning-of-defun-function' is set, call with optional ARG "
  (interactive "P")
  (let ((done done)
	(pps (parse-partial-sexp (point-min) (point))))
    (unless (bobp)
      (if beginning-of-defun-function
	  (funcall beginning-of-defun-function arg done)
	(when (nth 4 pps) (ar-backward-comment))
	(let ((liststart (nth 1 pps)))
	  (if liststart
	      (progn
		(goto-char liststart)
		(while (and (not (looking-at ar-beginning-of-defun-re))(setq liststart (nth 1 (parse-partial-sexp (point-min) (point)))))
		  (goto-char liststart)))
	    (unless done
	      (when (or (eq ?\)(char-before)) (< 0 (abs (skip-chars-backward "^)"))))
		(setq done t)
		(forward-char -1)
		(ar-beginning-of-defun arg done)))))))))

(defun ar-end-of-defun (&optional arg move)
  "Move to the end of a function definition.

Return position if successful, nil otherwise
When `end-of-defun-function' is set, call it with optional ARG "
  (interactive "P")
  (unless (eobp)
    (skip-chars-forward " \t\r\n\f") 
    (let* ((pps (parse-partial-sexp (point-min) (point)))
	  (nesting (nth 0 pps))
	  (in-comment (or (nth 4 pps) (looking-at comment-start)))
	  ;; clear emacs-lisps setting
	  (end-of-defun-function (if (eq major-mode 'emacs-lisp-mode)
				     nil
				   end-of-defun-function))
	  erg)
      (when in-comment
	(ar-forward-comment))
      (if end-of-defun-function
	  (funcall end-of-defun-function arg)
	(when move (skip-syntax-forward "^\\s("))
	(cond
	 ((eq 4 (car (syntax-after (point))))
	  (forward-sexp)
	  (if (< 0 (nth 0 (parse-partial-sexp (point-min) (point))))
	      (progn
		(ar-beginning-of-defun)
		(ar-end-of-defun))
	    (setq erg (point))))
	 ((< 0 nesting)
	  (ar-beginning-of-defun)
	  (ar-end-of-defun))
	 (t (ar-end-of-defun arg 'move)))))))

(defun ar-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position. "
  (interactive)
  ;; (when ar-debug-p (message "(current-buffer): %s" (current-buffer)))
  ;; (when ar-debug-p (message "major-mode): %s" major-mode))
  (let ((orig (point))
	(beginning-of-string-position (or beginning-of-string-position (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
                                          (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")(match-beginning 0))))
        erg)
    (if beginning-of-string-position
        (progn
          (goto-char beginning-of-string-position)
	  (when
	      ;; work around parse-partial-sexp error
	      (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
	    (goto-char (nth 3 (parse-partial-sexp 1 (point)))))
          (if (ignore-errors (setq erg (scan-sexps (point) 1)))
			      (goto-char erg)
	    (goto-char orig)))

      (error (concat "ar-end-of-string: don't see end-of-string at " (buffer-name (current-buffer)) "at pos " (point))))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;; Expression
(defun ar-backward-expression ()
  "Go to the beginning of a compound expression.

A a compound expression might be concatenated by \".\" operator, thus composed by minor expressions.

If already at the beginning or before a expression, go to next expression in buffer upwards"
  (interactive)
  (let (erg)
    (setq erg (ar--beginning-of-expression-intern))
    (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun ar--beginning-of-expression-intern (&optional orig)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (pps (syntax-ppss))
          erg)
      (cond
       ( ;; (empty-line-p)
        (eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
             (eq 9 (char-after))(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (ar--beginning-of-expression-intern orig))
       ;; lists
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (skip-chars-backward ar-expression-skip-chars))
       ((and (nth 3 pps)(nth 8 pps)
             (goto-char (nth 8 pps)))
        (cond (;; consider expression a string starting at BOL
               (bolp))
              ((looking-back ar-assignment-regexp))
              ((looking-back ar-operator-regexp)
               (when (nth 2 pps)
                 (goto-char (nth 2 pps))))
              (t (ar--beginning-of-expression-intern orig))))
       ;; comments left
       ((nth 8 pps)
        (goto-char (nth 8 pps))
        (unless (bobp)
          (ar--beginning-of-expression-intern orig)))
       ;; concatenated strings
       ((looking-back (concat ar-string-delim-re ar-expression-re ar-string-delim-re ar-operator-regexp ar-string-delim-re ar-expression-re ar-string-delim-re))
        (goto-char (match-beginning 0))
        (while (looking-back (concat ar-string-delim-re ar-expression-re ar-string-delim-re ar-operator-regexp) (line-beginning-position) t)
          (goto-char (match-beginning 0)))
        (skip-chars-backward ar-expression-skip-chars))
       ;; before comment
       ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (ar--beginning-of-expression-intern orig)))
       ((and (< (point) orig)(looking-at (concat ar-expression-re ar-delimiter-regexp))))
       ((looking-back (concat "[^ \t\n\r\f]+" ar-delimiter-regexp))
        (goto-char (match-beginning 0))
	(skip-chars-backward ar-expression-skip-chars)
        (unless (or (looking-back ar-assignment-regexp) (looking-back "^[ \t]*"))
          (ar--beginning-of-expression-intern orig)))
       ;; before assignment
       ((looking-back ar-assignment-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (ar--beginning-of-expression-intern orig))
       ((looking-back ar-operator-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (unless (< 0 (abs (skip-chars-backward ar-expression-skip-chars)))
          (ar--beginning-of-expression-intern orig)))
       ((looking-back "\"\\|'")
        (forward-char -1)
        (skip-chars-backward "\"'")
        (unless (looking-back ar-assignment-regexp)
          (ar--beginning-of-expression-intern orig)))
       ((looking-back "(\\|\\[")
        (forward-char -1)
        (unless (looking-back ar-assignment-regexp)
          (ar--beginning-of-expression-intern orig)))
       ((looking-back "[\])}]")
        (forward-char -1)
        (unless (looking-back ar-assignment-regexp)
          (ar--beginning-of-expression-intern orig)))
       ;; inside expression
       ((looking-back ar-expression-re)
        (skip-chars-backward ar-expression-skip-chars)
        (unless (or (looking-back "^[ \t]*") (looking-back ar-assignment-regexp))
          (ar--beginning-of-expression-intern orig)))
       ((looking-back (concat "[ \t]*" "[[:alnum:]_]*" ar-operator-regexp "[[:alnum:]_]*") (line-beginning-position) t)
        (goto-char (match-beginning 0))
        (unless (looking-back "^[ \t]*")
          (ar--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (looking-back "[ \t\r\n\f]"))
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (ar--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (not (bobp)) (looking-back ar-expression-re))
        (forward-char -1)
        (when (< 0 (abs (skip-chars-backward ar-expression-skip-chars)))
          (ar--beginning-of-expression-intern orig)))
       ((and (looking-at ar-expression-re) (not (looking-back "[ \t\r\n\f]")))
        (unless (< 0 (abs (skip-chars-backward ar-expression-skip-chars)))
          (ar--beginning-of-expression-intern orig)))
       ((and (eq (point) orig)(looking-back "[ \t]*="))
        (goto-char (match-beginning 0))
        (skip-chars-backward " \t\r\n\f")
        (ar--beginning-of-expression-intern orig)))
      (unless (or (eq (point) orig)(looking-at "[ \t]*#"))
        (setq erg (point)))
      erg)))

(defun ar-forward-of-expression (&optional arg)
  "Go to the end of a compound expression.

With numeric ARG do it that many times.

A a compound expression might be concatenated by \".\" operator, thus composed by minor expressions.

Operators however are left aside resp. limit ar-expression designed for edit-purposes. "
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (while (< 0 arg)
            (setq erg (ar--end-of-expression-intern))
            (setq arg (1- arg))))
      (setq arg (abs arg))
      (setq erg (ar-backward-expression arg)))
    (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun ar--end-of-expression-intern (&optional orig)
  (unless (eobp)
    (let* ((orig (or orig (point)))
           (pps (syntax-ppss))
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
      (cond
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (let ((parse-sexp-ignore-comments t))
          (forward-list))
        (unless (or (looking-at "[ \t]*$")(looking-at ar-assignment-regexp))
          (ar--end-of-expression-intern orig)))
       ;; in comment
       ((nth 4 pps)
        (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
        (ar--end-of-expression-intern orig))
       ( ;; (empty-line-p)
	(eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
	     (eq 9 (char-after))(not (eobp)))
          (forward-line 1))
        (ar--end-of-expression-intern orig))
       ((looking-at (concat ar-string-delim-re ar-expression-re ar-string-delim-re ar-operator-regexp ar-string-delim-re ar-expression-re ar-string-delim-re))
        (goto-char (match-end 0))
        (while (looking-at (concat ar-operator-regexp ar-string-delim-re ar-expression-re ar-string-delim-re))
          (goto-char (match-end 0))))
       ;; inside string
       ((ar-in-string-p)
        (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
          (goto-char (match-end 0)))
        (while
            (nth 3 (syntax-ppss))
          (forward-char 1))
        (unless (looking-at "[ \t]*$")
          (ar--end-of-expression-intern orig)))
       ((looking-at "[(\[]")
        (forward-list)
        (unless (looking-at "[ \t]*$")
          (ar--end-of-expression-intern orig)))
       ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
        (while (and (looking-at "[ \t]*#") (not (eobp)))
          (forward-line 1))
        (ar--end-of-expression-intern orig))
       ((and (eq orig (point)) (looking-at ar-assignment-regexp))
        (goto-char (match-end 0))
        (if (looking-at "[(\[]")
            (forward-list 1)
          (ar--end-of-expression-intern orig)))
       ((looking-at (concat "[^ \t\n\r\f]*" ar-delimiter-regexp))
        (goto-char (match-end 0))
        (while (looking-at (concat "[^ \t\n\r\f]*" ar-delimiter-regexp))
          (goto-char (match-end 0)))
        (forward-char -1)
        (unless (looking-at (concat ar-assignment-regexp "\\|[ \t]*$\\|" ar-delimiter-regexp))
          (ar--end-of-expression-intern orig)))
       ((looking-at (concat "\\([[:alnum:] ]+ \\)" ar-assignment-regexp))
	(goto-char (match-end 1))
	(skip-chars-backward " \t\r\n\f"))
       ((and (eq orig (point)) (looking-at (concat "[ \t]*" "[^(\t\n\r\f]+" ar-operator-regexp)))
	(skip-chars-forward " \t\r\n\f")
	(when (< 0 (skip-chars-forward ar-expression-skip-chars))
	  (ar--end-of-expression-intern orig)))
       ((and (eq orig (point)) (looking-at ar-not-expression-regexp))
        (skip-chars-forward ar-not-expression-chars)
        (unless (or (looking-at "[ \t]*$")(looking-at ar-assignment-regexp))
          (ar--end-of-expression-intern orig)))
       ((looking-at ar-expression-skip-regexp)
        (skip-chars-forward ar-expression-skip-chars)
        (unless (or (looking-at "[ \n\t\r\f]*$")(looking-at ar-assignment-regexp))
          (ar--end-of-expression-intern orig)))
       ((and (eq (point) orig)
	     (skip-chars-forward " \t\r\n\f")
	     (< 0 (skip-chars-forward ar-expression-skip-chars)))
	(ar--end-of-expression-intern orig)))

      (unless (or (eq (point) orig)(and (eobp)(bolp)))
        (setq erg (point)))
      erg)))

(defun ar-backward-partial-expression (&optional orig)
  (interactive)
  (let ((orig (point))
	erg)
    (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))(not (bobp))(forward-char -1))
    (when (ar--in-comment-p)
      (ar-backward-comment)
      (skip-chars-backward " \t\r\n\f"))
    ;; part of ar-partial-expression-forward-chars
    (when (member (char-after) (list ?\ ?\" ?' ?\) ?} ?\] ?: ?#))
      (forward-char -1))
    (skip-chars-backward ar-partial-expression-forward-chars)
    (when (< 0 (abs (skip-chars-backward ar-partial-expression-backward-chars)))
      (while (and (not (bobp)) (ar--in-comment-p)(< 0 (abs (skip-chars-backward ar-partial-expression-backward-chars))))))
    (when (< (point) orig)
      (unless
	  (and (bobp) (member (char-after) (list ?\ ?\t ?\r ?\n ?\f)))
	(setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-forward-of-partial-expression (&optional orig)
  (interactive)
  (let (erg)
    (skip-chars-forward ar-partial-expression-backward-chars)
    ;; group arg
    (and
     (looking-at "[\[{(]")
     (goto-char (scan-sexps (point) 1)))
    (setq erg (point))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar--beginning-of-expression-p (orig pps)
  "Returns position, if cursor is at the beginning of a `expression', nil otherwise. "
  (let ((orig (point))
        erg)
    (or (and pps (setq erg (eq 0 (nth 0 pps))))
	(save-excursion
	  (unless (and (eolp)(bolp))
	    (ar-forward-statement)
	    (ar-backward-statement))
	  (when (eq orig (point))
	    (setq erg orig))
	  erg))))

(defun ar--end-of-expression-p ()
  "Returns position, if cursor is at the end of a expression, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (ar-backward-statement)
      (ar-forward-statement)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun ar-backward-statement (&optional orig done limit)
  "Go to the initial line of a simple expression.
For beginning of compound expression use ar-backward-block.
For beginning of clause ar-backward-clause. "
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (this (point))
             (cui (current-indentation))
             (pps (progn (goto-char this)
                         (parse-partial-sexp (or limit (point-min))(point))))
             (done done)
             erg)
        (unless done
          (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
               (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (ar-backward-statement orig done limit))
         ((nth 8 pps)
          ;; inside string
          (and (nth 3 pps) (setq done t))
          (goto-char (nth 8 pps))
          (ar-backward-statement orig done limit))
         ((ar--preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (ar-backward-statement orig done limit))
         ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
          (forward-comment -1)
          (while (and (not (bobp))
                      (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (forward-comment -1))
          (unless (bobp)
            (ar-backward-statement orig done limit)))
         ;; at inline comment
         ((looking-at "[ \t]*#")
          (when (ar--skip-to-semicolon-backward
                 (save-excursion (back-to-indentation)(point)))
            (skip-chars-forward " \t")
            (unless (bobp)
              (ar-backward-statement orig done limit))))
         ;; at beginning of string
         ((and (not done) (looking-at ar-literal-delim-re))
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (ar-backward-statement orig done limit))
         ;; after end of expression
         ((and (not done) (eq (char-before) ?\;))
          (skip-chars-backward ";")
          (ar-backward-statement orig done limit))
         ;; at current indent
         ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (ar-backward-statement orig done limit))
         ((and (member (char-after) (list ?\" ?\'))
               (progn (back-to-indentation) (eq ?@ (char-after))))
          (back-to-indentation)
	  (when (< (point) orig) (setq done t))
          (ar-backward-statement orig done limit))
	 ;; ((eq 5 (car (syntax-after (1- (point)))))
	 ;;  (backward-sexp))
	 ((eq orig (point))
	  (back-to-indentation)
	  (when (< (point) orig)(setq done t))
	  (ar-backward-statement orig done limit))
	 ;; BOL considered sufficient
	 ;; ((and (not done) (nth 1 pps))
         ;;  (goto-char (nth 1 pps))
         ;;  (ar--skip-to-semicolon-backward
         ;;   (save-excursion (back-to-indentation)(point)))
         ;;  (setq done t)
         ;;  (ar-backward-statement orig done limit))
	 )
        ;; return nil when before comment
	(unless (eq (current-indentation)  (current-column))
	  (back-to-indentation)
	  (setq done t)
	  (ar-backward-statement orig done limit))
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (when (< (point) orig)(setq erg (point))))
        (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defun ar-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

Optional argument REPEAT, the number of loops done already,
is checked for ar-max-specpdl-size error.
Avoid eternal loops due to missing string delimters etc. "
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          erg pos last
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm pps err)
      ;;      (unless done (ar--skip-to-comment))
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (ar-count-lines)))
      (cond
       ((< ar-max-specpdl-size repeat)
        (error "ar-forward-statement reached loops max.
If no error, customize `ar-max-specpdl-size'"))
       ;; string
       ((or (nth 3 pps)(eq (char-syntax (char-after)) 34))
        (when (ar-end-of-string)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f")
          (setq pps (parse-partial-sexp (point-min) (point)))
          (unless (and done
                       (not (or (nth 1 pps) (nth 8 pps)))
                       (eolp))
            (ar-forward-statement orig done repeat))))
       ;; in comment
       ((or (nth 4 pps)(eq (char-syntax (char-after)) ?<))
	(ar-forward-comment)
        (ar-forward-statement orig done repeat))
       ((ar--current-line-backslashed-p)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (while (and (eq (char-before (point)) ?\\ )
                    (ar--escaped))
          (forward-line 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
        (unless (eobp)
          (ar-forward-statement orig done repeat)))
       ((eq orig (point))
        (or (and
	     (< 0 (abs (skip-chars-forward (concat " \t\r\n\f'\"" comment-start))))
	     (setq done t))
	    (end-of-line)
	    (skip-chars-backward " \t\r\n\f"))
        (ar-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(end-of-line)
	(skip-chars-backward " \t\r\n\f")
	(setq done t)
	(ar-forward-statement orig done repeat))
       ;; list
       ((nth 1 pps)
	(unless done
	  (goto-char (nth 1 pps))
	  (ignore-errors (forward-sexp))
	  (setq done t)
	  (ar-forward-statement orig done repeat))))
      (unless
	  (or
	   (eq (point) orig)
	   (member (char-before) (list 10 32 9)))
	(setq erg (point)))
      (if (and ar-verbose-p err)
	  (ar--message-error err)
	(and ar-verbose-p (interactive-p) (message "%s" erg)))
      erg)))

(defun ar-forward-expression (&optional orig done repeat pps)
  "Go to the end of a compound expression.

Operators are ignored. "
  (interactive)
  (unless done (skip-chars-forward " \t\r\n\f"))
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (pps (or pps (parse-partial-sexp (point-min) (point))))
          (orig (or orig (point)))
          erg)
      (if (< ar-max-specpdl-size repeat)
	  (error "`ar-forward-expression' reached loops max.")
	(cond
	 ;; in comment
	 ((nth 4 pps)
	  (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
	  (ar-forward-expression orig done repeat))
	 ;; empty before comment
	 ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
	  (while (and (looking-at "[ \t]*#") (not (eobp)))
	    (forward-line 1))
	  (ar-forward-expression orig done repeat))
	 ;; inside string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ((looking-at "\"\"\"\\|'''\\|\"\\|'")
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ;; looking at opening delimiter
	 ((eq 4 (car-safe (syntax-after (point))))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ((and (eq orig (point)) (looking-at ar-operator-regexp))
	  (goto-char (match-end 0))
	  (ar-forward-expression orig done repeat))
	 ((and (not done)
	       (< 0 (skip-chars-forward ar-expression-skip-chars)))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ;; at colon following arglist
	 ((looking-at ":[ \t]*$")
	  (forward-char 1)))
	(unless (or (eq (point) orig)(and (eobp)(bolp)))
	  (setq erg (point)))
	(when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun ar-forward-expression-bol ()
  "Go to the beginning-of-line following current expression."
  (interactive)
  (let ((erg (ar-forward-expression)))
    (setq erg (ar--beginning-of-line-form))
    (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun ar-down-expression ()
  "Go to the beginning of next expression downwards in buffer.

Return position if expression found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         (erg
          (cond ((ar--end-of-expression-p)
                 (and (ar-forward-expression) (ar-backward-expression)))
                ((ignore-errors (< orig (progn (ar-forward-expression) (ar-backward-expression))))
                 (point))
                (t (goto-char orig) (and (ar-forward-expression) (ar-forward-expression)(ar-backward-expression))))))
    (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun ar--backward-top-level-funktion (funktion &optional regexp)
  "Optional REGEXP in sh-mode would match \"fi\" for example. "
  (let (erg)
    (while (and (not (bobp))
		(setq erg (funcall funktion regexp))
		(or
		 (< 0 (current-column))
		 (looking-at regexp)
		 (ignore-errors (looking-at comment-start)))))
    erg))

(defun ar-backward-top-level (&optional funktion regexp)
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise
Optional REGEXP in sh-mode would match \"fi\" for example. "
  (interactive)
  (let (erg)
    (unless (bobp)
      (if funktion
	  (ar--backward-top-level-funktion funktion regexp)
	(while (and (not (bobp))
		    (setq erg (ar-backward-expression))
		    (or
		     (< 0 (current-column))
		     (and regexp (looking-at regexp))
		     (ignore-errors (looking-at comment-start))))))
      (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun ar--forward-top-level-intern (orig pps)
  (let (last)
    (unless (ar--beginning-of-expression-p orig pps)
      (ar-backward-expression))
    (unless (eq 0 (current-column))
      (ar-backward-top-level))
    (unless (< orig (point))
      (while (and
	      (not (eobp))
	      (save-excursion
		(ar-forward-expression orig nil nil pps)
		(setq last (point)))
	      (ar-down-expression)(< 0 (current-indentation)))))
    ;; (if (looking-at (ar-rx builtin-declaration))
    ;; (ar-forward-top-level)
    (and last (goto-char last))
    ;; (ar-forward-expression)
    ;;)
    ))

(defun ar-forward-top-level ()
  "Go to end of a top-level form.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
	(pps (parse-partial-sexp (point-min) (point))) 
        erg)
    (unless (eobp)
      (if (and (ar--forward-top-level-intern orig pps)
	       (< orig (point)))
	  (setq erg (point))
	(ar-down-expression)
	(ar-forward-top-level)))
    (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun ar-forward-top-level-bol ()
  "Go to beginning of line after end of a top-level form.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg last)
    (unless (eobp)
      (when (ar--forward-top-level-intern orig pps)
	(if (eobp)
	    (newline)
	  (forward-line 1)
	  (beginning-of-line)))
      (when (< orig (point))
	(setq erg (point))))
    (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun ar--go-to-keyword (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent
         (if (empty-line-p)
             (progn
               (ar-backward-statement)
               (current-indentation))
           (or maxindent (and (< 0 (current-indentation))(current-indentation))
               ;; make maxindent large enough if not set
               (* 99 ar-indent-offset))))
        (first t)
        done erg cui)
    (while (and (not done) (not (bobp)))
      (while (and (re-search-backward regexp nil 'move 1)(nth 8 (syntax-ppss))))
      ;; (or (< (point) orig) (ar-backward-statement))
      (if (and (looking-at regexp)(if maxindent
                                      (<= (current-indentation) maxindent) t))
          (progn
            (setq erg (point))
            (setq done t))
        (when (and first (not maxindent))
          (setq maxindent (current-indentation))
          (setq first nil))))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun ar--beginning-of-statement-p ()
  "Returns position, if cursor is at the beginning of a `statement', nil otherwise. "
  (save-excursion
    (let ((orig (point))
	  erg)
      (unless (and (eolp) (not (empty-line-p)))
	(ar-forward-statement))
      (ar-backward-statement)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun ar--beginning-of-form-intern (regexp &optional iact indent orig lc)
  "Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise"
  (interactive "P")
  (let (erg)
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (indent (or indent (progn
                                  (back-to-indentation)
                                  (or (ar--beginning-of-statement-p)
                                      (ar-backward-statement))
                                  (current-indentation)))))
        (setq erg (cond ((and (< (point) orig) (looking-at (symbol-value regexp)))
                         (point))
                        ((and (eq 0 (current-column)) (numberp indent) (< 0 indent))
                         (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
                           (ar-backward-statement)
                           (unless (looking-at (symbol-value regexp))
                             (cdr (ar--go-to-keyword (symbol-value regexp) (current-indentation))))))
                        ((numberp indent)
			 (cdr (ar--go-to-keyword (symbol-value regexp) indent)))
                        (t (ignore-errors
                             (cdr (ar--go-to-keyword (symbol-value regexp)
                                                    (- (progn (if (ar--beginning-of-statement-p) (current-indentation) (save-excursion (ar-backward-statement) (current-indentation)))) ar-indent-offset)))))))
        (when lc (beginning-of-line) (setq erg (point)))))
    (when (and ar-verbose-p iact) (message "%s" erg))
    erg))

(defun ar--beginning-of-prepare (indent final-re &optional inter-re iact lc)
  (let ((orig (point))
        (indent
         (or indent
             (progn (back-to-indentation)
                    (or (ar--beginning-of-statement-p)
                        (ar-backward-statement))
                    (cond ((eq 0 (current-indentation))
                           (current-indentation))
                          ((looking-at (symbol-value inter-re))
                           (current-indentation))
                          (t
                           (if (<= ar-indent-offset (current-indentation))
                               (- (current-indentation) (if ar-smart-indentation (ar-guess-indent-offset) ar-indent-offset))
                             ar-indent-offset))))))
        erg)
    (if (and (< (point) orig) (looking-at (symbol-value final-re)))
        (progn
          (and lc (beginning-of-line))
          (setq erg (point))
          (when (and ar-verbose-p iact) (message "%s" erg))
          erg)
      (ar--beginning-of-form-intern final-re iact indent orig lc))))

(defun ar-forward-block (&optional indent)
  "Go to end of block.

Returns end of block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (ar--end-base 'ar-block-re orig)))
    (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'ar-down-block-lc 'ar-forward-block-bol)
(defun ar-forward-block-bol ()
  "Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `ar-down-block': down from current definition to next beginning of block below. "
  (interactive)
  (let ((erg (ar-forward-block)))
    (setq erg (ar--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-backward-block (&optional indent)
 "Go to beginning block, skip whitespace at BOL.

Returns beginning of block if successful, nil otherwise"
  (interactive)
  (ar--beginning-of-prepare indent 'ar-block-re 'ar-clause-re (interactive-p)))

(defun ar-backward-indent ()
  "Go backward same or deeper levels of indentation. "
  (interactive)
  (let ((orig (point))
	(indent (progn (back-to-indentation) (current-column)))
	(last (point)))
    (while (and (not (bobp))(forward-line -1) (progn (back-to-indentation) (<= indent (current-indentation))))
      (setq last (point)))
    (goto-char last)))

(defun ar-forward-indent ()
  "Go backward same or deeper levels of indentation. "
  (interactive)
  (let ((indent (progn (back-to-indentation) (current-column)))
	last)
    (while
	(and (not (eobp)) (forward-line 1) (progn (back-to-indentation) (<= indent (current-indentation))))
      (setq last (line-end-position)))
    (goto-char last)))

(defun ar-mark-indent-level ()
  "Mark lines around point where indentation is equal or deeper. "
  (interactive)
  (let ((orig (point)))
    (ar-backward-indent)
    (push-mark (point) t)
    (goto-char orig)
    (ar-forward-indent)))

(provide 'ar-navigate)
;;; ar-navigate.el ends here
