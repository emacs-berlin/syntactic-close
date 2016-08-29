;;; general-close-modes.el --- mode-specific functions -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

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

(defvar general-close-python-keywords "\\<\\(ArithmeticError\\|AssertionError\\|AttributeError\\|BaseException\\|BufferError\\|BytesWarning\\|DeprecationWarning\\|EOFError\\|Ellipsis\\|EnvironmentError\\|Exception\\|False\\|FloatingPointError\\|FutureWarning\\|GeneratorExit\\|IOError\\|ImportError\\|ImportWarning\\|IndentationError\\|IndexError\\|KeyError\\|KeyboardInterrupt\\|LookupError\\|MemoryError\\|NameError\\|NoneNotImplementedError\\|NotImplemented\\|OSError\\|OverflowError\\|PendingDeprecationWarning\\|ReferenceError\\|RuntimeError\\|RuntimeWarning\\|StandardError\\|StopIteration\\|SyntaxError\\|SyntaxWarning\\|SystemError\\|SystemExit\\|TabError\\|True\\|TypeError\\|UnboundLocalError\\|UnicodeDecodeError\\|UnicodeEncodeError\\|UnicodeError\\|UnicodeTranslateError\\|UnicodeWarning\\|UserWarning\\|ValueError\\|Warning\\|ZeroDivisionError\\|__debug__\\|__import__\\|__name__\\|abs\\|all\\|and\\|any\\|apply\\|as\\|assert\\|basestring\\|bin\\|bool\\|break\\|buffer\\|bytearray\\|callable\\|chr\\|class\\|classmethod\\|cmp\\|coerce\\|compile\\|complex\\|continue\\|copyright\\|credits\\|def\\|del\\|delattr\\|dict\\|dir\\|divmod\\|elif\\|else\\|enumerate\\|eval\\|except\\|exec\\|execfile\\|exit\\|file\\|filter\\|float\\|for\\|format\\|from\\|getattr\\|global\\|globals\\|hasattr\\|hash\\|help\\|hex\\|id\\|if\\|import\\|in\\|input\\|int\\|intern\\|is\\|isinstance\\|issubclass\\|iter\\|lambda\\|len\\|license\\|list\\|locals\\|long\\|map\\|max\\|memoryview\\|min\\|next\\|not\\|object\\|oct\\|open\\|or\\|ord\\|pass\\|pow\\|print\\|property\\|quit\\|raise\\|range\\|raw_input\\|reduce\\|reload\\|repr\\|return\\|round\\|set\\|setattr\\|slice\\|sorted\\|staticmethod\\|str\\|sum\\|super\\|tuple\\|type\\|unichr\\|unicode\\|vars\\|while\\|with\\|xrange\\|yield\\|zip\\|\\)\\>"
  "Contents like py-font-lock-keyword")

(require 'sgml-mode)
(require 'comint)

(defvar general-close-comint-haskell-pre-right-arrow-re   "let [alpha][A-Za-z0-9_]+ +::")
;; (setq general-close-comint-haskell-pre-right-arrow-re   "let [alpha][A-Za-z0-9_]+ +::")
(defcustom general-close-comint-haskell-pre-right-arrow-re
  "let [alpha][A-Za-z0-9_]+ +::"
  "Insert \"=\" when looking back. "
  :type 'string
  :tag "general-close-comint-haskell-pre-right-arrow-re"
  :group 'general-close)

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
    (cond ((and force (eq (char-before) general-close-list-separator-char))
	   (delete-char -1)
	   (insert closer)
	   (setq done t))
	  ((member (char-before) (list ?' ?\"))
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
	     (unless general-close-electric-listify-p
	       (setq done t))))
	  ((eq (char-before) general-close-list-separator-char)
	   (if general-close-electric-listify-p
	       (progn
		 (save-excursion
		   (forward-char -1)
		   (setq closer (char-before)))
		 (insert closer))
	     (delete-char -1)
	     (insert closer))
	   (setq done t))
	  (t (insert closer)
	     (setq done t)))
    done))

;; Python
(defun general-close-python-close (closer pps force)
  "Might deliver equivalent to `py-dedent'"
  (interactive "*")
  (let (
	;; (closer closer)
	done)
    (unless closer (setq closer (general-close--fetch-delimiter-maybe pps)))
    (if closer
	(setq done (general-close-python-listclose closer force pps))
      (setq done
	    (let ((general-close-beginning-of-statement
		   (if (ignore-errors (functionp 'py-backward-statement))
		       'py-backward-statement
		     (lambda ()(beginning-of-line)(back-to-indentation))))
		  (general-close-beginning-of-block-re "[ 	]*\\_<\\(class\\|def\\|async def\\|async for\\|for\\|if\\|try\\|while\\|with\\|async with\\)\\_>[:( \n	]*")
		  done)
	      (cond ((and (not (char-equal ?: (char-before)))
			  (save-excursion
			    (funcall general-close-beginning-of-statement)
			    (looking-at general-close-beginning-of-block-re)))
		     (insert ":")
		     (setq done t))
		    (t (eolp)
		       (ignore-errors (newline-and-indent))
		       (setq done t)))
	      done))
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
  ;; general-close--fetch-delimiter-maybe
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

(defun general-close-haskell-close (beg &optional closer pps)
  (let ((closer (or closer
		    (and pps (general-close--fetch-delimiter-maybe pps))
		    (general-close--generic-fetch-delimiter-maybe)))
	done)
    (if closer
	(progn
	  (insert closer)
	  (setq done t))
      (cond ((setq done (general-close--right-arrow-maybe beg general-close-pre-right-arrow-re)))
	    ((setq done (general-close--insert-assignment-maybe (line-beginning-position) general-close-pre-assignment-re)))
	    ((setq done (general-close--insert-string-concat-op-maybe)))))
    done))

;; Php
(defun general-close--php-check (closer)
  (let ((orig (point))
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

(defun general-close--modes (beg pps orig &optional closer force)
  (let (done)
    (cond
     ((or (eq major-mode 'php-mode) (eq major-mode 'js-mode))
      (setq done (general-close--php-check closer)))
     ((eq major-mode 'python-mode)
      (setq done (general-close-python-close closer pps force)))
     ((eq major-mode 'ruby-mode)
      (setq done (general-close-ruby-close closer pps)))
     ((member major-mode general-close--ml-modes)
      (setq done (general-close-ml)))
     ((eq major-mode 'haskell-mode)
      (setq done (general-close-haskell-close beg closer pps)))
     (t (setq done (general-close--intern orig closer pps))))
    done))

(provide 'general-close-modes)
;;; general-close-modes.el ends here
