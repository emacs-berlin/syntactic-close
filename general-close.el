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

;;; Commentary: M-x general-close RET: close any syntactic element.

;; With optional `general-close-electric-listify-p' set to `t':

;; ['a','b   ==> ['a','b'
;; ['a','b'  ==> ['a','b',

;; With `C-u'
;; ['a','b', ==> ['a','b']

;; An explicit M-x general-close RET will then revert the
;; timer-triggered auto-closed, allowing to continue with contents

;; Some valid Emacs Lisp suitable for testing
;; (setq foo (list "([{123}])"))

;; A first draft was published at emacs-devel list:
;; http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

;;; Code:

(require 'cl-lib)
(require 'sgml-mode)
(require 'comint)
;; (require 'haskell)
;; (require 'haskell-mode)
;; (require 'sh-script)
;; (require 'python-mode)
;; (require 'beg-end)
;; (require 'ar-subr)
;; (require 'thingatpt-utils-base)
;; (require 'ar-navigate)

(require 'general-close-modes)

;; Stuff prefixed "ar-" to be removed when modules are ready
(defcustom ar-empty-line-p-chars "^[ \t\r]*$"
  "ar-empty-line-p-chars"
  :type 'regexp
  :group 'convenience)

(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'ar-empty-line-p))
(defun ar-empty-line-p (&optional iact)
  "Returns t if cursor is at an empty line, nil otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at ar-empty-line-p-chars)))
    (looking-at ar-empty-line-p-chars)))

(defun ar-previous-line-empty-or-BOB-p ()
  (save-excursion
    (beginning-of-line)
    (or
     (bobp)
     (when (forward-line -1)
       (ar-empty-line-p)))))
;;

(defun general-close-electric (&optional arg)
  "Call `general-close-electric-listify-p' set to `t'. "
    (interactive "P*")
  (let ((general-close-electric-listify-p t))
    (general-close arg nil t)))

(defun general-close-intern (beg list-separator-char electric iact &optional force)
  (let* ((orig (point))
	 (pps (parse-partial-sexp beg (point)))
	 (verbose general-close-verbose-p)
	 done closer)
    ;; ((setq done (general-close--travel-comments-maybe pps)))
    (cond
     ((setq done (general-close--modes pps orig list-separator-char closer force electric)))
     ((setq done (general-close--others orig closer pps)))
     ((setq done (general-close--common beg pps))))
    (unless done
      (and general-close-electric-newline-p (not (general-close-empty-line-p))
	   (newline)
	   (indent-according-to-mode)))
    (or (< orig (point)) (and iact verbose (message "%s" "nil")))))

(defun general-close (&optional arg beg electric force)
  "Command will insert closing delimiter whichever needed.

Smart insert default values when `general-close-electric-listify-p' is non-nil.

With \\[universal-argument]: close everything at point. "
  (interactive "P*")
  (let ((beg (or beg (general-close--point-min)))
	(list-separator-char general-close-list-separator-char)
	(electric (or electric general-close-electric-listify-p))
	(iact (called-interactively-p 'any)))
    (pcase (prefix-numeric-value arg)
      (4 (general-close-intern beg list-separator-char electric iact t))
      (_ (general-close-intern beg list-separator-char electric iact force)))))

(require 'general-close-modes)

(provide 'general-close)
;;; general-close.el ends here
