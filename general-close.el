;;; general-close.el --- Insert closing delimiter

;; Copyright (C) 2015  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>

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

;;; Commentary: A still naive implementation of a general-close command

;;

;;; Code:

;; Some valid Emacs Lisp suitable for testing
;; (setq foo (list "([{123}])"))

(ert-deftest gen-close-test ()
  (with-temp-buffer
    (insert "(list \"([{123")
    ;; (switch-to-buffer (current-buffer))
    (general-close)
    (should (eq (char-before) ?}))
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\"))))

(defun gen--return-compliment-char (erg)
  (cond ((eq erg ?\")
	 erg)
	((eq erg ?\[)
	 ?\])
	((eq erg ?\{)
	 ?\})
	((eq erg ?\()
	 ?\))))

(defun general-close ()
  "Command will insert closing delimiter whichever needed. " 
  (interactive "*")
  (let (res stack done)
    (save-excursion
      (while (and (not (bobp)) (not done))
	(cond ((member (char-before) (list ?\) ?\] ?}))
	       (push (char-before) stack)
	       (forward-char -1))
	      ((member (char-before) (list ?\( ?\" ?{ ?\[))
	       (setq res (gen--return-compliment-char (char-before)))
	       (if (eq (car stack) res)
		   (progn
		     (pop stack)
		     (forward-char -1))
		 (setq done t)))
	      (t (skip-chars-backward "^\"{\(\[\]\)}")) )))
    (insert res)))

(provide 'general-close)
;;; general-close.el ends here
