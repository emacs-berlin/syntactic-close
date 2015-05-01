;;; general-close.el --- Insert closing delimiter

;; Authored by Emacs User Group Berlin

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

(require 'general-close-modes)

(defvar gen-verbose-p nil)

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
		  (gen-in-string-p-intern pps))))
      (unless erg
	(when (looking-at "\"\\|'")
	  (forward-char 1)
	  (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	  (when (nth 3 pps)
	    (setq erg (gen-in-string-p-intern pps)))))

    ;; (list (nth 8 pps) (char-before) (1+ (skip-chars-forward (char-to-string (char-before)))))
    (when (and gen-verbose-p (interactive-p)) (message "%s" erg))
    erg)))


(defun general-close-stack-based ()
  "Command will insert closing delimiter whichever needed.

Does not require parenthesis syntax WRT \"{[(\" "
  (interactive "*")
  (let (res stack done erg pps-list)
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
	      (t (skip-chars-backward "^\"{\(\[\]\)}")))))
    (insert res)))

(defun general-close ()
  "Command will insert closing delimiter whichever needed. "
  (interactive "*")
  (when (ignore-errors comment-start)
    ;; travel comments
    (while (and (setq pps-list (parse-partial-sexp (point-min) (point))) (nth 4 pps-list) (nth 8 pps-list))
      (goto-char (nth 8 pps-list))
      (skip-chars-backward " \t\r\n\f")))
  (let* (erg
         (pps-list (parse-partial-sexp (point-min) (point)))
	 (orig (point))
         res done)
    ;; in string precedes
    (cond ((nth 3 pps-list)
	   (setq erg (gen--in-string-p-intern pps-list))
	   (setq res (make-string (nth 2 erg)(nth 1 erg)))
	   (goto-char orig))
	  ((nth 1 pps-list)
	   (goto-char (nth 1 pps-list))
	   (setq res (gen--return-compliment-char (char-after)))
	   (goto-char orig))
	  ;; other delimiter?
	  ((eq major-mode 'python-mode)
	   (gen-python-close)
	   (setq done t))
	  ((eq major-mode 'ruby-mode)
	   (gen-ruby-close)
	   (setq done t))
	  )
    (if res
        (insert res)
      (unless done
	(newline)
	(message "%s"  "Nothing to insert here!")))))

(provide 'general-close)
;;; general-close.el ends here
