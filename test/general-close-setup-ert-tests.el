;;; setup-ert-tests.el --- Provide needed forms

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Keywords: lisp

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

;;; Code:

(setq general-close-install-directory default-directory)
(sit-for 0.1 t)

(defvar general-close-debug-p nil
  "Avoid error")

(defvar general-close-verbose-p t)
;; (setq general-close-verbose-p t)

(defmacro general-close-test-with-temp-buffer (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test (contents mode verbose &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (funcall ,mode)
       (insert ,contents)
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-point-min (contents mode verbose &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
;;     (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (funcall ,mode)
       (insert ,contents)
       (goto-char (point-min))
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-python-buffer-point-min (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
;;     (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (python-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-python-buffer (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (python-mode)
       (insert ,contents)
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))


(defmacro general-close-test-with-php-buffer-point-min (contents &rest body)
  "Create temp buffer in `php-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
;;     (and (featurep 'php) (unload-feature 'php))
     (let (hs-minor-mode)
       (php-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-php-buffer (contents &rest body)
  "Create temp buffer in `php-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (php-mode)
       (insert ,contents)
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-ruby-buffer-point-min (contents &rest body)
  "Create temp buffer in `php-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (ruby-mode)
       (insert ,contents)
       ;; (message "ERT %s" (point))
       (goto-char (point-min))
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-ruby-buffer (contents &rest body)
  "Create temp buffer in `ruby-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (ruby-mode)
       (insert ,contents)
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-elisp-buffer (contents &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (emacs-lisp-mode)
       (insert ,contents)
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ;; (message "ERT %s" (point))
       ,@body)))

(defmacro general-close-test-with-js-buffer-point-min (contents &rest body)
  "Create temp buffer in `js-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
;;     (and (featurep 'js) (unload-feature 'js))
     (let (hs-minor-mode)
       (js-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(defmacro general-close-test-with-js-buffer (contents &rest body)
  "Create temp buffer in `js-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (js-mode)
       (insert ,contents)
       (when general-close-verbose-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
       ,@body)))

(provide 'setup-ert-tests)
;; setup-ert-tests.el ends here
