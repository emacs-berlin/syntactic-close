;;; general-close-shell-script-tests.el --- Tests in shell-script-mode -*- lexical-binding: t; -*-

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

;;

;;; Code:

(ert-deftest general-close-shell-script-paren-test-1 ()
  (general-close-test-with-shell-script-buffer
      "MULTIFORM=$(
    curl -k -A http://foo.com |
    grep -m1 multiform |
    tr '=' '\\n' |
    tail -1 |
    cut -d \"'\" -f 2"
    (general-close)
    (should (eq (char-before) ?\)))))


(provide 'general-close-shell-script-tests)

;;; general-close-shell-script-tests.el ends here
