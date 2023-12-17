;;; syntactic-close-org-mode-tests.el --- org-mode tests  -*- lexical-binding: t -*-


;; Authored and maintained by
;; Andreas Röhler, <andreas.roehler@online.de>
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

;;; Commentary: A still naive implementation of a syntactic-close command

;;

;;; Code:

(ert-deftest syntactic-close--elisp-org-quote-OkuWXL ()
  (syntactic-close-test
      "#+BEGIN_QUOTE
asdf"
      'org-mode
    syntactic-close-verbose-p
      (syntactic-close)
    (should (looking-back "#\\+END_QUOTE" (line-beginning-position)))))

(ert-deftest syntactic-close--elisp-org-src-WseYLy ()
  (syntactic-close-test
      "#+BEGIN_SRC
asdf"
    'org-mode
    syntactic-close-verbose-p
    (syntactic-close)
    (should (looking-back "#\\+END_SRC" (line-beginning-position)))))

(provide 'syntactic-close-org-mode-tests)
;;; syntactic-close-org-mode-tests.el ends here
