;;; syntactic-close-html-tests.el --- html tests -*- lexical-binding: t; -*-

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
;; <!DOCTYPE html>
;; <html lang="en">
;; <head>
;;     {% block head %}
;;     <link rel="stylesheet" href="style.css" />
;;     <title>{% block title %}{% endblock %} - My Webpage</title>
;;     {% endblock %}
;; </head>
;; <body>
;;     <div id="content">{% block content %}{% endblock %}</div>
;;     <div id="footer">
;;         {% block footer %}
;;         &copy; Copyright 2008 by <a href="http://domain.invalid/">you</a>.
;;         {% endblock %}
;;     </div>
;; </body>
;; </html>

;; http://jinja.pocoo.org/docs/2.10/templates/#escaping

(ert-deftest syntactic-close-html-1-test ()
  (syntactic-close-test-with-html-buffer
      "<body>"
    (syntactic-close)
    (should (looking-back "</body>" (line-beginning-position))))) 

(ert-deftest syntactic-close-html-test-RiZF5w ()
  (syntactic-close-test
      "<body>"
    'html-mode
    t
    (syntactic-close)
    (should (looking-back "</body>" (line-beginning-position))))) 


(provide 'syntactic-close-html-tests)
;;; syntactic-close-html-tests.el ends here
