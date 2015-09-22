;;; general-close-python-tests.el --- Python tests

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

(defvar general-close-python-test-string-1 "\"Some Doku")
(setq general-close-python-test-string-1 "\"Some Doku")

(defvar general-close-python-test-string-2 "{[(123")
(setq general-close-python-test-string-2 "{[(123")

(ert-deftest general-close-python-doublequoted-test ()
  (general-close-test-with-python-buffer
      general-close-python-test-string-1
    (general-close)
    (should (eq (char-before) ?\"))))

(ert-deftest general-close-python-singlequoted-test ()
  (general-close-test-with-python-buffer
      "'Some Doku"
    (general-close)
    (should (eq (char-before) ?'))))

(ert-deftest general-close-python-doublequoted-tqs-test ()
  (general-close-test-with-python-buffer
      "\"\"\"Some Doku"
    (font-lock-fontify-buffer)
    (general-close)
    (should (eq (char-before) ?\"))
    (should (eq -3 (skip-chars-backward "\"")))))

(ert-deftest general-close-python-singlequoted-tqs-test ()
  (general-close-test-with-python-buffer
      "'''Some Doku"
    (font-lock-fontify-buffer)
    (general-close)
    (should (eq (char-before) ?'))
    (should (eq -3 (skip-chars-backward "'")))))

(ert-deftest general-close-python-brace-paren-bracket-test ()
  (general-close-test-with-python-buffer
      general-close-python-test-string-2
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?}))
    (general-close)
    ;; (message "(current-indentation): %s" (current-indentation))
    ;; (message "(point): %s" (point))
    ;; (should (bolp))
    (should (eq 11 (point)))
    (should (eq 0 (current-indentation)))))

(ert-deftest general-close-delete-whitespace-backward-test ()
  (general-close-test-with-python-buffer
      "[1, 3] "
    (let ((general-close-delete-whitespace-backward-p t))
      (general-close)
      ;; (message "(point): %s" (point))
      ;; (message "(count-lines: %s" (count-lines (point-min) (point) ))
      ;; (should (bolp))
      ;; (should (eobp))
      (should (eq 8 (point)))
      (should (eq 0 (current-indentation)))
      )))

(ert-deftest general-close-python-nested-paren-test ()
  (general-close-test-with-python-buffer
      "(list ([\n# {123\n# {123\n"
    (general-close)
    (should (eq (char-before) ?\]))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest general-close-python-string-interpolation-test-1 ()
  (general-close-test-with-python-buffer "print('%(language"
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest general-close-python-string-interpolation-test-2 ()
  (general-close-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language"
    (general-close)
    (should (eq (char-before) ?'))))

(ert-deftest general-close-python-string-interpolation-test-3 ()
  (general-close-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language': \"Python"
    (general-close)
    (should (eq (char-before) ?\"))))

(ert-deftest general-close-python-string-interpolation-test-4 ()
  (general-close-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language': \"Python\", \"number\": 2"
    (general-close)
    (should (eq (char-before) ?}))))

(ert-deftest general-close-python-string-interpolation-test-5 ()
  (general-close-test-with-python-buffer "print('%(language)s has %(number)03d quote types.' %
      {'language': \"Python\", \"number\": 2}"
    (general-close)
    (should (eq (char-before) ?\)))))

(ert-deftest colon-after-arguments-list-test ()
  (general-close-test-with-python-buffer "def datei(datei, verzeichnis)"
    (general-close)
    (should (eq (char-before) ?:))))

(ert-deftest general-close-python-colon-test ()
  (general-close-test-with-python-buffer
      "#! /usr/bin/env python3
with open(verzeichnis + \"/\" + datei, \"w\") as ausgabe"
    (general-close)
    (should (eq (char-before) ?:))
    (general-close)
    (should (eq (char-before) 32))))

(ert-deftest general-close-python-colon-test-2 ()
  (general-close-test-with-python-buffer
      "class TutorialApp(App):
    def build(self):
        return Button(text=\"Hello!\",
                      background_color=(0, 0, 1, 1)
                      font_size=150)
if __name__ == \"__main__\""
    (switch-to-buffer (current-buffer))
    (font-lock-fontify-buffer)
    (general-close)
    (should (eq (char-before) ?:))
    (general-close)
    (should (eq 4 (current-indentation)))))

(provide 'general-close-python-tests)
;;; general-close-python-tests.el ends here
