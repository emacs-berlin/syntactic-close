
;; (ert-deftest syntactic-close-haskell-concat-test ()
;;   ;; indent s = "    " ++ s
;;   (syntactic-close-test "indent s = \"asdf\""
;;     'haskell-mode
;;     'syntactic-close-debug-p
;;     (syntactic-close)
;;     (skip-chars-backward " \t\r\n\f")
;;     (should (eq (char-before) ?+))))

(provide 'syntactic-close-haskell-tests)
;;; syntactic-close-haskell-tests.el ends here
