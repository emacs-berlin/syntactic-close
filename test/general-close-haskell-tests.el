
;; (ert-deftest general-close-haskell-concat-test ()
;;   ;; indent s = "    " ++ s
;;   (general-close-test "indent s = \"asdf\""
;;     'haskell-mode
;;     'general-close-debug-p
;;     (general-close)
;;     (skip-chars-backward " \t\r\n\f")
;;     (should (eq (char-before) ?+))))

(ert-deftest general-close-haskell-typelist-1 ()
  (general-close-test-with-haskell-buffer
      "zip :: (["
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back general-close-default-argument-1)))))

(ert-deftest general-close-haskell-typelist-2 ()
  (general-close-test-with-haskell-buffer
      "zip :: ([x"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\])))))

(ert-deftest general-close-haskell-typelist-2 ()
  (general-close-test-with-haskell-buffer
      "zip :: ([x"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\])))))

(ert-deftest general-close-haskell-typelist-3 ()
  (general-close-test-with-haskell-buffer
      "zip :: ([x],"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\[)))))

(ert-deftest general-close-haskell-typelist-4 ()
  (general-close-test-with-haskell-buffer
      "zip :: ([x],["
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?y)))))


(provide 'general-close-haskell-tests)
;;; general-close-haskell-tests.el ends here
