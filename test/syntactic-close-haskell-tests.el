

;; zipWith (\x ->y(\y -> (x, y) [1, 2, 3] [1, 2, 3]

(ert-deftest syntactic-close-haskell-string-test-gJVmTt ()
  (syntactic-close-test
      "indent s = \"asdf"
    'haskell-mode
    'syntactic-close-debug-p
    (syntactic-close)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close-haskell-lambda-test-gJVmTt ()
  (syntactic-close-test
      "zipWith (\\x ->y(\\y -> (x, y)\[1, 2, 3] \[1, 2, 3]"
    'haskell-mode
    'syntactic-close-debug-p
    (search-backward "y")
    (forward-char 2)
    (syntactic-close)
    (should (looking-back "))" (line-beginning-position)))))


(provide 'syntactic-close-haskell-tests)
;;; syntactic-close-haskell-tests.el ends here
