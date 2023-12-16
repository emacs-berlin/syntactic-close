
;; zipWith (\x ->y(\y -> (x, y) [1, 2, 3] [1, 2, 3]

(ert-deftest syntactic-close-haskell-string-test-gJVmTt ()
  (syntactic-close-test
      "indent s = \"asdf"
    'haskell-mode
    'syntactic-close-debug-p
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?\"))))

(ert-deftest syntactic-close-haskell-test-T1mOKj ()
  (syntactic-close-test
      "wertweise n x =
  [ k | k<-[0..n-1], k == x "
    'haskell-mode
    'syntactic-close-debug-p
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) 93))))

(ert-deftest syntactic-close-haskell-test-YUumkX ()
  (syntactic-close-test
      "{-# LANGUAGE OverloadedStrings"
    'haskell-mode
    'syntactic-close-debug-p
    (skip-chars-backward " \t\r\n\f")
    (syntactic-close)
    (should (eq (char-before) ?}))
    (should (eq (char-before (1- (point))) ?-))
    (should (eq (char-before (- (point) 2)) ?#))
    (should (eq (char-before (- (point) 3)) 32))
    )
  )



(provide 'syntactic-close-haskell-tests)
;;; syntactic-close-haskell-tests.el ends here
