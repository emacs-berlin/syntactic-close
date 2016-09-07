
;; (ert-deftest general-close-haskell-concat-test ()
;;   ;; indent s = "    " ++ s
;;   (general-close-test "indent s = \"asdf\""
;;     'haskell-mode
;;     'general-close-debug-p
;;     (general-close)
;;     (skip-chars-backward " \t\r\n\f")
;;     (should (eq (char-before) ?+))))

(ert-deftest general-close-list-comprehension-test-1 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test "[(asdb,"
    'haskell-mode
    'general-close-debug-p
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?b)))))

(ert-deftest general-close-list-comprehension-test-2 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test "[(asdb,cdfg"
    'haskell-mode
    'general-close-debug-p
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?\))))))

(ert-deftest general-close-list-comprehension-test-3 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test "[(x"
    'haskell-mode
    'general-close-debug-p
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?,)))))


(ert-deftest general-close-list-comprehension-test-4 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test "[(x,"
    'haskell-mode
    'general-close-debug-p
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?y)))))

(ert-deftest general-close-list-comprehension-test-5 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test "[(a,"
    'haskell-mode
    'general-close-debug-p
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?b)))))




(provide 'general-close-haskell-tests)
;;; general-close-haskell-tests.el ends here
