
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
  (general-close-test-with-haskell-buffer "[(asdb,"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?b)))))

(ert-deftest general-close-list-comprehension-test-2 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(asdb,cdfg"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?\))))))

(ert-deftest general-close-list-comprehension-test-3 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?,)))))

(ert-deftest general-close-list-comprehension-test-4 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?y)))))

(ert-deftest general-close-list-comprehension-test-5 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(a,"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (should (eq (char-before) ?b)))))

(ert-deftest general-close-list-comprehension-test-6 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(a,b)] |"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?a)))))

(ert-deftest general-close-list-comprehension-test-7 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,y)] |"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?x)))))

(ert-deftest general-close-list-comprehension-test-8 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(abd,def)] |"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "abd")))))

(ert-deftest general-close-list-comprehension-test-9 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(a,b)] | a"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest general-close-list-comprehension-test-10 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(x,y)] | x"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest general-close-list-comprehension-test-11 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(abd,def)] | abd"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (looking-back "<-")))))

(ert-deftest general-close-list-comprehension-test-12 ()
  ;; [(x,y)|x<-[1..3],y<-[4,5]]
  (general-close-test-with-haskell-buffer "[(asdb, asdb"
    (let ((general-close-electric-listify-p t))
      (general-close)
      (skip-chars-backward " \t\r\n\f")
      (should (eq (char-before) ?\))))))


(provide 'general-close-haskell-tests)
;;; general-close-haskell-tests.el ends here
