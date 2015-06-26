(defvar general-close-js-test-string-1 " $(document).ready(function() {

          $('.nav-tabs-custom-2').tabs();

          $('.table').delegate('td','mouseover mouseleave',function(e) {
              if (e.type == 'mouseover') {
                  $(this).addClass('hover")

(setq general-close-js-test-string-1 " $(document).ready(function() {

          $('.nav-tabs-custom-2').tabs();

          $('.table').delegate('td','mouseover mouseleave',function(e) {
              if (e.type == 'mouseover') {
                  $(this).addClass('hover")

(ert-deftest gen-close-js-test-1 ()
  (gen-test-with-js-buffer
      general-close-js-test-string-1
    (skip-chars-backward " \t\r\n\f")
    (general-close)
    (should (eq (char-before) ?'))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\;))
    (general-close)
    (should (eq (char-before) ?\}))
    (general-close)
    (should (eq (char-before) ?\}))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\;))
    (general-close)
    (should (eq (char-before) ?\}))
    (general-close)
    (should (eq (char-before) ?\)))
    (general-close)
    (should (eq (char-before) ?\;))

))
