;;; shortcuts-tests.el --- Supporting tests
;;; Commentary:
;;
;; Test code for shortcuts module.
;;
;;; Code:
(require 'ert)
(require 'shortcuts)

(ert-deftest test-parse-link ()
  (let* ((link "https://www.myurl.com")
         (parsed (parse-link link)))
    (should (= 1 (length parsed)))
    (should (string-equal link (caar parsed))))
  (let* ((base-link "https://www.myurl.com/")
         (prompt "query-lisp")
         (link (string-join (list base-link
                                  link-lisp-replacement-separator
                                  prompt
                                  link-lisp-replacement-separator)))
         (parsed (parse-link link)))
    (should (= 1 (length parsed)))
    (let ((link-component (caar parsed))
          (interactive-component (cadar parsed)))
      (should (string-equal base-link link-component))
      (should (string-equal prompt (cadr interactive-component)))
      (should (string-equal "X" (car interactive-component))))
    )
  (let* ((base-link "https://www.myurl.com/")
         (prompt "query-string")
         (link (string-join (list base-link
                                  link-string-replacement-separator
                                  prompt
                                  link-string-replacement-separator)))
         (parsed (parse-link link)))
    (should (= 1 (length parsed)))
    (let ((link-component (caar parsed))
          (interactive-component (cadar parsed)))
      (should (string-equal base-link link-component))
      (should (string-equal prompt (cadr interactive-component)))
      (should (string-equal "s" (car interactive-component))))
    )
  (let* ((base-link "https://www.myurl.com/")
         (link-fragment "fragment")
         (prompt-string-1 "query-string-1")
         (prompt-string-2 "query-string-2")
         (link (string-join (list base-link
                                  link-string-replacement-separator
                                  prompt-string-1
                                  link-string-replacement-separator
                                  link-fragment
                                  link-string-replacement-separator
                                  prompt-string-2
                                  link-string-replacement-separator)))
         (parsed (parse-link link)))
    (should (= 2 (length parsed)))
    (let ((link-component (caar parsed))
          (interactive-component (cadar parsed)))
      (should (string-equal link-fragment link-component))
      (should (string-equal prompt-string-2 (cadr interactive-component)))
      (should (string-equal "s" (car interactive-component))))
    (let ((link-component (caadr parsed))
          (interactive-component (cadadr parsed)))
      (should (string-equal base-link link-component))
      (should (string-equal prompt-string-1 (cadr interactive-component)))
      (should (string-equal "s" (car interactive-component))))
    )
  )

(ert-deftest test-into-interactive-args ()
  (let* ((base-link "https://www.myurl.com/")
         (link-fragment "fragment")
         (prompt-string-1 "query-string-1")
         (prompt-string-2 "query-string-2")
         (link-fragment-end "fragment-end")
         (link (string-join (list base-link
                                  link-string-replacement-separator
                                  prompt-string-1
                                  link-string-replacement-separator
                                  link-fragment
                                  link-string-replacement-separator
                                  prompt-string-2
                                  link-string-replacement-separator
                                  link-fragment-end)))
         (parsed (parse-link link))
         (interactive-args (into-interactive-args parsed)))
    (should (string-equal "squery-string-1: \nsquery-string-2: " interactive-args))))

(ert-deftest test-enumerate-selections ()
  (let ((options '(())))
    (should (seq-empty-p (enumerate-selections options))))
  (let* ((options '((1)))
         (selections (enumerate-selections options)))
    (should (= 1 (length selections)))
    (should (= 1 (caar selections))))
  (let* ((options '((1 2) (4) (3 "a")))
        (selections (enumerate-selections options)))
    (should (= 4 (length selections)))
    (should (seq-contains-p selections (reverse '(1 4 3))))
    (should (seq-contains-p selections (reverse '(1 4 "a"))))
    (should (seq-contains-p selections (reverse '(2 4 3))))
    (should (seq-contains-p selections (reverse '(2 4 "a")))))
  (let* ((options '((1 2 3) (4 5 6) (7 8 9)))
        (selections (enumerate-selections options)))
    (should (= 27 (length selections))))
  (let* ((options '((1) (4) (7)))
         (selections (enumerate-selections options)))
    (should (= 1 (length selections)))
    (should (seq-contains-p selections (reverse '(1 4 7)))))
  )

(ert-deftest test-build-link ()
  (let ((link "🔑protocol🔑://test.com/🔑path1🔑/somepath/🔑path2🔑/end"))
    (should (string-equal
             "http://test.com/test2/somepath/test3/end"
             (car
              (build-link (parse-link link) '(("http") ("test2") ("test3"))
                          '(0 3))))))
  (let ((link "http://test.com/🔑path1🔑/somepath/🔑path2🔑/end"))
    (should (string-equal
             "http://test.com/test2/somepath/test3/end"
             (car
              (build-link (parse-link link) '(("test2") ("test3"))
                          '(0 2))))))
  (let ((link "http://test.com/somepath/🔑path2🔑/end"))
    (should (string-equal
             "http://test.com/somepath/test3/end"
             (car
              (build-link (parse-link link) '(("test3"))
                          '(0 1))))))
  (let ((link "🔑protocol🔑://test.com/🔑path1🔑/somepath/🔑path2🔑"))
    (should (string-equal
             "http://test.com/test2/somepath/test3"
             (car
              (build-link (parse-link link) '(("http") ("test2") ("test3"))
                          '(0 3))))))
  (let ((link "🔑protocol🔑://test.com/🔑path1🔑/"))
    (should (string-equal
             "http://test.com/test2/"
             (car
              (build-link (parse-link link) '(("http") ("test2"))
                          '(0 2))))))
  (let ((link "🔑protocol🔑://"))
    (should (string-equal
             "http://"
             (car
              (build-link (parse-link link) '(("http"))
                          '(0 1))))))
  (let ((link "http://"))
    (should (string-equal
             "http://"
             (car
              (build-link (parse-link link) '()
                          '(0 0)))))))

(provide 'shortcuts-tests)
;;; shortcuts-tests.el ends here
