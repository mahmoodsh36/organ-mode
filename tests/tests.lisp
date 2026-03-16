(in-package :organ-mode-tests)

(deftest organ-mode-buffer-creation
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (lem:make-buffer "test.org")))
        (ok (lem:bufferp buffer))
        (ok (string= "test.org" (lem:buffer-name buffer)))))))

(deftest roman-numeral-tests
  (let ((test-alist '((1 . "i")
                      (4 . "iv")
                      (9 . "ix")
                      (14 . "xiv")
                      (42 . "xlii")
                      (99 . "xcix")
                      (100 . "c")
                      (399 . "cccxcix")
                      (500 . "d")
                      (1000 . "m")
                      (1994 . "mcmxciv")
                      (3999 . "mmmcmxcix"))))
    (testing "int-to-roman conversion"
      (dolist (pair test-alist)
        (let ((n (car pair))
              (roman (cdr pair)))
          (ok (string= roman (organ/organ-mode::int-to-roman n))
              (format nil "~A -> ~A" n roman)))))
    (testing "roman-to-int conversion"
      (dolist (pair test-alist)
        (let ((n (car pair))
              (roman (cdr pair)))
          (ok (= n (organ/organ-mode::roman-to-int roman))
              (format nil "~A -> ~A" roman n))))
      (ok (null (organ/organ-mode::roman-to-int ""))
          "empty string should return nil")
      (ok (null (organ/organ-mode::roman-to-int "qwerty"))
          "invalid string should return nil"))))

(deftest next-bullet-test
  (ok (string= "10." (organ/organ-mode::next-bullet "9."))
      "next-bullet numeric 9->10")
  (ok (string= "iv." (organ/organ-mode::next-bullet "iii."))
      "next-bullet roman increment iii->iv"))

(deftest parse-simple-org-tree
  (let* ((text "* header 1
some body text
** sub-header
more text
* header 2")
         (tree (parse-org text)))
    (let ((headers (collect-children-of-type tree 'cltpt/org-mode:org-header)))
      (ok (= 3 (length headers))
          (format nil "expected 3 headers, got ~A" (length headers))))))

(deftest find-node-at-pos-test
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((text (format nil "* my header~%some body~%** sub~%"))
             (tree (parse-org text)))
        ;; position 3 is inside "* my header" (after "* ")
        (let ((node (organ/utils:find-node-at-pos tree 3 'cltpt/org-mode:org-header)))
          (ok (typep node 'cltpt/org-mode:org-header)
              "node of type header found")
          (ok (search "my header" (cltpt/base:text-object-text node))
              "correct header found"))
        ;; position well past headers should return nil for header type
        ;; (depends on exact text layout, just check it doesn't error)
        (ok (not (organ/utils:find-node-at-pos tree 1000 'cltpt/org-mode:org-header))
            "no header at out-of-bounds pos")))))