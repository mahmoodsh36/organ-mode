(defpackage :organ-mode-tests
  (:use :cl :rove)
  (:import-from :lem-fake-interface
   :with-fake-interface))

(in-package :organ-mode-tests)

(defun parse-org (text)
  "parse org TEXT into a cltpt tree."
  (cltpt/base:parse cltpt/org-mode:*org-mode* text))

(defun make-organ-buffer (name text)
  "create a lem buffer with NAME, insert TEXT, parse the organ tree, and return (values buffer tree)."
  (let ((buffer (lem:make-buffer name)))
    (lem:insert-string (lem:buffer-point buffer) text)
    (let ((tree (parse-org text)))
      (setf (lem:buffer-value buffer 'organ/organ-mode::cltpt-tree) tree)
      (values buffer tree))))

(deftest organ-mode-buffer-creation
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (lem:make-buffer "test.org")))
        (ok (lem:bufferp buffer))
        (ok (string= "test.org" (lem:buffer-name buffer)))))))

(defun collect-children-of-type (tree type)
  "collect all descendants of TREE that are of TYPE."
  (let (results)
    (labels ((walk (node)
               (when (typep node type)
                 (push node results))
               (dolist (child (cltpt/base:text-object-children node))
                 (walk child))))
      (walk tree))
    (nreverse results)))

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

(deftest object-closest-to-pos-forward
  (let* ((text "* first
body
* second
* third")
         (tree (parse-org text)))
    ;; from position 1 (at "* first"), :forward finds strictly after pos,
    ;; so first (which starts at 1) is skipped and second is found.
    (let ((next (organ/organ-mode::object-closest-to-pos
                 tree
                 1
                 :forward
                 (lambda (obj) (typep obj 'cltpt/org-mode:org-header)))))
      (ok (search "second" (cltpt/base:text-object-text next))
          "first forward hit from pos 1 is second"))))

(deftest organ-header-navigation
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let* ((headers '("head 1" "head 2" "head 3"))
             (org-text (format nil "~{~%* ~a~%~%body~}" headers))
             (buffer (make-organ-buffer "nav.org" org-text))
             (pt (lem:buffer-point buffer)))
        (lem:switch-to-buffer buffer)
        (testing "forward navigation"
          (lem:move-point pt (organ/utils:char-offset-to-point buffer 1))
          (dolist (expected (cdr headers))
            (organ/organ-mode::organ-move-to-element
             :forward
             (lambda (obj) (typep obj 'cltpt/org-mode:org-header)))
            (let ((line-text (lem:line-string pt)))
              (ok (search expected line-text)
                  (format nil "forward should land on ~a, got: ~a" expected line-text)))))
        (testing "backward navigation"
          (lem:buffer-end pt)
          (dolist (expected (reverse (cdr headers)))
            (organ/organ-mode::organ-move-to-element
             :backward
             (lambda (obj) (typep obj 'cltpt/org-mode:org-header)))
            (let ((line-text (lem:line-string pt)))
              (ok (search expected line-text)
                  (format nil "backward should land on ~a, got: ~a" expected line-text)))))
        (testing "back-and-forth navigation"
          (lem:move-point pt (organ/utils:char-offset-to-point buffer 1))
          (organ/organ-mode::organ-move-to-element
           :forward
           (lambda (obj) (typep obj 'cltpt/org-mode:org-header)))
          (let ((line-text (lem:line-string pt)))
            (ok (search "head 2" line-text)
                "forward to move to head 2"))
          (organ/organ-mode::organ-move-to-element
           :backward
           (lambda (obj) (typep obj 'cltpt/org-mode:org-header)))
          (let ((line-text (lem:line-string pt)))
            (ok (search "head 1" line-text)
                "backward to return to head 1"))
          (organ/organ-mode::organ-move-to-element
           :forward
           (lambda (obj) (typep obj 'cltpt/org-mode:org-header)))
          (let ((line-text (lem:line-string pt)))
            (ok (search "head 2" line-text)
                "forward again to return to head 2")))))))

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