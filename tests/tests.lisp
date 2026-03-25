(in-package :organ-mode-tests)

(deftest organ-mode-buffer-creation
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (lem:make-buffer "test.org")))
        (ok (lem:bufferp buffer))
        (ok (string= "test.org" (lem:buffer-name buffer)))))))

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
              "correct header found"))))))