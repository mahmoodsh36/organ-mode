(defpackage :organ-mode-tests/move
  (:use :cl :rove :organ-mode-tests)
  (:import-from
   :lem-fake-interface
   :with-fake-interface))

(in-package :organ-mode-tests/move)

(register-test-suite :organ-mode-tests/move)

(defun setup-header-move-buffer (text)
  "create a buffer, insert TEXT, parse the cltpt tree, switch to it,
and return (values buffer tree headers) where headers are the
level-1 org-header objects sorted by position."
  (multiple-value-bind (buffer tree)
      (make-organ-buffer (make-test-buf-name) text)
    (lem:switch-to-buffer buffer)
    (let ((headers
            (sort (remove-if-not
                   (lambda (c)
                     (and (typep c 'cltpt/org-mode:org-header)
                          (= (cltpt/base:text-object-property c :level) 1)))
                   (cltpt/base:text-object-children tree))
                  #'<
                  :key #'cltpt/base:text-object-begin-in-root)))
      (values buffer tree headers))))

(defun cursor-char-offset (buffer)
  "return the 0-indexed character offset of the current point in BUFFER."
  (organ/utils:point-to-char-offset (lem:buffer-point buffer)))

(defun run-header-move-case (label text idx direction expected expected-cursor-str)
  "run a single header-move test case.

IDX: index of header to move.
DIRECTION: is +1 or -1.
EXPECTED: the expected buffer text after the move.
EXPECTED-CURSOR-STR: the string to search for in EXPECTED to find the expected cursor position."
  (multiple-value-bind (buffer tree headers)
      (setup-header-move-buffer text)
    (let ((header (nth idx headers)))
      (lem:move-point
       (lem:current-point)
       (organ/utils:char-offset-to-point
        buffer
        (cltpt/base:text-object-begin-in-root header)))
      (organ/organ-mode::org-header-move header direction)
      (let* ((result (lem:buffer-text buffer))
             (cursor (cursor-char-offset buffer))
             (expected-pos (search expected-cursor-str expected))
             (buf-ok (string= result expected))
             (cur-ok (= cursor expected-pos)))
        (ok buf-ok
            (if buf-ok
                (format nil "~A buffer ok" label)
                (format nil "~A buffer mismatch:~%  got: ~S~%  exp: ~S" label result expected)))
        (ok cur-ok
            (if cur-ok
                (format nil "~A cursor ok (~A)" label cursor)
                (format nil "~A cursor mismatch: got ~A, exp ~A" label cursor expected-pos)))))))

(deftest header-move
  "comprehensive header movement tests."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "forward two headers"
        (run-header-move-case
         "fwd-2"
         "* alpha
body alpha

* beta
body beta"
         0
         1
         "* beta
body beta
* alpha
body alpha

"
         "* alpha"))
      (testing "backward two headers"
        (run-header-move-case
         "bwd-2"
         "* alpha
body alpha

* beta
body beta"
         1
         -1
         "* beta
body beta
* alpha
body alpha

"
         "* beta"))
      (testing "three headers middle swap"
        (run-header-move-case
         "mid-3"
         "* alpha
body a

* beta
body b

* gamma
body g"
         0
         1
         "* beta
body b

* alpha
body a

* gamma
body g"
         "* alpha"))
      (testing "three headers last up"
        (run-header-move-case
         "last-up"
         "* alpha
body a

* beta
body b

* gamma
body g"
         2
         -1
         "* alpha
body a

* gamma
body g
* beta
body b

"
         "* gamma"))
      (testing "second down to end"
        (run-header-move-case
         "2nd-down"
         "* alpha
body a

* beta
body b

* gamma
body g"
         1
         1
         "* alpha
body a

* gamma
body g
* beta
body b

"
         "* beta"))
      (testing "with subheaders"
        (run-header-move-case
         "sub"
         "* alpha
** sub-alpha
body sub

* beta
body beta"
         0
         1
         "* beta
body beta
* alpha
** sub-alpha
body sub

"
         "* alpha"))
      (testing "forward with trailing newline"
        (run-header-move-case
         "trail-fwd"
         "* alpha
body alpha

* beta
body beta
"
         0
         1
         "* beta
body beta
* alpha
body alpha

"
         "* alpha"))
      (testing "backward with trailing newline"
        (run-header-move-case
         "trail-bwd"
         "* alpha
body alpha

* beta
body beta
"
         1
         -1
         "* beta
body beta
* alpha
body alpha

"
         "* beta"))
      (testing "three headers trailing newline"
        (run-header-move-case
         "trail-3"
         "* alpha
body a

* beta
body b

* gamma
body g
"
         1
         1
         "* alpha
body a

* gamma
body g
* beta
body b

"
         "* beta"))
      (dolist (case '(("round-trip"
                       "* alpha
body alpha

* beta
body beta"
                       "* alpha
body alpha

* beta
body beta
")
                      ("round-trip-trail"
                       "* alpha
body alpha

* beta
body beta
"
                       nil)))
        (destructuring-bind (label text expected-or-nil) case
          (let ((expected (or expected-or-nil text)))
            (testing label
              (multiple-value-bind (buffer tree headers)
                  (setup-header-move-buffer text)
                (let* ((first-header (first headers))
                       (original-pos (cltpt/base:text-object-begin-in-root first-header)))
                  (lem:move-point (lem:current-point)
                                  (organ/utils:char-offset-to-point buffer original-pos))
                  (organ/organ-mode::org-header-move first-header 1)
                  (let* ((new-tree (cltpt/base:parse cltpt/org-mode:*org-mode*
                                                     (lem:buffer-text buffer)))
                         (new-headers
                           (sort (remove-if-not
                                  (lambda (c)
                                    (and (typep c 'cltpt/org-mode:org-header)
                                         (= (cltpt/base:text-object-property c :level) 1)))
                                  (cltpt/base:text-object-children new-tree))
                                 #'<
                                 :key #'cltpt/base:text-object-begin-in-root))
                         (alpha-header (second new-headers)))
                    (setf (lem:buffer-value buffer 'organ/organ-mode::cltpt-tree) new-tree)
                    (organ/organ-mode::org-header-move alpha-header -1)
                    (let* ((result (lem:buffer-text buffer))
                           (cursor (cursor-char-offset buffer))
                           (buf-ok (string= result expected))
                           (cur-ok (= cursor original-pos)))
                      (ok buf-ok
                          (if buf-ok (format nil "~A buffer ok" label)
                              (format nil
                                      "~A buffer mismatch:~%  got: ~S~%  exp: ~S"
                                      label
                                      result
                                      expected)))
                      (ok cur-ok
                          (if cur-ok (format nil "~A cursor ok (~A)" label cursor)
                              (format nil
                                      "~A cursor mismatch: got ~A, exp ~A"
                                      label
                                      cursor
                                      original-pos)))))))))))
      (testing "no-op at boundary"
        (let ((text "* alpha
body a

* beta
body b"))
          (multiple-value-bind (buffer tree headers)
              (setup-header-move-buffer text)
            (let ((first-header (first headers)))
              (lem:move-point (lem:current-point)
                              (organ/utils:char-offset-to-point
                               buffer
                               (cltpt/base:text-object-begin-in-root first-header)))
              (organ/organ-mode::org-header-move first-header -1)
              (ok (string= (lem:buffer-text buffer) text)
                  "first header up is no-op"))
            (let ((last-header (second headers)))
              (lem:move-point (lem:current-point)
                              (organ/utils:char-offset-to-point
                               buffer
                               (cltpt/base:text-object-begin-in-root last-header)))
              (organ/organ-mode::org-header-move last-header 1)
              (ok (string= (lem:buffer-text buffer) text)
                  "last header down is no-op"))))))))

(defun setup-block-move-buffer (text)
  "create a buffer with TEXT, parse the cltpt tree, switch to it,
and return (values buffer tree blocks) where blocks are org-src-block
and org-block objects sorted by position."
  (multiple-value-bind (buffer tree)
      (make-organ-buffer (make-test-buf-name) text)
    (lem:switch-to-buffer buffer)
    (let ((blocks
            (sort (remove-if-not
                   (lambda (c)
                     (or (typep c 'cltpt/org-mode:org-src-block)
                         (typep c 'cltpt/org-mode:org-block)))
                   (cltpt/base:text-object-children tree))
                  #'<
                  :key #'cltpt/base:text-object-begin-in-root)))
      (values buffer tree blocks))))

(defun run-block-move-case (label text idx direction expected expected-cursor-str)
  (multiple-value-bind (buffer tree blocks)
      (setup-block-move-buffer text)
    (let ((blk (nth idx blocks)))
      (lem:move-point
       (lem:current-point)
       (organ/utils:char-offset-to-point
        buffer
        (cltpt/base:text-object-begin-in-root blk)))
      (organ/organ-mode::org-block-move blk direction)
      (let* ((result (lem:buffer-text buffer))
             (cursor (cursor-char-offset buffer))
             (expected-pos (search expected-cursor-str expected))
             (buf-ok (string= result expected))
             (cur-ok (= cursor expected-pos)))
        (ok buf-ok
            (if buf-ok (format nil "~A buffer ok" label)
                (format nil "~A buffer mismatch:~%  got: ~S~%  exp: ~S" label result expected)))
        (ok cur-ok
            (if cur-ok (format nil "~A cursor ok (~A)" label cursor)
                (format nil "~A cursor mismatch: got ~A, exp ~A" label cursor expected-pos)))))))

(deftest block-move
  "comprehensive block movement tests."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "move first block down"
        (run-block-move-case
         "blk-fwd"
         "#+begin_src lisp
(print 1)
#+end_src

#+begin_src lisp
(print 2)
#+end_src"
         0
         1
         "#+begin_src lisp
(print 2)
#+end_src

#+begin_src lisp
(print 1)
#+end_src"
         "#+begin_src lisp
(print 1)"))
      (testing "move second block up"
        (run-block-move-case
         "blk-bwd"
         "#+begin_src lisp
(print 1)
#+end_src

#+begin_src lisp
(print 2)
#+end_src"
         1 -1
         "#+begin_src lisp
(print 2)
#+end_src

#+begin_src lisp
(print 1)
#+end_src"
         "#+begin_src lisp
(print 2)"))
      (testing "three blocks middle swap"
        (run-block-move-case
         "blk-mid"
         "#+begin_src lisp
(print 1)
#+end_src

#+begin_src lisp
(print 2)
#+end_src

#+begin_src lisp
(print 3)
#+end_src"
         0 1
         "#+begin_src lisp
(print 2)
#+end_src

#+begin_src lisp
(print 1)
#+end_src

#+begin_src lisp
(print 3)
#+end_src"
         "#+begin_src lisp
(print 1)"))
      (testing "no-op at boundary"
        (let ((text "#+begin_src lisp
(print 1)
#+end_src

#+begin_src lisp
(print 2)
#+end_src"))
          (multiple-value-bind (buffer tree blocks)
              (setup-block-move-buffer text)
            (let ((first-blk (first blocks)))
              (lem:move-point (lem:current-point)
                              (organ/utils:char-offset-to-point
                               buffer
                               (cltpt/base:text-object-begin-in-root first-blk)))
              (organ/organ-mode::org-block-move first-blk -1)
              (ok (string= (lem:buffer-text buffer) text) "first block up is no-op"))
            (let ((last-blk (second blocks)))
              (lem:move-point (lem:current-point)
                              (organ/utils:char-offset-to-point
                               buffer
                               (cltpt/base:text-object-begin-in-root last-blk)))
              (organ/organ-mode::org-block-move last-blk 1)
              (ok (string= (lem:buffer-text buffer) text) "last block down is no-op"))))))))

(defun setup-header-level-buffer (text)
  "create a buffer with TEXT, parse the tree, switch to it, and return (values buffer tree)."
  (multiple-value-bind (buffer tree) (make-organ-buffer (make-test-buf-name) text)
    (lem:switch-to-buffer buffer)
    (values buffer tree)))

(defun all-headers (tree)
  (sort (collect-children-of-type tree 'cltpt/org-mode:org-header)
        #'<
        :key #'cltpt/base:text-object-begin-in-root))

(defun run-header-level-change-case (label text header-idx action expected)
  "run a single promote/demote test case.
HEADER-IDX: index into the sorted list of all headers.
ACTION: one of :promote, :demote, :promote-tree, :demote-tree.
EXPECTED: the expected buffer text after the action."
  (multiple-value-bind (buffer tree) (setup-header-level-buffer text)
    (let* ((headers (all-headers tree))
           (header (nth header-idx headers)))
      (lem:move-point
       (lem:current-point)
       (organ/utils:char-offset-to-point
        buffer
        (cltpt/base:text-object-begin-in-root header)))
      (ecase action
        (:promote (organ/organ-mode::org-header-level-increase header -1))
        (:demote (organ/organ-mode::org-header-level-increase header 1))
        (:promote-tree (organ/organ-mode::org-header-level-increase-tree header -1))
        (:demote-tree (organ/organ-mode::org-header-level-increase-tree header 1)))
      (check-buffer label buffer expected))))

(deftest header-promote-demote
  "header promotion and demotion tests."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "demote and promote single header"
        (run-header-level-change-case
         "demote"
         "* alpha
* beta"
         0
         :demote
         "** alpha
* beta")
        (run-header-level-change-case
         "promote"
         "** alpha
* beta"
         0
         :promote
         "* alpha
* beta")
        (run-header-level-change-case
         "promote-noop"
         "* alpha"
         0
         :promote
         "* alpha"))
      (testing "demote and promote subtree round-trip"
        ;; demote subtree: mixed levels, sibling not affected
        (run-header-level-change-case
         "demote-tree"
         "* test
** test2
**** hello1
*** hello2
* other"
         1
         :demote-tree
         "* test
*** test2
***** hello1
**** hello2
* other")
        ;; promote it back
        (run-header-level-change-case
         "promote-tree"
         "* test
*** test2
***** hello1
**** hello2
* other"
         1
         :promote-tree
         "* test
** test2
**** hello1
*** hello2
* other")))))