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

(defun setup-table-move-buffer (text)
  "create a buffer with TEXT, parse the cltpt tree, switch to it,
and return (values buffer tree table) where table is the first org-table object."
  (multiple-value-bind (buffer tree)
      (make-organ-buffer (make-test-buf-name) text)
    (lem:switch-to-buffer buffer)
    (let ((table (find-if
                  (lambda (c) (typep c 'cltpt/org-mode:org-table))
                  (cltpt/base:text-object-children tree))))
      (values buffer tree table))))

(defun check-buffer (label buffer expected)
  "check that BUFFER text matches EXPECTED, with concise output."
  (let* ((result (lem:buffer-text buffer))
         (buf-ok (string= result expected)))
    (ok buf-ok
        (if buf-ok (format nil "~A buffer ok" label)
            (format nil "~A buffer mismatch:~%  got: ~S~%  exp: ~S" label result expected)))))

(deftest table-move-row
  "table row movement tests."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "move first row down"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| a | b |
| c | d |
| e | f |")
          (lem:move-point (lem:current-point)
                          (organ/utils:char-offset-to-point
                           buffer
                           (cltpt/base:text-object-begin-in-root table)))
          (organ/organ-mode::org-table-move-row table 1)
          (check-buffer
           "row-down"
           buffer
           "| c | d |
| a | b |
| e | f |")))
      (testing "move last row up"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| a | b |
| c | d |
| e | f |")
          (let ((last-row-pos (+ (cltpt/base:text-object-begin-in-root table)
                                 (search "| e" (cltpt/base:text-object-text table)))))
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point buffer last-row-pos))
            (organ/organ-mode::org-table-move-row table -1)
            (check-buffer
             "row-up"
             buffer
             "| a | b |
| e | f |
| c | d |"))))
      (testing "move middle row down in 4-row table"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| a | 1 |
| b | 2 |
| c | 3 |
| d | 4 |")
          (let ((row-pos (+ (cltpt/base:text-object-begin-in-root table)
                            (search "| b" (cltpt/base:text-object-text table)))))
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point buffer row-pos))
            (organ/organ-mode::org-table-move-row table 1)
            (check-buffer
             "row-mid"
             buffer
             "| a | 1 |
| c | 3 |
| b | 2 |
| d | 4 |"))))
      (testing "no-op at boundary"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| a | b |
| c | d |")
          (lem:move-point (lem:current-point)
                          (organ/utils:char-offset-to-point
                           buffer
                           (cltpt/base:text-object-begin-in-root table)))
          (organ/organ-mode::org-table-move-row table -1)
          (check-buffer
           "row no move up"
           buffer
           "| a | b |
| c | d |")
          (let ((last-row-pos (+ (cltpt/base:text-object-begin-in-root table)
                                 (search "| c" (cltpt/base:text-object-text table)))))
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point buffer last-row-pos))
            (organ/organ-mode::org-table-move-row table 1)
            (check-buffer
             "row-noop-down"
             buffer
             "| a | b |
| c | d |"))))
      (testing "table with hrule"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| h1 | h2 |
|----+----|
| a  | b  |
| c  | d  |")
          (let ((row-pos (+ (cltpt/base:text-object-begin-in-root table)
                            (search "| a" (cltpt/base:text-object-text table)))))
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point buffer row-pos))
            (organ/organ-mode::org-table-move-row table 1)
            (check-buffer
             "row-hrule"
             buffer
             "| h1 | h2 |
|----+----|
| c  | d  |
| a  | b  |")))))))

(deftest table-move-column
  "table column movement tests."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "move first column right"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| a | b | c |
| d | e | f |")
          ;; position cursor inside first cell
          (lem:move-point (lem:current-point)
                          (organ/utils:char-offset-to-point
                           buffer
                           (1+ (cltpt/base:text-object-begin-in-root table))))
          (organ/organ-mode::org-table-move-column table 1)
          (check-buffer
           "col-right"
           buffer
           "| b | a | c |
| e | d | f |")))
      (testing "move last column left"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| a | b | c |
| d | e | f |")
          ;; position cursor inside last cell
          (let ((col-pos (+ (cltpt/base:text-object-begin-in-root table)
                            (search " c " (cltpt/base:text-object-text table)))))
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point buffer col-pos))
            (organ/organ-mode::org-table-move-column table -1)
            (check-buffer
             "col-left"
             buffer
             "| a | c | b |
| d | f | e |"))))
      (testing "no-op at boundary"
        (multiple-value-bind (buffer tree table)
            (setup-table-move-buffer
             "| a | b |
| c | d |")
          ;; first col left is no-op
          (lem:move-point (lem:current-point)
                          (organ/utils:char-offset-to-point
                           buffer
                           (1+ (cltpt/base:text-object-begin-in-root table))))
          (organ/organ-mode::org-table-move-column table -1)
          (check-buffer
           "col-noop-left"
           buffer
           "| a | b |
| c | d |"))))))

(defun setup-list-move-buffer (text)
  "create a buffer with TEXT, parse the cltpt tree, switch to it,
and return (values buffer tree list-obj) where list-obj is the first org-list object."
  (multiple-value-bind (buffer tree)
      (make-organ-buffer (make-test-buf-name) text)
    (lem:switch-to-buffer buffer)
    (let ((list-obj (find-if
                     (lambda (c) (typep c 'cltpt/org-mode:org-list))
                     (cltpt/base:text-object-children tree))))
      (values buffer tree list-obj))))

(deftest list-move-item
  "list item movement tests."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "move first item down"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-move-buffer
             "- alpha
- beta
- gamma")
          (lem:move-point (lem:current-point)
                          (organ/utils:char-offset-to-point
                           buffer
                           (cltpt/base:text-object-begin-in-root list-obj)))
          (organ/organ-mode::org-list-move-item list-obj 1)
          (check-buffer
           "list-down"
           buffer
           "- beta
- alpha
- gamma")))
      (testing "move last item up"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-move-buffer
             "- alpha
- beta
- gamma")
          (let ((last-pos (+ (cltpt/base:text-object-begin-in-root list-obj)
                             (search "- gamma" (cltpt/base:text-object-text list-obj)))))
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point buffer last-pos))
            (organ/organ-mode::org-list-move-item list-obj -1)
            (check-buffer
             "list-up"
             buffer
             "- alpha
- gamma
- beta"))))
      (testing "numbered list preserves bullets"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-move-buffer "1. alpha
2. beta
3. gamma")
          (lem:move-point (lem:current-point)
                          (organ/utils:char-offset-to-point
                           buffer
                           (cltpt/base:text-object-begin-in-root list-obj)))
          (organ/organ-mode::org-list-move-item list-obj 1)
          (check-buffer
           "list-numbered"
           buffer
           "1. beta
2. alpha
3. gamma")))
      (testing "no-op at boundary"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-move-buffer "- alpha
- beta")
          (lem:move-point (lem:current-point)
                          (organ/utils:char-offset-to-point
                           buffer
                           (cltpt/base:text-object-begin-in-root list-obj)))
          (organ/organ-mode::org-list-move-item list-obj -1)
          (check-buffer
           "list-noop-up"
           buffer
           "- alpha
- beta")
          (let ((last-pos (+ (cltpt/base:text-object-begin-in-root list-obj)
                             (search "- beta" (cltpt/base:text-object-text list-obj)))))
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point buffer last-pos))
            (organ/organ-mode::org-list-move-item list-obj 1)
            (check-buffer
             "list-noop-down"
             buffer
             "- alpha
- beta")))))))

(defun reparse-list (buffer)
  "re-parse the buffer and return the first org-list object."
  (let* ((new-tree (cltpt/base:parse cltpt/org-mode:*org-mode* (lem:buffer-text buffer)))
         (new-list (find-if (lambda (c) (typep c 'cltpt/org-mode:org-list))
                            (cltpt/base:text-object-children new-tree))))
    (setf (lem:buffer-value buffer 'organ/organ-mode::cltpt-tree) new-tree)
    new-list))

(defun move-to-item (buffer list-obj search-str &optional end-of-line)
  "move cursor to the item matching SEARCH-STR in LIST-OBJ. when END-OF-LINE, move to end of that line."
  (let ((pos (+ (cltpt/base:text-object-begin-in-root list-obj)
                (search search-str (cltpt/base:text-object-text list-obj)))))
    (lem:move-point (lem:current-point)
                    (organ/utils:char-offset-to-point buffer pos))
    (when end-of-line
      (lem:line-end (lem:current-point)))))

(deftest list-indent-dedent-cycle
  "sequential indent/dedent cycle on a flat ordered list."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. alpha
2. beta
3. gamma
4. delta")
        ;; step 1: indent beta into a sublist of alpha
        (move-to-item buffer list-obj "2. beta")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "step1-indent-beta"
         buffer
         "1. alpha
   1. beta
2. gamma
3. delta")
        ;; step 2: indent gamma into alpha (continues sub-list after beta)
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "2. gamma")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "step2-indent-gamma"
         buffer
         "1. alpha
   1. beta
   2. gamma
2. delta")
        ;; step 3: dedent gamma back to top level, renumbers delta
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "2. gamma")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "step3-dedent-gamma"
         buffer
         "1. alpha
   1. beta
2. gamma
3. delta")
        ;; step 4: dedent beta, flatten back to original
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "1. beta")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "step4-flatten"
         buffer
         "1. alpha
2. beta
3. gamma
4. delta")
        ;; step 5: indent gamma again, then indent deeper (nested under beta)
        ;; first re-indent beta to set up the right state
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "2. beta")
        (organ/organ-mode::org-list-indent-item list-obj)
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "2. gamma")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "step5-indent-gamma-again"
         buffer
         "1. alpha
   1. beta
   2. gamma
2. delta")
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "2. gamma")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "step5b-indent-deeper"
         buffer
         "1. alpha
   1. beta
      1. gamma
2. delta")
        ;; step 6: dedent gamma from depth 3, becomes sibling of beta
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "1. gamma")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "step6-dedent-deep"
         buffer
         "1. alpha
   1. beta
   2. gamma
2. delta")))))

(deftest more-list-indent-dedent-cases
  "more indent/dedent/etc list operations"
  (lem:with-current-buffers ()
    (with-fake-interface ()
      ;; test 1: dedent item with children errors
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
   1. is
      1. going
   2. heyy")
        (move-to-item buffer list-obj "1. is")
        (ok (typep (nth-value
                    0
                    (ignore-errors
                     (organ/organ-mode::org-list-dedent-item list-obj)))
                   'null)
            "dedent item with children signals editor-error")
        (check-buffer
         "test1-dedent-children-noop"
         buffer
         "1. hey
   1. is
      1. going
   2. heyy")
        ;; test 2: dedent leaf item (going)
        (move-to-item buffer list-obj "1. going")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "test2-dedent-leaf"
         buffer
         "1. hey
   1. is
   2. going
   3. heyy"))
      ;; test 3: dedent with subsequent siblings, siblings become children
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
   1. is
      1. going
      2. wow")
        (move-to-item buffer list-obj "1. going")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "test3-dedent-adopt-siblings"
         buffer
         "1. hey
   1. is
   2. going
      1. wow"))
      ;; test 4: indent-item detaches children
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
2. is
   1. hello")
        (move-to-item buffer list-obj "2. is")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "test4-indent-detach-children"
         buffer
         "1. hey
   1. is
   2. hello"))
      ;; test 5: indent-tree keeps children
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
2. is
   1. hello")
        (move-to-item buffer list-obj "2. is")
        (organ/organ-mode::org-list-indent-tree list-obj)
        (check-buffer
         "test5-indent-tree-with-children"
         buffer
         "1. hey
   1. is
      1. hello"))
      ;; test 6: dedent-tree
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
   1. is
      1. going
   2. heyy")
        (move-to-item buffer list-obj "1. is")
        (organ/organ-mode::org-list-dedent-tree list-obj)
        (check-buffer
         "test6-dedent-tree"
         buffer
         "1. hey
2. is
   1. going
   2. heyy"))
      ;; test 7: indent roman numeral item preserves roman bullet type
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
   i. hey
   ii. what")
        (move-to-item buffer list-obj "ii. what")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "test7-indent-roman-bullet"
         buffer
         "1. hey
   i. hey
      i. what"))
      ;; test 8: dedent with bullet length change preserves cursor position
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
   i. alpha
   ii. what
      i. z")
        (move-to-item buffer list-obj "i. z" t)
        (let ((col-before (lem:point-column (lem:current-point))))
          (organ/organ-mode::org-list-dedent-item list-obj)
          (check-buffer
           "test8-dedent-cursor-position"
           buffer
           "1. hey
   i. alpha
   ii. what
   iii. z")
          (ok (= (lem:point-column (lem:current-point))
                 (1- col-before))
              "dedent cursor accounts for bullet length change")))
      ;; test 9: dedent with multi-line content preserves continuation indentation
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. hey
   i. alpha
   ii. what
      i. hellok
         hey")
        (move-to-item buffer list-obj "i. hellok")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "test9-dedent-multiline-content"
         buffer
         "1. hey
   i. alpha
   ii. what
   iii. hellok
        hey"))
      ;; test 10: indent first item errors
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "1. alpha
   1. beta
   2. gamma
   3. delta")
        (move-to-item buffer list-obj "1. alpha")
        (ok (typep (nth-value
                    0
                    (ignore-errors
                     (organ/organ-mode::org-list-indent-item list-obj)))
                   'null)
            "indent first item signals editor-error")
        ;; test 11: dedent top-level item is no-op
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "test11-dedent-noop"
         buffer
         "1. alpha
   1. beta
   2. gamma
   3. delta")
        ;; test 12: newline at end of sub-list item
        ;; note: lines with just a bullet have a trailing space after the dot
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "1. beta" t)
        (organ/organ-mode::org-list-newline)
        (check-buffer
         "test12-newline-sublist"
         buffer
         "1. alpha
   1. beta
   2. 
   2. gamma
   3. delta")
        ;; test 13: newline at end of top-level item
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "1. alpha" t)
        (organ/organ-mode::org-list-newline)
        (check-buffer
         "test13-newline-toplevel"
         buffer
         "1. alpha
2. 
   1. beta
   2. 
   2. gamma
   3. delta")))))

(deftest list-unordered-tests
  "indent/dedent/newline operations on unordered lists."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-move-buffer
           "- apple
- banana
- cherry
- date")
        ;; indent banana (2-space indent, bullet stays -)
        (move-to-item buffer list-obj "- banana")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "indent"
         buffer
         "- apple
  - banana
- cherry
- date")
        ;; indent cherry into same sub-list
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "- cherry")
        (organ/organ-mode::org-list-indent-item list-obj)
        (check-buffer
         "indent-consecutive"
         buffer
         "- apple
  - banana
  - cherry
- date")
        ;; dedent cherry from multi-child sub-list
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "- cherry")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "dedent-multi-child"
         buffer
         "- apple
  - banana
- cherry
- date")
        ;; dedent banana
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "- banana")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "flatten"
         buffer
         "- apple
- banana
- cherry
- date")
        ;; newline in unordered sub-list
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "- banana")
        (organ/organ-mode::org-list-indent-item list-obj)
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "- banana" t)
        (organ/organ-mode::org-list-newline)
        (check-buffer
         "newline-sublist"
         buffer
         "- apple
  - banana
  - 
- cherry
- date")))))