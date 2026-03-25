(defpackage :organ-mode-tests/org-list
  (:use :cl :rove :organ-mode-tests)
  (:import-from
   :lem-fake-interface
   :with-fake-interface))

(in-package :organ-mode-tests/org-list)

(register-test-suite :organ-mode-tests/org-list)

(defun setup-list-buffer (text)
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
            (setup-list-buffer
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
            (setup-list-buffer
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
            (setup-list-buffer "1. alpha
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
            (setup-list-buffer "- alpha
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
          (setup-list-buffer
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
          (setup-list-buffer
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
          (setup-list-buffer
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
          (setup-list-buffer
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
          (setup-list-buffer
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
          (setup-list-buffer
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
          (setup-list-buffer
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
          (setup-list-buffer
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
      ;; test 9: indent then dedent preserves cursor position (unequal bullet lengths)
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-buffer
           "i) hello
ii) here
iii) hello
iv) dont throw")
        (move-to-item buffer list-obj "dont throw")
        (let ((col-before (lem:point-column (lem:current-point))))
          ;; indent "iv)" under "iii)"
          (organ/organ-mode::org-list-indent-item list-obj)
          (check-buffer
           "test9-after-indent"
           buffer
           "i) hello
ii) here
iii) hello
     i) dont throw")
          (ok (= (lem:point-column (lem:current-point))
                 (+ col-before 4))
              "indent cursor accounts for bullet length change")
          ;; dedent back
          (setf list-obj (reparse-list buffer))
          (organ/organ-mode::org-list-dedent-item list-obj)
          (check-buffer
           "test9-after-dedent"
           buffer
           "i) hello
ii) here
iii) hello
iv) dont throw")
          (ok (= (lem:point-column (lem:current-point))
                 col-before)
              "dedent cursor restores original column")))
      ;; test 10: dedent with multi-line content preserves continuation indentation
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-buffer
           "1. hey
   i. alpha
   ii. what
      i. hellok
         hey")
        (move-to-item buffer list-obj "i. hellok")
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "test10-dedent-multiline-content"
         buffer
         "1. hey
   i. alpha
   ii. what
   iii. hellok
        hey"))
      ;; test 11: indent first item errors
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-buffer
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
        ;; test 12: dedent top-level item is no-op
        (organ/organ-mode::org-list-dedent-item list-obj)
        (check-buffer
         "test12-dedent-noop"
         buffer
         "1. alpha
   1. beta
   2. gamma
   3. delta")
        ;; test 13: newline at end of sub-list item renumbers subsequent siblings
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "1. beta" t)
        (organ/organ-mode::org-list-newline)
        (check-buffer
         "test13-newline-sublist"
         buffer
         "1. alpha
   1. beta
   2. 
   3. gamma
   4. delta")
        ;; test 14: newline at end of top-level item with children
        ;; new sibling appears after the full subtree of the current item
        (setf list-obj (reparse-list buffer))
        (move-to-item buffer list-obj "1. alpha" t)
        (organ/organ-mode::org-list-newline)
        (check-buffer
         "test14-newline-toplevel"
         buffer
         "1. alpha
   1. beta
   2. 
   3. gamma
   4. delta
2. ")))))

(deftest list-unordered-tests
  "indent/dedent/newline operations on unordered lists."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (multiple-value-bind (buffer tree list-obj)
          (setup-list-buffer
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

(deftest list-newline-top-level
  "org-list-newline on top-level ordered lists produces correct next bullet."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "numeric list newline on last item"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-buffer
             "1. more text
2. more text2
3. more lists")
          (move-to-item buffer list-obj "3. more lists" t)
          (organ/organ-mode::org-list-newline)
          (check-buffer
           "numeric-newline"
           buffer
           "1. more text
2. more text2
3. more lists
4. ")))
      (testing "alpha list newline on last item"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-buffer
             "a. first
b. second")
          (move-to-item buffer list-obj "b. second" t)
          (organ/organ-mode::org-list-newline)
          (check-buffer
           "alpha-newline"
           buffer
           "a. first
b. second
c. ")))
      (testing "numeric list newline twice"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-buffer
             "1. alpha
2. beta")
          (move-to-item buffer list-obj "2. beta" t)
          (organ/organ-mode::org-list-newline)
          (setf list-obj (reparse-list buffer))
          (move-to-item buffer list-obj "3. " t)
          (organ/organ-mode::org-list-newline)
           (check-buffer
            "numeric-newline-twice"
            buffer
            "1. alpha
2. beta
3. 
4. "))))))

(deftest list-newline-nested
  "org-list-newline in nested lists produces correct indentation and bullet."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "newline in sub-list maintains correct indentation"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-buffer
             "1. hello
2. there
   i. what is up
3. test")
          (move-to-item buffer list-obj "i. what is up" t)
          (organ/organ-mode::org-list-newline)
          (check-buffer
           "newline-nested-roman"
           buffer
           "1. hello
2. there
   i. what is up
   ii. 
3. test")
          ;; cursor should be at end of "   ii. " (column 7)
          (ok (= (lem:point-column (lem:current-point)) 7)
              "cursor placed after bullet and space"))))))

(deftest list-newline-end-of-file
  "org-list-newline on last item at end of file produces correct indentation."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "newline at end of top-level list at end of file"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-buffer
             "1. first
2. second
3. third")
          (move-to-item buffer list-obj "3. third" t)
          (organ/organ-mode::org-list-newline)
          (check-buffer
           "newline-eof-toplevel"
           buffer
           "1. first
2. second
3. third
4. ")))
      (testing "newline at end of nested list at end of file"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-buffer
             "1. hello
2. there
   i. what is up")
          (move-to-item buffer list-obj "i. what is up" t)
          (organ/organ-mode::org-list-newline)
           (check-buffer
            "newline-eof-nested"
            buffer
            "1. hello
2. there
   i. what is up
   ii. "))))))

(deftest list-cycle-bullet-indentation
  "cycling bullet type adjusts sub-list indentation to match new bullet length."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (testing "cycle from numeric-paren to roman adjusts sub-list indent"
        (multiple-value-bind (buffer tree list-obj)
            (setup-list-buffer
             "1) hello
2) here
   1. hello
   2. hey")
          (move-to-item buffer list-obj "1) hello")
          (organ/organ-mode::org-list-cycle-bullet list-obj)
          (check-buffer
           "cycle-indent-roman"
           buffer
           "i. hello
ii. here
    1. hello
    2. hey"))))))