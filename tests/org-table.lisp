(defpackage :organ-mode-tests/org-table
  (:use :cl :rove :organ-mode-tests)
  (:import-from
   :lem-fake-interface
   :with-fake-interface))

(in-package :organ-mode-tests/org-table)

(register-test-suite :organ-mode-tests/org-table)

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
