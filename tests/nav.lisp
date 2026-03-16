(defpackage :organ-mode-tests/nav
  (:use :cl :rove :organ-mode-tests)
  (:import-from
   :lem-fake-interface
   :with-fake-interface))

(in-package :organ-mode-tests/nav)

(register-test-suite :organ-mode-tests/nav)

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
