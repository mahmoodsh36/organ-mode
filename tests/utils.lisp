(defpackage :organ-mode-tests
  (:use :cl :rove)
  (:import-from
   :lem-fake-interface
   :with-fake-interface)
  (:export
   :parse-org
   :make-organ-buffer
   :make-test-buf-name
   :collect-children-of-type
   :with-fake-interface
   :register-test-suite
   :run-all-suites))

(in-package :organ-mode-tests)

(defvar *test-suites*
  (list :organ-mode-tests)
  "list of test suite package names to run.")

(defun register-test-suite (package-name)
  "register a test suite to be run by run-all-suites."
  (pushnew package-name *test-suites*))

(defun run-all-suites ()
  "run all registered test suites."
  (dolist (suite (reverse *test-suites*))
    (rove:run-suite suite)))

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

(let ((buf-counter 0))
  (defun make-test-buf-name ()
    (format nil "test-~A.org" (incf buf-counter))))