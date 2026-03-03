(defpackage :organ-mode-tests
  (:use :cl :rove)
  (:import-from :lem-fake-interface
   :with-fake-interface))

(in-package :organ-mode-tests)

(deftest int-to-roman-test
  (ok (string= "mcmxciv" (organ/organ-mode::int-to-roman 1994))))

(deftest organ-mode-buffer-creation
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (lem:make-buffer "test.org")))
        (ok (lem:bufferp buffer))
        (ok (string= "test.org" (lem:buffer-name buffer)))))))