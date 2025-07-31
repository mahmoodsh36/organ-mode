(defpackage :organ/utils
  (:use :cl :lem)
  (:export :char-offset-to-point))
(in-package :organ/utils)

(defun char-offset-to-point (buf offset)
  (let ((start (lem:copy-point (lem:buffer-start-point buf))))
    (lem:character-offset start offset)
    start))