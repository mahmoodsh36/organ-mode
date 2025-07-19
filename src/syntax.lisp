(in-package :organ-mode)

;; should return a list of attributes for lem for "syntax highlighting" in a buffer
;; technically could be used for more than just syntax highlighting
(defgeneric text-object-lem-overlays (text-obj buf))

;; default function for syntax highlighting.
(defmethod text-object-lem-overlays ((obj cltpt/base:text-object) buf)
  (unless (typep obj 'cltpt/base::document)
    (let ((begin (cltpt/base:text-object-begin-in-root obj))
          (end (cltpt/base:text-object-end-in-root obj)))
      (list
       (lem:make-overlay (char-offset-to-point buf begin)
                         (char-offset-to-point buf end)
                         (lem:make-attribute :background  "#FFFACD"))))))

(defun char-offset-to-point (buf offset)
  (let ((start (lem:copy-point (lem:buffer-start-point buf))))
    (lem:character-offset start offset)
    start))