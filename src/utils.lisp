(defpackage :organ/utils
  (:use :cl)
  (:export
   :char-offset-to-point :current-pos :replace-text-between-positions
   :format-timestamp :replace-submatch-text))

(in-package :organ/utils)

(defun char-offset-to-point (buf offset)
  (let ((start (lem:copy-point (lem:buffer-start-point buf) :temporary)))
    (lem:character-offset start offset)
    start))

(defun current-pos ()
  (1- (lem:position-at-point (lem:current-point))))

(defun delete-text-between-positions (buffer start-pos end-pos)
  "delete text between two absolute positions (indices) in the buffer."
  (when (> start-pos end-pos)
    (rotatef start-pos end-pos))
  (let ((start-point (lem:copy-point (lem:buffer-start-point buffer) :temporary))
        (end-point (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
    (lem:move-to-position start-point start-pos)
    (lem:move-to-position end-point end-pos)
    (lem:delete-between-points start-point end-point)))

(defun replace-text-between-positions (buffer start-pos end-pos replacement-text)
  "replace text between two absolute positions in the buffer with replacement text."
  (when (> start-pos end-pos)
    (rotatef start-pos end-pos))
  (let ((start-point (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
    (unless (lem:move-to-position start-point start-pos)
      (error "start position ~A is out of buffer bounds" start-pos))
    (let ((end-point (lem:copy-point start-point :temporary)))
      (unless (lem:move-to-position end-point end-pos)
        (error "end position ~A is out of buffer bounds" end-pos))
      (lem:delete-between-points start-point end-point)
      (lem:insert-string start-point replacement-text))))

(defun replace-submatch-text (buffer text-obj submatch-id new-text)
  (let* ((match (cltpt/base:text-object-combinator-match text-obj))
         (submatch (cltpt/combinator:find-submatch match submatch-id)))
    (replace-text-between-positions
     buffer
     (1+ (getf (car submatch) :begin))
     (1+ (getf (car submatch) :end))
     new-text)))

(defun format-timestamp (&optional (timestamp (local-time:now)))
  "return a string like <2024-01-30 Tue> for the given TIMESTAMP."
  (let* ((base (local-time:format-timestring
                nil
                timestamp
                :format '(:year "-" (:month 2) "-" (:day 2))))
         (weekday (nth (local-time:timestamp-day-of-week timestamp)
                       '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
    (format nil "<~A ~A>" base weekday)))