(defpackage :organ/utils
  (:use :cl)
  (:export
   :char-offset-to-point :current-pos :replace-text-between-positions
   :*weekday-names*
   :format-timestamp :format-inactive-timestamp-with-time
   :replace-submatch-text :replace-submatch-text*
   :find-parent-of-type :find-node-at-pos :find-node-at-point))

(in-package :organ/utils)

(defun char-offset-to-point (buf offset)
  (let ((start (lem:copy-point (lem:buffer-start-point buf) :temporary)))
    (lem:character-offset start offset)
    start))

(defun point-to-char-offset (point)
  (1- (lem:position-at-point point)))

(defun current-pos ()
  (point-to-char-offset (lem:current-point)))

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

(defun replace-submatch-text* (buffer submatch new-text)
  (replace-text-between-positions
   buffer
   (1+ (cltpt/combinator:match-begin-absolute submatch))
   (1+ (cltpt/combinator:match-end-absolute submatch))
   new-text))

(defun replace-submatch-text (buffer text-obj submatch-id new-text)
  (let* ((match (cltpt/base:text-object-match text-obj))
         (submatch (cltpt/combinator:find-submatch match submatch-id)))
    (replace-submatch-text*
     buffer
     submatch
     new-text)))

(defparameter *weekday-names*
  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  "short weekday names indexed by day-of-week (0=Sunday).")

(defun format-timestamp (&optional (timestamp (local-time:now)))
  "return a string like <2024-01-30 Tue> for the given TIMESTAMP."
  (let* ((base (local-time:format-timestring
                nil
                timestamp
                :format '(:year "-" (:month 2) "-" (:day 2))))
         (weekday (nth (local-time:timestamp-day-of-week timestamp)
                       *weekday-names*)))
    (format nil "<~A ~A>" base weekday)))

(defun format-inactive-timestamp-with-time (&optional (timestamp (local-time:now)))
  "return an inactive timestamp with time like [2024-01-30 Tue 15:02:11] for the given TIMESTAMP."
  (let* ((base (local-time:format-timestring
                nil
                timestamp
                :format '(:year "-" (:month 2) "-" (:day 2))))
         (time-str (local-time:format-timestring
                    nil
                    timestamp
                    :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))
         (weekday (nth (local-time:timestamp-day-of-week timestamp)
                       *weekday-names*)))
    (format nil "[~A ~A ~A]" base weekday time-str)))

(defun find-parent-of-type (node type)
  "walk up from NODE using `cltpt/base:text-object-parent' to find the first ancestor of TYPE."
  (loop for current = node then (cltpt/base:text-object-parent current)
        while current
        when (typep current type)
          return current))

(defun find-node-at-pos (tree pos type)
  "find the node of TYPE at the given character offset POS in TREE."
  (let ((node (cltpt/base:child-at-pos tree pos)))
    (find-parent-of-type node type)))

(defun find-node-at-point (tree point type)
  "find the node of TYPE at the given lem POINT in TREE."
  (find-node-at-pos tree (point-to-char-offset point) type))