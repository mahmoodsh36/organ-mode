(defpackage :organ/agenda-mode/utils
  (:use :cl :lem)
  (:export
   :task-record-location
   :node-timestamp
   :find-line-for-current-time
   :pad-to
   :insert-closed-timestamp
   :remove-closed-timestamp
   :resolve-agenda-header-and-buffer))

(in-package :organ/agenda-mode/utils)

(defun task-record-location (node)
  "extract the file path and text object from a task-record NODE.
returns (values filepath text-obj) or nil if NODE is not a task-record."
  (when (typep node 'cltpt/agenda:task-record)
    (let* ((task (cltpt/agenda:task-record-task node))
           (roam-node (cltpt/agenda:task-node task))
           (filepath (cltpt/roam:node-file roam-node))
           (text-obj (cltpt/roam:node-text-obj roam-node)))
      (values filepath text-obj))))

(defun node-timestamp (node)
  "extract a comparable timestamp from a task-record's time.
returns nil if node has no time or is not a task-record."
  (when (typep node 'cltpt/agenda:task-record)
    (let ((time (cltpt/agenda:task-record-time node)))
      (when time
        (if (typep time 'cltpt/agenda:time-range)
            (cltpt/agenda:time-range-begin time)
            time)))))

;; doing things this way wouldnt be necessary if we were to keep track of which line contains
;; which entry during rendering, but it may be more modification-tolerant if we were to allow
;; modification to the buffer in the future.
(defun find-line-for-current-time (forest)
  "walk the forest in render order to find the line of the first task at or after the current hour. returns a 1-indexed line number."
  (let ((now (local-time:now))
        (current-line 0)
        (last-timed-line))
    (labels ((walk (node)
               (incf current-line)
               (let ((ts (node-timestamp node)))
                 (when ts
                   (when (local-time:timestamp<= ts now)
                     (setf last-timed-line current-line))))
               (when (cltpt/tree/outline:should-expand node)
                 (dolist (child (cltpt/tree:tree-children node))
                   (walk child)))))
      (dolist (root forest)
        (walk root)))
    last-timed-line))

(defun pad-to (str width)
  "right-pad STR with spaces to WIDTH characters."
  (let ((len (length str)))
    (if (>= len width)
        str
        (concatenate 'string
                     str
                     (make-string (- width len) :initial-element #\space)))))

(defun insert-closed-timestamp (buffer header)
  "insert CLOSED: [timestamp] on the action line of HEADER."
  (organ/utils:append-header-action
   buffer
   header
   (format nil
           "CLOSED: ~A"
           (organ/utils:format-inactive-timestamp-with-time))))

(defun remove-closed-timestamp (buffer header)
  "remove an existing CLOSED: [...] entry from HEADER's action line, if present."
  (organ/utils:remove-header-action buffer header "CLOSED"))

(defun resolve-agenda-header-and-buffer ()
  "resolve the org-header and source buffer for the agenda entry at point.
returns (values header source-buffer) or signals an editor-error."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (multiple-value-bind (filepath text-obj) (task-record-location node)
      (if (and filepath text-obj)
          (let* ((source-buffer (lem:find-file-buffer filepath))
                 (header (organ/utils:find-parent-of-type
                          text-obj
                          'cltpt/org-mode:org-header)))
            (if header
                (values header source-buffer)
                (lem:editor-error "not inside an org-header.")))
          (lem:editor-error "not inside an org-header.")))))