(defpackage :organ/agenda-mode
  (:use :cl :lem)
  (:export :agenda-mode-open :interactive-render-node))

(in-package :organ/agenda-mode)

(lem:define-attribute *agenda-keyword-attribute*
  (t :foreground "blue" :background nil))

(lem:define-attribute *agenda-time-attribute*
  (t :foreground "red" :background nil))

(lem:define-attribute *agenda-state-attribute*
  (t :foreground "purple" :background nil))

(lem/transient:define-transient *agenda-mode-keymap*
  :base organ/outline-mode:*outline-mode-keymap*
  :display-style :row
  :description "organ-mode keymap"
  (:key "Return" :suffix 'agenda-mode-follow))

(lem:define-major-mode agenda-mode organ/outline-mode:outline-mode
  (:name "agenda-mode"
   :keymap *agenda-mode-keymap*))

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode agenda-mode))
  (list *agenda-mode-keymap*))

(defun task-record-location (node)
  "extract the file path and text object from a task-record NODE.
returns (values filepath text-obj) or nil if NODE is not a task-record."
  (when (typep node 'cltpt/agenda:task-record)
    (let* ((task (cltpt/agenda:task-record-task node))
           (roam-node (cltpt/agenda:task-node task))
           (filepath (cltpt/roam:node-file roam-node))
           (text-obj (cltpt/roam:node-text-obj roam-node)))
      (values filepath text-obj))))

(lem:define-command agenda-mode-follow () ()
  "open the agenda entry at point."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (multiple-value-bind (filepath text-obj) (task-record-location node)
      (when filepath
        (let ((buffer (lem:find-file-buffer filepath)))
          (lem:switch-to-buffer buffer)
          (lem:move-to-position (lem:current-point)
                                (cltpt/base:text-object-begin-in-root text-obj)))))))

(defun node-timestamp (node)
  "extract a comparable timestamp from a task-record's time.
returns nil if node has no time or is not a task-record."
  (when (typep node 'cltpt/agenda:task-record)
    (let ((time (cltpt/agenda:task-record-time node)))
      (when time
        (if (typep time 'cltpt/agenda:time-range)
            (cltpt/agenda:time-range-begin time)
            time)))))

;; doing things this way shouldnt be necessary if we were to keep track of which line contains
;; which entry during rendering, but it may be more modification-tolerant if we were to allow
;; modification to the buffer in the future.
(defun find-line-for-current-time (forest)
  "walk the forest in render order to find the line of the first task
at or after the current hour. returns a 1-indexed line number."
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

(defun agenda-mode-open (agenda &key begin-ts end-ts)
  (let ((buffer (lem:make-buffer "*agenda*")))
    (lem:change-buffer-mode buffer 'agenda-mode)
    (setf (lem:buffer-value buffer 'agenda) agenda)
    (let ((forest (cltpt/agenda:build-agenda-forest
                   agenda
                   :begin-ts begin-ts
                   :end-ts end-ts)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest)
      (lem:switch-to-buffer buffer)
      (let ((line (find-line-for-current-time forest)))
        (when line
          (let ((point (lem:buffer-point buffer)))
            (lem:move-to-line point line)
            ;; advance into the content area where :outline-node is set
            (lem:next-single-property-change point :outline-node)))))
    buffer))

(defmethod organ/outline-mode:interactive-render-node ((node cltpt/agenda:task-record)
                                                       buffer
                                                       point
                                                       depth)
  "render a task record in org-agenda style."
  (let* ((indent (make-string (* depth 2) :initial-element #\space)))
    ;; this should be in-sync with `(defmethod cltpt/tree/outline:outline-text ((rec task-record))'
    (labels ((format-ts (ts)
               (local-time:format-timestring
                nil
                ts
                :format cltpt/agenda:*agenda-time-format*))
             (format-time (time)
               (if (typep time 'cltpt/agenda:time-range)
                   (format nil "~A--~A"
                           (format-ts (cltpt/agenda:time-range-begin time))
                           (format-ts (cltpt/agenda:time-range-end time)))
                   (format-ts time))))
      (let* ((task1 (cltpt/agenda:task-record-task node))
             (time (cltpt/agenda:task-record-time node))
             (state-name (princ-to-string
                          (cltpt/agenda:state-name (cltpt/agenda:task-state task1))))
             (title (cltpt/agenda:task-title task1))
             (prefix-keyword (cond
                               ((cltpt/agenda:deadline node) "Deadline")
                               ((cltpt/agenda:start-task node) "Scheduled"))))
        (lem:insert-string point indent)
        ;; add the tree connector symbol based on whether node has children
        (if (cltpt/tree/outline:could-expand node)
            (lem:insert-string point
                               (if (cltpt/tree/outline:should-expand node)
                                   "- "
                                   "+ "))
            (lem:insert-string point "  "))
        ;; capture start position after indentation (clickable region starts here)
        (let ((content-start-pos (lem:copy-point point :temporary)))
          (when time
            (lem:insert-string point
                               (format-time time)
                               :attribute '*agenda-time-attribute*)
            (lem:insert-string point " ----- "))
          (when prefix-keyword
            (lem:insert-string point prefix-keyword :attribute '*agenda-keyword-attribute*)
            (lem:insert-string point ":   "))
          ;; state name without parentheses
          (lem:insert-string point state-name :attribute '*agenda-state-attribute*)
          (lem:insert-string point " ")
          ;; title
          (lem:insert-string point title)
          ;; right-aligned tags
          (let ((tags (cltpt/agenda:task-tags task1)))
            (when tags
              (let* ((tag-str (format nil ":~{~A~^:~}:" tags))
                     (tag-column 80)
                     (current-col (lem:point-column point))
                     (padding (max 1 (- tag-column current-col (length tag-str)))))
                (lem:insert-string point (make-string padding :initial-element #\space))
                (lem:insert-string point tag-str))))
          (let ((node-end-pos (lem:copy-point point :temporary)))
            (lem:insert-character point #\newline)
            (lem-core::set-clickable
             content-start-pos
             node-end-pos
             (lambda (window clicked-point)
               (declare (ignore window))
               (organ/outline-mode::outline-expand-collapse-at-point clicked-point)))
            (lem:put-text-property content-start-pos node-end-pos :outline-node node)))))))

(lem/transient:define-transient *todo-state-keymap*
  :display-style :row
  :description "todo-state keymap")

(defun current-header ()
  (let ((header))
    (if (lem:mode-active-p (lem:current-buffer) 'agenda-mode)
        (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
          (multiple-value-bind (filepath text-obj) (task-record-location node)
            (when filepath
              (setf header text-obj)
              (lem:switch-to-buffer (lem:find-file-buffer filepath))
              (lem:move-to-position (lem:current-point)
                                    (1+ (cltpt/base:text-object-begin-in-root text-obj))))))
        (let ((text-obj (cltpt/base:child-at-pos
                         (organ/organ-mode:current-tree)
                         (organ/utils:current-pos))))
          (loop while text-obj
                until (typep text-obj 'cltpt/org-mode:org-header)
                do (setf text-obj (cltpt/base:text-object-parent text-obj)))
          (setf header text-obj)))
    header))

(defmethod lem:keymap-children ((keymap (eql *todo-state-keymap*)))
  (let* ((header (current-header))
         (task (cltpt/agenda:text-object-task header))
         (state (cltpt/agenda:task-state task))
         ;; (next-state (cltpt/agenda:cycle state))
         (state-names
           (mapcar
            (lambda (x)
              (princ-to-string (cltpt/agenda:state-desc-name x)))
            (cltpt/agenda:state-sequence-desc-state-descs
             (cltpt/agenda:state-sequence-desc state)))))
    (loop for state-name in state-names
          collect (lem:make-prefix
                   :key (car (parse-keyspec (string-downcase (string (char state-name 0)))))
                   :suffix state-name))))

(lem:define-command agenda-mode-change-task-state () ()
  (let ((header (current-header)))
    (if header
        (let ((task (cltpt/agenda:text-object-task header)))
          (if task
              (let* ((state (cltpt/agenda:task-state task))
                     (new-state-name
                       (lem:with-special-keymap (*todo-state-keymap*)
                         (lem/transient::show-transient *todo-state-keymap*)
                         (lem:read-command)))
                     (new-state (when new-state-name
                                  (cltpt/agenda:state-by-name
                                   new-state-name))))
                (when new-state
                  (lem:message "state changed from ~A to ~A"
                               (cltpt/agenda:state-name state)
                               (cltpt/agenda:state-name new-state))
                  (organ/utils:replace-submatch-text
                   (lem:current-buffer)
                   header
                   'cltpt/org-mode::todo-keyword
                   (princ-to-string (cltpt/agenda:state-name new-state)))))
              (lem:message "this header contains no TODO data.")))
        (lem:message "not under header"))))

(defmethod lem/transient:mode-transient-keymap ((mode agenda-mode))
  *agenda-mode-keymap*)