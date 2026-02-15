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

(defun agenda-mode-open (agenda &key begin-ts end-ts)
  (let ((buffer (lem:make-buffer "*agenda*")))
    (lem:change-buffer-mode buffer 'agenda-mode)
    (setf (lem:buffer-value buffer 'agenda) agenda)
    (let ((forest (cltpt/agenda:build-agenda-forest
                   agenda
                   :begin-ts begin-ts
                   :end-ts end-ts)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest))
    (lem:switch-to-buffer buffer)
    buffer))

(defmethod organ/outline-mode:interactive-render-node ((node cltpt/agenda:task-record)
                                                       buffer
                                                       point
                                                       depth
                                                       click-handler)
  "render a node interactively with clickable regions."
  (let* ((indent (make-string (* depth 2) :initial-element #\space))
         ;; mark the start of the line
         (line-start-pos (lem:copy-point point :temporary)))
    (lem:insert-string point indent)
    ;; add the tree connector symbol based on whether node has children
    (if (cltpt/tree/outline:could-expand node)
        (lem:insert-string point
                           (if (cltpt/tree/outline:should-expand node)
                               "- "
                               "+ "))
        (lem:insert-string point "  ")) ;; indentation for leaf
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
      (let ((task1 (cltpt/agenda:task-record-task node))
            (prefix-keyword (cond
                              ((cltpt/agenda:deadline node)
                               "DEADLINE")
                              ((cltpt/agenda:start-task node)
                               "START"))))
        (when prefix-keyword
          (lem:insert-string point prefix-keyword :attribute '*agenda-keyword-attribute*)
          (lem:insert-string point ": "))
        (lem:insert-string point "(")
        (lem:insert-string
         point
         (princ-to-string (cltpt/agenda:state-name (cltpt/agenda:task-state task1)))
         :attribute '*agenda-state-attribute*)
        (lem:insert-string point ")")
        (lem:insert-string point " ")
        (lem:insert-string point
                           (format-time (cltpt/agenda:task-record-time node))
                           :attribute '*agenda-time-attribute*)
        (lem:insert-string point " ")
        (lem:insert-string point (cltpt/agenda:task-title task1))))
    (let ((node-end-pos (lem:copy-point point :temporary))) ;; save position before newline
      (lem:insert-character point #\newline)
      ;; set properties for the line
      (lem:put-text-property line-start-pos node-end-pos :clickable click-handler)
      (lem:put-text-property line-start-pos node-end-pos :outline-node node))))

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
  (log:info "got here4321")
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