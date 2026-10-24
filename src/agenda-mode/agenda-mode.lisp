(defpackage :organ/agenda-mode
  (:use :cl :lem :organ/agenda-mode/utils)
  (:export :agenda-mode-open :agenda-reload :interactive-render-node :agenda-mode :current-header))

(in-package :organ/agenda-mode)

(defvar *log-state-change*
  t
  "when non-nil, log state changes under the header.")

(defvar *agenda-tag-column*
  80
  "column at which entry tags are right-aligned in the agenda view.")

(lem:define-attribute agenda-keyword-attribute
  (t :foreground :base0e))

(lem:define-attribute agenda-time-attribute
  (t :foreground :base09))

(lem:define-attribute agenda-state-attribute
  (t :foreground :base0a))

(lem:define-attribute agenda-day-attribute
  (t :foreground :base07 :bold t))

;; TODO: this pattern is repeated multiple times in the codebase. DRY it.
(defmethod organ/outline-mode:interactive-render-node ((node cltpt/agenda:agenda-outline-node)
                                                       buffer
                                                       point
                                                       depth)
  (if (null (cltpt/agenda:agenda-outline-node-parent node))
      (let ((indent (make-string (* depth 2) :initial-element #\space)))
        (lem:insert-string point indent)
        (if (cltpt/tree/outline:could-expand node)
            (lem:insert-string point
                               (if (cltpt/tree/outline:should-expand node)
                                   "- "
                                   "+ "))
            (lem:insert-string point "  "))
        (let ((content-start-pos (lem:copy-point point :right-inserting)))
          (lem:insert-string point
                             (format nil "~A" (cltpt/tree/outline:outline-text node))
                             :attribute 'agenda-day-attribute)
          (let ((node-end-pos (lem:copy-point point :right-inserting)))
            (lem:insert-character point #\newline)
            (lem-core::set-clickable
             content-start-pos
             node-end-pos
             (lambda (window clicked-point)
               (organ/outline-mode::outline-expand-collapse-at-point clicked-point)))
            (lem:put-text-property content-start-pos node-end-pos :outline-node node))))
      ;; non-root node (not a day node): delegate to default
      (call-next-method)))

(lem/transient:define-transient *agenda-mode-keymap*
  :base organ/outline-mode:*outline-mode-keymap*
  :display-style :row
  :description "agenda-mode keymap"
  (:key "Return" :suffix 'agenda-mode-follow)
  (:key "r" :suffix 'agenda-reload)
  (:key "C-c C-s" :suffix 'agenda-schedule)
  (:key "C-c C-d" :suffix 'agenda-deadline))

(lem/transient:assign-transient-key organ/organ-mode:*organ-mode-keymap* "C-c C-t"
  (list :suffix ''agenda-mode-change-task-state))

(lem:define-major-mode agenda-mode organ/outline-mode:outline-mode
  (:name "agenda-mode"
   :keymap *agenda-mode-keymap*))

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode agenda-mode))
  (list *agenda-mode-keymap*))

(lem:define-command agenda-mode-follow () ()
  "open the agenda entry at point."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (multiple-value-bind (filepath text-obj) (task-record-location node)
      (when filepath
        (let ((buffer (lem:find-file-buffer filepath)))
          (lem:switch-to-buffer buffer)
          (lem:move-to-position (lem:current-point)
                                (1+ (cltpt/base:text-object-begin-in-root text-obj))))))))

(defun agenda-mode-open (agenda
                         &key
                         begin-ts end-ts
                         include-done (include-undated t)
                         first-repeat-only)
  (let ((buffer (lem:make-buffer "*agenda*")))
    (lem:change-buffer-mode buffer 'agenda-mode)
    (setf (lem:buffer-value buffer 'agenda) agenda)
    ;; remember how this view was built so `agenda-reload' can rebuild it.
    (setf (lem:buffer-value buffer 'agenda-begin-ts) begin-ts)
    (setf (lem:buffer-value buffer 'agenda-end-ts) end-ts)
    (setf (lem:buffer-value buffer 'agenda-include-done) include-done)
    (setf (lem:buffer-value buffer 'agenda-first-repeat-only) first-repeat-only)
    (setf (lem:buffer-value buffer 'agenda-include-undated) include-undated)
    (let ((forest (cltpt/agenda:build-agenda-forest
                   agenda
                   :begin-ts begin-ts
                   :end-ts end-ts
                   :include-done include-done
                   :first-repeat-only first-repeat-only
                   :include-undated include-undated)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest)
      (let ((point (lem:buffer-point buffer)))
        (lem:buffer-start point)
        (lem:switch-to-buffer buffer)
        (let ((line (find-line-for-current-time forest)))
          (when line
            (lem:move-to-line point line)
            ;; advance into the content area where :outline-node is set
            (lem:next-single-property-change point :outline-node)))))
    buffer))

(defun agenda-rebuild-buffer (buffer)
  "rebuild the agenda forest for BUFFER and re-render it.
returns the new forest. falls back to the stored agenda when `*organ-files*' is nil."
  (let* ((begin-ts (lem:buffer-value buffer 'agenda-begin-ts))
         (end-ts (lem:buffer-value buffer 'agenda-end-ts))
         (include-done (lem:buffer-value buffer 'agenda-include-done))
         (first-repeat-only (lem:buffer-value buffer 'agenda-first-repeat-only))
         (include-undated (lem:buffer-value buffer 'agenda-include-undated))
         (agenda (if organ/roam:*organ-files*
                     (cltpt/agenda:from-roamer (organ/roam:current-roamer)
                                               :include-tags organ/roam:*agenda-include-tags*
                                               :exclude-tags organ/roam:*agenda-exclude-tags*)
                     (lem:buffer-value buffer 'agenda))))
    (setf (lem:buffer-value buffer 'agenda) agenda)
    (setf (lem:buffer-value buffer 'agenda-include-undated) include-undated)
    (let ((forest (cltpt/agenda:build-agenda-forest
                   agenda
                   :begin-ts begin-ts
                   :end-ts end-ts
                   :include-done include-done
                   :first-repeat-only first-repeat-only
                   :include-undated include-undated)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest)
      forest)))

(defun agenda-restore-line (buffer line)
  (let ((point (lem:buffer-point buffer)))
    (lem:move-to-line point (min line (lem:buffer-nlines buffer)))
    ;; move into the node's content
    (lem:with-point ((line-end point))
      (lem:line-end line-end)
      (lem:next-single-property-change point :outline-node line-end))))

(lem:define-command agenda-reload () ()
  "rebuild the current agenda view, picking up file edits seen by the latest roam rescan."
  (let ((buffer (lem:current-buffer)))
    (unless (lem:mode-active-p buffer 'agenda-mode)
      (lem:editor-error "not in an agenda buffer."))
    (let ((line (lem:line-number-at-point (lem:current-point))))
      (agenda-rebuild-buffer buffer)
      (agenda-restore-line buffer line))))

(defun render-agenda-entry (node point depth task &key time-str prefix-keyword)
  "render one agenda line for TASK at POINT.
NODE: the display node.
TIME-STR: the preformatted timestamp.
PREFIX-KEYWORD: e.g. \"Scheduled:\".
this should stay in-sync with `cltpt/agenda::record-outline-text'."
  (let* ((indent (make-string (* depth 2) :initial-element #\space))
         (state-name (princ-to-string
                      (cltpt/agenda:state-name (cltpt/agenda:task-state task))))
         (title (cltpt/agenda:task-title task))
         (tags (cltpt/agenda:task-tags task)))
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
      (when time-str
        (lem:insert-string point time-str :attribute 'agenda-time-attribute)
        (lem:insert-string point " ----- "))
      (when prefix-keyword
        (lem:insert-string point prefix-keyword :attribute 'agenda-keyword-attribute)
        (lem:insert-string point (make-string (max 1 (- 13 (length prefix-keyword)))
                                              :initial-element #\space)))
      ;; state name without parentheses
      (lem:insert-string point state-name :attribute 'agenda-state-attribute)
      (lem:insert-string point " ")
      ;; title
      (lem:insert-string point title)
      ;; right-aligned tags
      (when tags
        (let* ((tag-str (format nil ":~{~A~^:~}:" tags))
               (tag-column *agenda-tag-column*)
               (current-col (lem:point-column point))
               (padding (max 1 (- tag-column current-col (length tag-str)))))
          (lem:insert-string point (make-string padding :initial-element #\space))
          (lem:insert-string point tag-str)))
      (let ((node-end-pos (lem:copy-point point :temporary)))
        (lem:insert-character point #\newline)
        (lem-core::set-clickable
         content-start-pos
         node-end-pos
         (lambda (window clicked-point)
           (declare (ignore window))
           (organ/outline-mode::outline-expand-collapse-at-point clicked-point)))
        (lem:put-text-property content-start-pos node-end-pos :outline-node node)))))

(defun render-task-record (node point depth &optional overdue-days)
  "render task-record NODE, OVERDUE-DAYS replaces the keyword for an overdue prefix."
  ;; this should be in-sync with `cltpt/agenda::record-outline-text'
  (labels ((format-ts (ts)
             (local-time:format-timestring
              nil
              ts
              :format cltpt/agenda:*agenda-time-format*))
           (format-time (time)
             (if (typep time 'cltpt/agenda:time-range)
                 (format nil
                         "~A--~A"
                         (format-ts (cltpt/agenda:time-range-begin time))
                         (format-ts (cltpt/agenda:time-range-end time)))
                 (format-ts time))))
    (let* ((task (cltpt/agenda:task-record-task node))
           (time (unless (or overdue-days (cltpt/agenda:task-record-date-only-p node))
                   (cltpt/agenda:task-record-time node)))
           (prefix-keyword (cond
                             ((cltpt/agenda:deadline node)
                              (if overdue-days
                                  (format nil "~2d d. ago:" overdue-days)
                                  "Deadline:"))
                             ((cltpt/agenda:start-task node)
                              (if overdue-days
                                  (format nil "Sched.~2dx:" overdue-days)
                                  "Scheduled:")))))
      (render-agenda-entry node
                           point
                           depth
                           task
                           :time-str (when time (format-time time))
                           :prefix-keyword prefix-keyword))))

(defmethod organ/outline-mode:interactive-render-node ((node cltpt/agenda:task-record)
                                                       buffer
                                                       point
                                                       depth)
  (render-task-record node point depth))

(defmethod organ/outline-mode:interactive-render-node ((node cltpt/agenda:agenda-overdue-node)
                                                       buffer
                                                       point
                                                       depth)
  (render-task-record (cltpt/agenda:agenda-overdue-node-record node)
                      point
                      depth
                      (cltpt/agenda:agenda-overdue-node-days node)))

(defmethod organ/outline-mode:interactive-render-node ((node cltpt/agenda:agenda-undated-node)
                                                       buffer
                                                       point
                                                       depth)
  (render-agenda-entry node point depth (cltpt/agenda:agenda-undated-node-task node)))

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
        (setf header
              (organ/utils:find-node-at-pos-ignore-newline
               (organ/organ-mode:current-tree)
               (organ/utils:current-pos)
               'cltpt/org-mode:org-header)))
    header))

(lem/transient:define-transient *todo-state-keymap*
  :display-style :row
  :description "todo-state keymap")

(defmethod lem:keymap-prefixes ((keymap (eql *todo-state-keymap*)))
  (let* ((header (current-header))
         (task (cltpt/agenda:text-object-task header))
         ;; (next-state (cltpt/agenda:cycle state))
         (seq-desc (if task
                       (cltpt/agenda:state-sequence-desc
                        (cltpt/agenda:task-state task))
                       (car cltpt/agenda::*agenda-seqs*)))
         (state-names
           (mapcar
            (lambda (x)
              (princ-to-string (cltpt/agenda:state-desc-name x)))
            (cltpt/agenda:state-sequence-desc-state-descs seq-desc))))
    (loop for state-name in state-names
          collect (lem:make-prefix
                   :key (car (parse-keyspec (string-downcase (string (char state-name 0)))))
                   :suffix state-name))))

(lem:define-command agenda-mode-change-task-state () ()
  (let ((header (current-header)))
    (if header
        (let ((task (cltpt/agenda:text-object-task header)))
          (let* ((new-state-name
                   (lem:with-special-keymap (*todo-state-keymap*)
                     (lem/transient::show-transient *todo-state-keymap*)
                     (lem:read-command)))
                 (new-state (when new-state-name
                              (cltpt/agenda:state-by-name new-state-name))))
            (when new-state
              (let ((new-state-name-str (princ-to-string (cltpt/agenda:state-name new-state)))
                    (is-terminal (cltpt/agenda:state-is-terminal new-state)))
                (if task
                    (let ((old-state-name (princ-to-string (cltpt/agenda:state-name
                                                            (cltpt/agenda:task-state task))))
                          (was-terminal (cltpt/agenda:state-is-terminal
                                         (cltpt/agenda:task-state task))))
                      (lem:message "state changed from ~A to ~A"
                                   old-state-name
                                   new-state-name-str)
                      ;; insert log entry
                      (when *log-state-change*
                        (organ/utils:insert-header-log-entry
                         (lem:current-buffer)
                         header
                         (format nil
                                 "- State ~A from ~A ~A"
                                 ;; we shouldnt hardcode the padding length but thats the way org-mode does it
                                 (pad-to (format nil "\"~A\"" new-state-name-str) 12)
                                 (pad-to (format nil "\"~A\"" old-state-name) 12)
                                 (organ/utils:format-inactive-timestamp-with-time))))
                      (if (and is-terminal (cltpt/agenda:task-repeating-p task))
                          ;; finishing a repeating task only hides occurrences up to now (like
                          ;; org-mode, LAST_REPEAT is the current time), the keyword stays as is.
                          (organ/utils:set-header-property
                           (lem:current-buffer)
                           header
                           "LAST_REPEAT"
                           (organ/utils:format-inactive-timestamp-with-time))
                          (progn
                            ;; properly place the CLOSED timestamp (on the action line)
                            (cond
                              ((and is-terminal (not was-terminal))
                               (insert-closed-timestamp (lem:current-buffer) header))
                              ((and was-terminal (not is-terminal))
                               (remove-closed-timestamp (lem:current-buffer) header)))
                            ;; TODO keyword replacement
                            (organ/utils:replace-submatch-text
                             (lem:current-buffer)
                             header
                             'cltpt/org-mode::todo-keyword
                             new-state-name-str))))
                    ;; no existing TODO keyword, insert before the title
                    (let* ((match (cltpt/base:text-object-match header))
                           (title-submatch (cltpt/combinator:find-submatch
                                            match
                                            'cltpt/org-mode::title))
                           (insert-pos (1+ (cltpt/combinator:match-begin-absolute title-submatch))))
                      (when is-terminal
                        (insert-closed-timestamp (lem:current-buffer) header))
                      (organ/utils:replace-text-between-positions
                       (lem:current-buffer)
                       insert-pos insert-pos
                       (format nil "~A " new-state-name-str))))))))
        (lem:editor-error "not under header"))))

(lem:define-command agenda-schedule () ()
  "prompt for a date and insert/update a SCHEDULED timestamp for the agenda entry at point."
  (multiple-value-bind (header source-buffer) (resolve-agenda-header-and-buffer)
    (organ/organ-mode::set-action-timestamp
     "SCHEDULED"
     header
     source-buffer
     'cltpt/agenda/task::record-scheduled
     "Rescheduled")))

(lem:define-command agenda-deadline () ()
  "prompt for a date and insert/update a DEADLINE timestamp for the agenda entry at point."
  (multiple-value-bind (header source-buffer) (resolve-agenda-header-and-buffer)
    (organ/organ-mode::set-action-timestamp
     "DEADLINE"
     header
     source-buffer
     'cltpt/agenda/task::record-deadline
     "New deadline")))

(defmethod lem/transient:mode-transient-keymap ((mode agenda-mode))
  *agenda-mode-keymap*)