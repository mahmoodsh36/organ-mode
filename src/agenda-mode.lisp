(defpackage :organ/agenda-mode
  (:use :cl)
  (:export :agenda-mode-open))

(in-package :organ/agenda-mode)

(defvar *agenda-mode-keymap*
  (lem:make-keymap :name '*agenda-mode-keymap*
                   :parent organ/outline-mode:*outline-mode-keymap*))

;; (lem:undefine-key *agenda-mode-keymap* "Return")
(lem:define-key *agenda-mode-keymap* "Return" 'agenda-mode-follow)

(lem:define-major-mode agenda-mode organ/outline-mode:outline-mode
  (:name "agenda-mode"
   :keymap *agenda-mode-keymap*))

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode agenda-mode))
  (list *agenda-mode-keymap*))

(lem:define-command agenda-mode-follow () ()
  "open the agenda entry at point."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (when (typep node 'cltpt/agenda:task-record)
      (let* ((rec node)
             (task (cltpt/agenda:task-record-task node))
             (roam-node (cltpt/agenda:task-node task))
             (filepath (cltpt/roam:node-file roam-node))
             (text-obj (cltpt/roam:node-text-obj roam-node))
             (pos (cltpt/base:text-object-begin-in-root text-obj))
             (buffer (lem:find-file-buffer filepath)))
        (lem:switch-to-buffer buffer)
        (lem:move-to-position (lem:current-point) pos)
        (lem:message "node1: ~A, ~A" pos text-obj)))))

(defun agenda-mode-open (agenda)
  (let ((buffer (lem:make-buffer "*agenda*")))
    (lem:change-buffer-mode buffer 'agenda-mode)
    (setf (lem:buffer-value buffer 'agenda) agenda)
    (let ((forest (cltpt/agenda:build-agenda-forest agenda)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest))
    (lem:switch-to-buffer buffer)
    buffer))

(lem:define-command agenda-mode-change-task-state () ()
  (let ((header))
    (if (lem:mode-active-p (lem:current-buffer) 'agenda-mode)
        (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
          (when (typep node 'cltpt/agenda:task-record)
            (let* ((rec node)
                   (task (cltpt/agenda:task-record-task node))
                   (roam-node (cltpt/agenda:task-node task))
                   (filepath (cltpt/roam:node-file roam-node))
                   (text-obj (cltpt/roam:node-text-obj roam-node))
                   (pos (1+ (cltpt/base:text-object-begin-in-root text-obj))))
              (setf header text-obj)
              (setf buffer (lem:find-file-buffer filepath))
              (lem:switch-to-buffer buffer)
              (lem:move-to-position (lem:current-point)
                                    pos))))
        (let ((text-obj (cltpt/base:child-at-pos
                         (organ/organ-mode:current-tree)
                         (organ/utils:current-pos))))
          (loop while text-obj
                until (typep text-obj 'cltpt/org-mode:org-header)
                do (setf text-obj (cltpt/base:text-object-parent text-obj)))
          (setf header text-obj)))
    (when header
      (let* ((pos (cltpt/base:text-object-begin-in-root header))
             ;; (match (cltpt/base:text-object-match header))
             (task (cltpt/agenda:text-object-task header))
             (state (cltpt/agenda:task-state task))
             ;; (next-state (cltpt/agenda:cycle state))
             (state-names
               (mapcar
                (lambda (x)
                  (princ-to-string (cltpt/agenda:state-desc-name x)))
                (cltpt/agenda:state-sequence-desc-state-descs
                 (cltpt/agenda:state-sequence-desc state))))
             (new-state-name
               (lem:prompt-for-string
                "state: "
                :completion-function (lambda (str)
                                       state-names)))
             (new-state (when new-state-name
                          (cltpt/agenda:state-by-name
                           new-state-name))))
        (when new-state
          (lem:message "changed from ~A to ~A"
                       (cltpt/agenda:state-name state)
                       (cltpt/agenda:state-name new-state))
          (organ/utils:replace-submatch-text
           (lem:current-buffer)
           header
           'cltpt/org-mode::todo-keyword
           ;; we're using princ-to-string since it can be a symbol.. (why tho?)
           (princ-to-string (cltpt/agenda:state-name new-state))))))))