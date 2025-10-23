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

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode agenda-mode))
  (list *agenda-mode-keymap*))