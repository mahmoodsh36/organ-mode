(defpackage :organ/agenda-mode
  (:use :cl)
  (:export :agenda-mode-open))

(in-package :organ/agenda-mode)

(defvar *agenda-mode-keymap*
  (lem:make-keymap :name '*agenda-mode-keymap*
                   :parent organ/outline-mode:*outline-mode-keymap*))

(lem:define-key *agenda-mode-keymap* "Return" 'agenda-mode-follow)

(lem:define-major-mode agenda-mode organ/outline-mode:outline-mode
  (:name "agenda-mode"
   :keymap *agenda-mode-keymap*))

(lem:define-command agenda-mode-follow () ()
  "open the agenda entry at point."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (lem:message "node: ~A" node)))

(defun agenda-mode-open (agenda)
  (let ((buffer (lem:make-buffer "*agenda*")))
    (lem:change-buffer-mode buffer 'agenda-mode)
    (setf (lem:buffer-value buffer 'agenda) agenda)
    (let ((forest (cltpt/agenda:build-agenda-forest agenda)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest))
    (lem:switch-to-buffer buffer)
    buffer))