(in-package :organ/organ-mode)

(define-prefix *swap-up-prefix*
  :key "M-k"
  :behavior :drop
  :description "swap up (element-specific)")

(define-prefix *swap-down-prefix*
  :key "M-j"
  :behavior :drop
  :description "swap down (element-specific)")

(define-prefix *swap-right-prefix*
  :key "M-l"
  :behavior :drop
  :description "swap right (element-specific)")

(define-prefix *swap-left-prefix*
  :key "M-h"
  :behavior :drop
  :description "swap left (element-specific)")

;; this doesnt work properly yet, vi-mode's normal-mode return key takes priority over it.
(define-prefix *return-prefix*
  :key "Return"
  :behavior :drop
  :description "context-sensitive return")

(define-prefix *tab-prefix*
  :key "Tab"
  :behavior :drop
  :description "context-sensitive tab")

(define-prefix *shift-tab-prefix*
  :key "Shift-Tab"
  :behavior :drop
  :description "context-sensitive shift-tab")

(define-transient *organ-dwim-keymap*
  :description "dwim keymap"
  *swap-up-prefix*
  *swap-down-prefix*
  *swap-right-prefix*
  *swap-left-prefix*
  *return-prefix*
  *tab-prefix*
  *shift-tab-prefix*)

(lem:keymap-add-child *organ-mode-keymap* *organ-dwim-keymap*)

(defun vertical-move-context ()
  (let ((table  (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list   (find-node-at-current-pos 'cltpt/org-mode:org-list))
        (header (find-header-at-title-line))
        (blk  (find-block-at-delimiter-line)))
    (cond
      (table  (values :table table))
      (list   (values :list list))
      (header (values :header header))
      (blk    (values :block blk)))))

(defun vertical-move (direction)
  "dispatch a vertical move in DIRECTION (-1 or +1) based on the element at cursor."
  (multiple-value-bind (type element) (vertical-move-context)
    (when type
      (ecase type
        (:table  (org-table-move-row element direction))
        (:list   (org-list-move-item element direction))
        (:header (org-header-move element direction))
        (:block  (org-block-move element direction))))))

(defmethod prefix-active-p ((p (eql *swap-up-prefix*)))
  (vertical-move-context))

(lem:define-command organ-dwim-move-up () ()
  (vertical-move -1))

(defmethod prefix-suffix ((p (eql *swap-up-prefix*)))
  'organ-dwim-move-up)

(defmethod prefix-active-p ((p (eql *swap-down-prefix*)))
  (vertical-move-context))

(lem:define-command organ-dwim-move-down () ()
  (vertical-move 1))

(defmethod prefix-suffix ((p (eql *swap-down-prefix*)))
  'organ-dwim-move-down)

(defmethod prefix-active-p ((p (eql *swap-left-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(lem:define-command organ-dwim-move-left () ()
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
    (when table-found
      (org-table-move-column table-found -1))))

(defmethod prefix-suffix ((p (eql *swap-left-prefix*)))
  'organ-dwim-move-left)

(defmethod prefix-active-p ((p (eql *swap-right-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(lem:define-command organ-dwim-move-right () ()
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
    (when table-found
      (org-table-move-column table-found 1))))

(defmethod prefix-suffix ((p (eql *swap-right-prefix*)))
  'organ-dwim-move-right)

(defmethod prefix-active-p ((p (eql *return-prefix*)))
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found (find-node-at-current-pos 'cltpt/org-mode:org-list)))
    (or table-found
        (and list-found (lem:end-line-p (lem:current-point))))))

(defmethod prefix-active-p ((p (eql *tab-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(lem:define-command organ-dwim-tab () ()
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
    (when table-found
      (org-table-navigate table-found 1 0))))

(defmethod prefix-suffix ((p (eql *tab-prefix*)))
  'organ-dwim-tab)

(defmethod prefix-active-p ((p (eql *shift-tab-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(lem:define-command organ-dwim-shift-tab () ()
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
    (when table-found
      (org-table-navigate table-found -1 0))))

(defmethod prefix-suffix ((p (eql *shift-tab-prefix*)))
  'organ-dwim-shift-tab)

(lem:define-command organ-dwim-return () ()
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found (find-node-at-current-pos 'cltpt/org-mode:org-list)))
    (cond
      (table-found (org-table-navigate table-found 0 1))
      (list-found (org-list-newline list-found)))))

(defmethod prefix-suffix ((p (eql *return-prefix*)))
  'organ-dwim-return)