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

(defmethod prefix-active-p ((p (eql *swap-up-prefix*)))
  (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
        (header-found (find-header-at-title-line))
        (block-found  (find-block-at-delimiter-line)))
    (or table-found list-found header-found block-found)))

(defmethod prefix-suffix ((p (eql *swap-up-prefix*)))
  (lambda ()
    (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
          (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
          (header-found (find-header-at-title-line))
          (block-found  (find-block-at-delimiter-line)))
      (cond
        (table-found  (org-table-move-row table-found -1))
        (list-found   (org-list-move-item list-found -1))
        (header-found (org-header-move header-found -1))
        (block-found  (org-block-move block-found -1))))))

(defmethod prefix-active-p ((p (eql *swap-down-prefix*)))
  (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
        (header-found (find-header-at-title-line))
        (block-found  (find-block-at-delimiter-line)))
    (or table-found list-found header-found block-found)))

(defmethod prefix-suffix ((p (eql *swap-down-prefix*)))
  (lambda ()
    (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
          (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
          (header-found (find-header-at-title-line))
          (block-found  (find-block-at-delimiter-line)))
      (cond
        (table-found  (org-table-move-row table-found 1))
        (list-found   (org-list-move-item list-found 1))
        (header-found (org-header-move header-found 1))
        (block-found  (org-block-move block-found 1))))))

(defmethod prefix-active-p ((p (eql *swap-left-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *swap-left-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-move-column table-found -1)))))

(defmethod prefix-active-p ((p (eql *swap-right-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *swap-right-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-move-column table-found 1)))))

(defmethod prefix-active-p ((p (eql *return-prefix*)))
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found (find-node-at-current-pos 'cltpt/org-mode:org-list)))
    (or table-found
        (and list-found (lem:end-line-p (lem:current-point))))))

(defmethod prefix-active-p ((p (eql *tab-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *tab-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-navigate table-found 1 0)))))

(defmethod prefix-active-p ((p (eql *shift-tab-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *shift-tab-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-navigate table-found -1 0)))))

(defmethod prefix-suffix ((p (eql *return-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table))
          (list-found (find-node-at-current-pos 'cltpt/org-mode:org-list)))
      (cond
        (table-found (org-table-navigate table-found 0 1))
        (list-found (org-list-newline list-found))))))