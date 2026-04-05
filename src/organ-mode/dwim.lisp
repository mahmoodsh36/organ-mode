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

(define-prefix *shift-swap-left-prefix*
  :key "M-H"
  :behavior :drop
  :description "shift-swap left (subtree)")

(define-prefix *shift-swap-right-prefix*
  :key "M-L"
  :behavior :drop
  :description "shift-swap right (subtree)")

(define-transient *organ-dwim-keymap*
  :description "dwim keymap"
  *swap-up-prefix*
  *swap-down-prefix*
  *swap-right-prefix*
  *swap-left-prefix*
  *shift-swap-left-prefix*
  *shift-swap-right-prefix*
  *return-prefix*
  *tab-prefix*
  *shift-tab-prefix*)

;; ideally this should be assigned in `*organ-dwim-keymap*' but this would cause issues with
;; preexisting C-c bindings in the parent `*organ-mode-keymap*' since it would focus the dwim
;; keymap when C-c is pressed.
(lem/transient:define-transient-key
 *ctrl-c-ctrl-c-prefix*
 *organ-mode-keymap*
 "C-c C-c"
 :behavior :drop
 :description "context-sensitive C-c C-c")

(lem:keymap-add-child *organ-mode-keymap* *organ-dwim-keymap*)

(defun vertical-move-context ()
  (let ((table  (current-text-obj-ignore-newline 'cltpt/org-mode:org-table))
        (list   (current-text-obj-ignore-newline 'cltpt/org-mode:org-list))
        (header (find-header-at-title-line))
        (blk    (find-block-at-delimiter-line)))
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

(defun horizontal-move-context ()
  "return (values type element) for the element at cursor suitable for horizontal moves."
  (let ((table (current-text-obj-ignore-newline 'cltpt/org-mode:org-table))
        (list (current-text-obj-ignore-newline 'cltpt/org-mode:org-list))
        (header (find-header-at-title-line)))
    (cond
      (table (values :table table))
      (list (values :list list))
      (header (values :header header)))))

(defun list-obj-for-dedent (list-found)
  "return the appropriate list object for dedent. walks up to parent org-list if nested."
  (let ((parent-list (organ/utils:find-parent-of-type
                      (cltpt/base:text-object-parent list-found)
                      'cltpt/org-mode:org-list)))
    (or parent-list list-found)))

(defun list-obj-for-checkbox-toggle (list-found)
  "return the outermost containing org-list for checkbox propagation."
  (loop with current = list-found
        for parent-list = (organ/utils:find-parent-of-type
                           (cltpt/base:text-object-parent current)
                           'cltpt/org-mode:org-list)
        while parent-list
        do (setf current parent-list)
        finally (return current)))

(defmethod prefix-active-p ((p (eql *swap-left-prefix*)))
  (horizontal-move-context))

(lem:define-command organ-dwim-move-left () ()
  (multiple-value-bind (type element) (horizontal-move-context)
    (when type
      (ecase type
        (:table (org-table-move-column element -1))
        (:list (org-list-dedent-item (list-obj-for-dedent element)))
        (:header (org-header-level-increase element -1))))))

(defmethod prefix-suffix ((p (eql *swap-left-prefix*)))
  'organ-dwim-move-left)

(defmethod prefix-active-p ((p (eql *swap-right-prefix*)))
  (horizontal-move-context))

(lem:define-command organ-dwim-move-right () ()
  (multiple-value-bind (type element) (horizontal-move-context)
    (when type
      (ecase type
        (:table (org-table-move-column element 1))
        (:list (org-list-indent-item element))
        (:header (org-header-level-increase element 1))))))

(defmethod prefix-suffix ((p (eql *swap-right-prefix*)))
  'organ-dwim-move-right)

(defmethod prefix-active-p ((p (eql *shift-swap-left-prefix*)))
  (or (current-text-obj-ignore-newline 'cltpt/org-mode:org-list)
      (find-header-at-title-line)))

(lem:define-command organ-dwim-shift-move-left () ()
  (let ((list-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-list))
        (header (find-header-at-title-line)))
    (cond
      (list-found (org-list-dedent-tree (list-obj-for-dedent list-found)))
      (header (org-header-level-increase-tree header -1)))))

(defmethod prefix-suffix ((p (eql *shift-swap-left-prefix*)))
  'organ-dwim-shift-move-left)

(defmethod prefix-active-p ((p (eql *shift-swap-right-prefix*)))
  (or (current-text-obj-ignore-newline 'cltpt/org-mode:org-list)
      (find-header-at-title-line)))

(lem:define-command organ-dwim-shift-move-right () ()
  (let ((list-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-list))
        (header (find-header-at-title-line)))
    (cond
      (list-found (org-list-indent-tree list-found))
      (header (org-header-level-increase-tree header 1)))))

(defmethod prefix-suffix ((p (eql *shift-swap-right-prefix*)))
  'organ-dwim-shift-move-right)

(defmethod prefix-active-p ((p (eql *return-prefix*)))
  (let ((table-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-table))
        (list-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-list)))
    (or table-found
        (and list-found (lem:end-line-p (lem:current-point))))))

(defmethod prefix-active-p ((p (eql *tab-prefix*)))
  (current-text-obj-ignore-newline 'cltpt/org-mode:org-table))

(lem:define-command organ-dwim-tab () ()
  (let ((table-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-table)))
    (when table-found
      (org-table-navigate table-found 1 0))))

(defmethod prefix-suffix ((p (eql *tab-prefix*)))
  'organ-dwim-tab)

(defmethod prefix-active-p ((p (eql *shift-tab-prefix*)))
  (current-text-obj-ignore-newline 'cltpt/org-mode:org-table))

(lem:define-command organ-dwim-shift-tab () ()
  (let ((table-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-table)))
    (when table-found
      (org-table-navigate table-found -1 0))))

(defmethod prefix-suffix ((p (eql *shift-tab-prefix*)))
  'organ-dwim-shift-tab)

(lem:define-command organ-dwim-return () ()
  (let ((table-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-table))
        (list-found (current-text-obj-ignore-newline 'cltpt/org-mode:org-list)))
    (cond
      (table-found (org-table-navigate table-found 0 1))
      (list-found (org-list-newline)))))

(defmethod prefix-suffix ((p (eql *return-prefix*)))
  'organ-dwim-return)

(defmethod prefix-active-p ((p (eql *ctrl-c-ctrl-c-prefix*)))
  (current-text-obj-ignore-newline 'cltpt/org-mode:org-list))

(lem:define-command organ-ctrl-c-ctrl-c () ()
  "context-sensitive command bound to C-c C-c."
  (let ((list-obj (current-text-obj-ignore-newline 'cltpt/org-mode:org-list)))
    (cond
      (list-obj (org-list-toggle-checkbox (list-obj-for-checkbox-toggle list-obj)))
      (t (lem:editor-error "nothing to do here.")))))

(defmethod prefix-suffix ((p (eql *ctrl-c-ctrl-c-prefix*)))
  'organ-ctrl-c-ctrl-c)