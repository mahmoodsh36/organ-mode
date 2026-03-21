(in-package :organ/organ-mode)

(defun element-outer-range (text-obj)
  "return (beg-point end-point) for TEXT-OBJ."
  (let* ((begin (cltpt/base:text-object-begin-in-root text-obj))
         (end (cltpt/base:text-object-end-in-root text-obj))
         (buf (lem:current-buffer)))
    (values (organ/utils:char-offset-to-point buf begin)
            (organ/utils:char-offset-to-point buf end))))

(defun element-inner-range (text-obj)
  "return (beg-point end-point) for the contents region of TEXT-OBJ."
  (let* ((obj-begin (cltpt/base:text-object-begin-in-root text-obj))
         (contents-begin (+ obj-begin (cltpt/base:text-object-contents-begin text-obj)))
         (contents-end (+ obj-begin (cltpt/base:text-object-contents-end text-obj)))
         (buf (lem:current-buffer)))
    (values (organ/utils:char-offset-to-point buf contents-begin)
            (organ/utils:char-offset-to-point buf contents-end))))

(defun match-range (submatch &optional content-id)
  "return (beg-point end-point) for SUBMATCH.
when CONTENT-ID, use the child with that ID for the range instead."
  (let ((m (if content-id
               (cltpt/combinator/match:find-direct-match-child-by-id submatch content-id)
               submatch))
        (buf (lem:current-buffer)))
    (if m
        (values (organ/utils:char-offset-to-point buf (cltpt/combinator:match-begin-absolute m))
                (organ/utils:char-offset-to-point buf (cltpt/combinator:match-end-absolute m)))
        ;; no content child (empty cell/item): zero-width range inside the match.
        (let ((p (organ/utils:char-offset-to-point
                  buf
                  (1+ (cltpt/combinator:match-begin-absolute submatch)))))
          (values p (lem:copy-point p :temporary))))))

;; this maps a text-object type to the submatch id that should be considered for actions like 'vie'
;; element type -> (match-id content-child-id)
(defvar *submatch-element-types*
  (list (list 'cltpt/org-mode:org-table
              'cltpt/org-mode::table-cell
              'cltpt/org-mode::table-cell-content)
        (list 'cltpt/org-mode:org-list
              'cltpt/org-mode::list-item
              'cltpt/org-mode::list-item-content)))

(defun submatch-element-range (obj inner-p)
  "if OBJ is inside an element with submatch structure (table or list),
return (beg-point end-point) for the sub-element at cursor. returns nil if not applicable."
  (loop for (type match-id content-id) in *submatch-element-types*
        for parent = (organ/utils:find-parent-of-type obj type)
        when parent
          return (let ((submatch (find-submatch-at-pos parent match-id)))
                   (if submatch
                       (match-range submatch (when inner-p content-id))
                       (if inner-p
                           (element-inner-range parent)
                           (element-outer-range parent))))))

(defun element-range (obj inner-p)
  "return (beg-point end-point) for the element at cursor, drilling into table cells / list items."
  (multiple-value-bind (beg end) (submatch-element-range obj inner-p)
    (if beg
        (values beg end)
        (if inner-p
            (element-inner-range obj)
            (element-outer-range obj)))))

(lem-vi-mode/commands/utils:define-text-object-command vi-a-element () () ()
  (let ((obj (current-text-obj)))
    (if obj
        (multiple-value-bind (beg end) (element-range obj nil)
          (lem-vi-mode/core:make-range beg end))
        (lem:keyboard-quit))))

(lem-vi-mode/commands/utils:define-text-object-command vi-inner-element () () ()
  (let ((obj (current-text-obj)))
    (if obj
        (multiple-value-bind (beg end) (element-range obj t)
          (lem-vi-mode/core:make-range beg end))
        (lem:keyboard-quit))))

(lem-vi-mode/commands/utils:define-text-object-command vi-a-whole-element () () ()
  (let ((obj (current-text-obj)))
    (if obj
        (multiple-value-bind (beg end) (element-outer-range obj)
          (lem-vi-mode/core:make-range beg end))
        (lem:keyboard-quit))))

(lem-vi-mode/commands/utils:define-text-object-command vi-inner-whole-element () () ()
  (let ((obj (current-text-obj)))
    (if obj
        (multiple-value-bind (beg end) (element-inner-range obj)
          (lem-vi-mode/core:make-range beg end))
        (lem:keyboard-quit))))

(lem:define-key lem-vi-mode/states:*outer-text-objects-keymap* "e" 'vi-a-element)
(lem:define-key lem-vi-mode/states:*inner-text-objects-keymap* "e" 'vi-inner-element)
;; whole element (no considering a submatch like table cells / list items)
(lem:define-key lem-vi-mode/states:*outer-text-objects-keymap* "E" 'vi-a-whole-element)
(lem:define-key lem-vi-mode/states:*inner-text-objects-keymap* "E" 'vi-inner-whole-element)