(in-package :organ/organ-mode)

(defvar *organ-fold-placeholder* "..."
  "the string rendered in place of a folded element's hidden lines.")

(defgeneric organ-element-foldable-p (obj)
  (:documentation
   "whether OBJ can be folded. folding hides every line of OBJ after its first, which stays
visible with a placeholder standing in for the rest. `organ-element-fold' decides what folding it
actually does."))

(defgeneric organ-element-fold (obj buf)
  (:documentation
   "fold OBJ in BUF. returns the fold overlay, or NIL when there was nothing to hide.
the default folds all of OBJ after its first line.")
  (:method (obj buf)
    (organ-fold-region buf
                       (cltpt/base:text-object-begin-in-root obj)
                       (cltpt/base:text-object-end-in-root obj))))

;; by default nothing is foldable.
(defmethod organ-element-foldable-p ((obj cltpt/base:text-object))
  nil)

(defmethod organ-element-foldable-p ((obj cltpt/org-mode:org-header))
  t)

(defmethod organ-element-foldable-p ((obj cltpt/org-mode:org-block))
  t)

(defmethod organ-element-foldable-p ((obj cltpt/org-mode:org-src-block))
  t)

(defmethod organ-element-foldable-p ((obj cltpt/org-mode:org-drawer))
  t)

(defmethod organ-element-foldable-p ((obj cltpt/org-mode:org-prop-drawer))
  t)

(defmethod organ-element-foldable-p ((obj cltpt/org-mode:org-list))
  t)

(defun organ-fold-line-point (buf obj)
  "a point on OBJ's first line, the one line left visible when OBJ is folded."
  (organ/utils:char-offset-to-point buf (cltpt/base:text-object-begin-in-root obj)))

(defun organ-foldable-at-point ()
  "the innermost foldable element whose first line is the cursor's line, or NIL."
  (let ((tree (current-tree))
        (buf (lem:current-buffer))
        (pos (organ/utils:current-pos-no-newline)))
    (when tree
      (loop :for obj := (cltpt/base:child-at-pos tree pos)
              :then (cltpt/base:text-object-parent obj)
            :while obj
            :do (when (and (organ-element-foldable-p obj)
                           (lem:same-line-p (lem:current-point)
                                            (organ-fold-line-point buf obj)))
                  (return obj))))))

(defun organ-fold-overlays (&optional (buf (lem:current-buffer)))
  "the fold overlays currently present in BUF."
  (remove-if-not (lambda (ov) (lem:overlay-get ov :fold))
                 (lem:buffer-overlays buf)))

(defun organ-fold-overlay-on-line (line-point &optional (buf (lem:current-buffer)))
  "return the fold overlay whose visible line is LINE-POINT's line, or NIL."
  (find-if (lambda (ov)
             (lem:same-line-p (lem:overlay-start ov) line-point))
           (organ-fold-overlays buf)))

(defun organ-fold-would-hide-content-p (buf start end)
  "true when folding [START, END) would actually hide something.
mirrors the clamping `lem:place-region-placeholder-overlay' does, so that an element whose region
ends at the following line's start (a header with no body, for example) is not offered a fold that
would hide nothing and leave a stray placeholder."
  (let ((s (organ/utils:char-offset-to-point buf start))
        (e (organ/utils:char-offset-to-point buf end)))
    (lem:line-end s)
    (when (lem:start-line-p e)
      (lem:character-offset e -1))
    (lem:point< s e)))

(defun organ-fold-region (buf start end
                          &key (placeholder *organ-fold-placeholder*)
                               (cursor-behavior :move-out))
  "hide the lines of [START, END) (0-based char offsets) after the first, leaving PLACEHOLDER.
returns the fold overlay, or NIL when there was nothing to hide."
  (when (organ-fold-would-hide-content-p buf start end)
    (lem:place-region-placeholder-overlay
     (organ/utils:char-offset-to-point buf start)
     (organ/utils:char-offset-to-point buf end)
     :placeholder placeholder
     :cursor-behavior cursor-behavior
     :is-line-fold t)))

(lem:define-command organ-fold-toggle () ()
  "fold or unfold the innermost foldable element at point."
  (let* ((buf (lem:current-buffer))
         (obj (organ-foldable-at-point)))
    (if (null obj)
        (lem:editor-error "no foldable element at point.")
        (let ((existing (organ-fold-overlay-on-line (organ-fold-line-point buf obj) buf)))
          (if existing
              (lem:delete-overlay existing)
              (unless (organ-element-fold obj buf)
                (lem:editor-error "nothing to fold here.")))))))

(lem:define-command organ-unfold-all () ()
  "remove every fold in the current buffer."
  (mapc #'lem:delete-overlay (organ-fold-overlays)))

(defun organ-top-level-headers ()
  "the top-level org-headers of the current buffer's tree, in document order."
  (let ((tree (current-tree)))
    (when tree
      (remove-if-not (lambda (c) (typep c 'cltpt/org-mode:org-header))
                     (cltpt/base:text-object-children tree)))))

(lem:define-command organ-fold-all () ()
  "fold every top-level header subtree in the current buffer."
  (let ((buf (lem:current-buffer)))
    (dolist (header (organ-top-level-headers))
      ;; dont stack a second fold on a header already folded.
      (unless (organ-fold-overlay-on-line (organ-fold-line-point buf header) buf)
        (organ-element-fold header buf)))))

(lem:define-command organ-fold-cycle-global () ()
  "unfold everything if anything is folded, otherwise fold all top-level headers."
  (if (organ-fold-overlays)
      (organ-unfold-all)
      (organ-fold-all)))