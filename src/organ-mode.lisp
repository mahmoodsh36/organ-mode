(defpackage :organ/organ-mode
  (:use :cl)
  (:export :organ-mode :current-tree))

(in-package :organ/organ-mode)

;; organ-redraw-buffer is written in highlighting.lisp
(declaim (ftype function organ-redraw-buffer))

;; incremental parsing doesnt yet work correctly.
;; while the functionality mostly already exists in 'cltpt', it hasnt been fully
;; ported to organ, and the buffer is redrawn on every change regardless.
(defvar *organ-enable-incremental-changes*
  nil
  "whether to enable incremental changes in `organ-mode'.")

(defvar *organ-mode-keymap*
  (lem:make-keymap :name '*organ-mode-keymap* :parent lem:*global-keymap*))
(defvar *organ-mode-navigation-keymap*
  (lem:make-keymap :name '*organ-mode-keymap* :parent lem:*global-keymap*))

(lem:define-key *organ-mode-keymap* "C-l" *organ-mode-navigation-keymap*)
(lem:define-keys *organ-mode-navigation-keymap*
  ("n" 'organ-next-element)
  ("p" 'organ-prev-element)
  )

(defvar *organ-mode-hook*
  '((organ-mode-init-all . 0)))

(lem:define-major-mode *organ-mode* nil
  (:name "organ-mode"
   :keymap *organ-mode-keymap*
   :mode-hook *organ-mode-hook*)
  (setf (lem:variable-value 'lem:enable-syntax-highlight) t))

(defun organ-mode-init-all ()
  "called when organ-mode is started, adds modification hooks to reparse buffer."
  (init-tree)
  (let ((buf (lem:current-buffer)))
    (lem:add-hook
     (lem:variable-value
      'lem:after-change-functions
      :buffer buf)
     'update-tree)))

(defun init-tree ()
  (let* ((buf (lem:current-buffer))
         (buffer-contents (lem:buffer-text buf))
         (cltpt-tree (cltpt/base:parse cltpt/org-mode:*org-mode* buffer-contents)))
    (setf (lem:buffer-value buf 'cltpt-tree) cltpt-tree)
    (organ-redraw-buffer buf)))

(defun update-tree (start-point end-point length)
  "updates the organ-mode AST of the current buffer."
  (let* ((buf (lem:current-buffer))
         (buffer-contents (lem:buffer-text buf))
         (new-text (lem:points-to-string start-point end-point))
         (begin-pos (1- (lem:position-at-point start-point)))
         (end-pos (1- (lem:position-at-point end-point)))
         (cltpt-tree (lem:buffer-value buf 'cltpt-tree)))
    (if *organ-enable-incremental-changes*
        (cltpt/base:handle-change cltpt-tree
                              cltpt/org-mode:*org-mode*
                              begin-pos
                              (lem:buffer-text buf))
        (setf (lem:buffer-value buf 'cltpt-tree)
              (cltpt/base:parse cltpt/org-mode:*org-mode* (lem:buffer-text buf))))
    ;; this is wrong
    ;; (cltpt/base:handle-changed-regions
    ;;  cltpt-tree
    ;;  cltpt/org-mode:*org-mode*
    ;;  (list (cons
    ;;         new-text
    ;;         (cltpt/base:make-region
    ;;          :begin begin-pos
    ;;          :end (+ begin-pos change-in-length))))
    ;;  t)
    (organ-redraw-buffer buf)
    ;; (lem:message "custom syntax highlighting triggered in ~A, size is ~A"
    ;;              (lem:buffer-name buf)
    ;;              (length (lem:buffer-text buf)))
    ))

(defun current-tree ()
  (lem:buffer-value (lem:current-buffer) 'cltpt-tree))

(defmethod object-closest-before-pos ((text-obj cltpt/base:text-object) pos)
  "finds the text object ending closest to but before POS."
  (let ((best-candidate))
    ;; the current object itself is a potential candidate if it ends before the cursor.
    (when (< (cltpt/base:text-object-end-in-root text-obj) pos)
      (setf best-candidate text-obj))
    ;; now check the children for a potentially better (closer) candidate.
    (loop for child in (cltpt/base:text-object-children text-obj)
          do
             ;; if a child starts at or after the cursor position,
             ;; then no subsequent sibling or descendant can be a valid "before" candidate.
             ;; we can stop searching entirely.
             (when (>= (cltpt/base:text-object-begin-in-root child) pos)
               (return-from object-closest-before-pos best-candidate))
             ;; recurse into the child to find the best candidate in that subtree.
             (let ((candidate-from-child (object-closest-before-pos child pos)))
               ;; if the recursive call found a better candidate (one that ends later), update our best.
               (when (and candidate-from-child
                          (> (cltpt/base:text-object-end-in-root
                              candidate-from-child)
                             (if best-candidate
                                 (cltpt/base:text-object-end-in-root best-candidate)
                                 -1)))
                 (setf best-candidate candidate-from-child))))
    best-candidate))

(defmethod object-closest-after-pos ((text-obj cltpt/base:text-object) pos)
  "finds the text object starting closest to but after POS using an efficient."
  ;; if the entire object ends before our position, it and all its
  ;; children are irrelevant. We can prune this entire branch.
  (when (<= (cltpt/base:text-object-end-in-root text-obj) pos)
    (return-from object-closest-after-pos nil))
  (let ((best-candidate))
    ;; step 1: The current object itself is a potential candidate.
    ;; if it starts after the cursor, it's our current best guess.
    (when (> (cltpt/base:text-object-begin-in-root text-obj) pos)
      (setf best-candidate text-obj))
    ;; step 2: now, search the children to see if one of them contains a better candidate.
    ;; a "better" candidate is one that also starts after `pos` but is closer (has a smaller start position).
    (loop for child in (cltpt/base:text-object-children text-obj)
          do
             ;; if the child starts after our current best candidate,
             ;; there's no point in searching it or any subsequent siblings, as they will
             ;; all be further away.
             (when (and best-candidate
                        (>= (cltpt/base:text-object-begin-in-root child)
                            (cltpt/base:text-object-begin-in-root best-candidate)))
               (return))
             (let ((candidate-from-child (object-closest-after-pos child pos)))
               ;; if the recursive call found a valid candidate in the child's branch...
               (when candidate-from-child
                 ;; ...and that candidate is better than our current best...
                 (if (or (null best-candidate)
                         (< (cltpt/base:text-object-begin-in-root
                             candidate-from-child)
                            (cltpt/base:text-object-begin-in-root best-candidate)))
                     ;; ...then it becomes the new best candidate.
                     (setf best-candidate candidate-from-child)))))
    best-candidate))

(lem:define-command organ-next-element () ()
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (next (object-closest-after-pos tr pos))
         (new-pos (when next (cltpt/base:text-object-begin-in-root next))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(lem:define-command organ-prev-element () ()
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (prev (object-closest-before-pos tr pos))
         (new-pos (when prev (cltpt/base:text-object-begin-in-root prev))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

;; this used calendar-mode.lisp directly instead of the popup
;; (lem:define-command edit-timestamp () ()
;;   "edit the timestamp at the cursor using `calendar-mode'."
;;   (let* ((obj (cltpt/base:child-at-pos (current-tree) (current-pos)))
;;          (source-buffer (lem:current-buffer)))
;;     ;; (lem:message "DEBUG: ~A" (cltpt/tree/outline:render-tree obj))
;;     (if (typep obj 'cltpt/org-mode::org-timestamp)
;;         (organ/calendar-mode:calendar-with-callback
;;          (lambda (dates)
;;            (when dates
;;              (let ((new-date (car dates)))
;;                (lem:with-current-buffer source-buffer
;;                  (organ/utils:replace-text-between-positions
;;                   source-buffer
;;                   (1+ (cltpt/base:text-object-begin-in-root obj))
;;                   (1+ (cltpt/base:text-object-end-in-root obj))
;;                   (organ/utils:format-timestamp new-date)))
;;                (lem:message "replaced ~A with ~A"
;;                             (cltpt/base:text-object-text obj)
;;                             new-date))))
;;          "*calendar*"
;;          nil)
;;         (lem:message "object under cursor (~A) isnt a timestamp."
;;                      (type-of obj)))))

(defun current-text-obj ()
  (let ((text-obj (cltpt/base:child-at-pos
                   (current-tree)
                   (organ/utils:current-pos))))
    text-obj))

(lem:define-command organ-insert-timestamp () ()
  "if there is a timestamp at the cursor, edit it using `popup-calendar', otherwise insert new timestamp."
  (let* ((obj (current-text-obj))
         (pt (lem:buffer-point (lem:current-buffer)))
         (source-buffer (lem:current-buffer)))
    (organ/popup-calendar:popup-calendar-with-callback
     (lambda (new-date)
       (when new-date
         (lem:with-current-buffer source-buffer
           (if (typep obj 'cltpt/org-mode::org-timestamp)
               (progn
                 (organ/utils:replace-text-between-positions
                  source-buffer
                  (1+ (cltpt/base:text-object-begin-in-root obj))
                  (1+ (cltpt/base:text-object-end-in-root obj))
                  (organ/utils:format-timestamp new-date))
                 (lem:message "replaced ~A with ~A"
                              (cltpt/base:text-object-text obj)
                              new-date))
               (lem:insert-string
                pt
                (organ/utils:format-timestamp new-date)))))))))

;; this currently only works for links that 'resolve' to filepaths
(lem:define-command organ-open-at-point () ()
  "open the link at point."
  (let* ((obj (cltpt/base:child-at-pos (current-tree)
                                       (organ/utils:current-pos)))
         (resolved (cltpt/base:text-link-resolve obj))
         (link (cltpt/base:text-link-link obj))
         (dest (cltpt/base:link-dest link))
         (source-buffer (lem:current-buffer))
         (cltpt/base:text-link-link obj))
    (when (typep obj 'cltpt/base::text-link)
      (let ((dest-filepath
              (typecase resolved
                (pathname (cltpt/file-utils:ensure-filepath-string resolved))
                (cltpt/roam:node (cltpt/roam:node-file resolved))
                (t dest))))
        (if (probe-file dest-filepath)
            (lem:find-file dest-filepath)
            (lem:message "file ~A doesnt exist" dest-filepath))))))

(defgeneric text-object-handle-tab (text-obj)
  (:documentation "tab was pressed over the text-object. handle it if need be."))

(defmethod text-object-handle-tab ((text-obj cltpt/base:text-object))
  nil)

;; tab in org-table should navigate to the next table, and possibly reorder
;; the table.
(defmethod text-object-handle-tab ((text-obj cltpt/org-mode::org-table))
  (let* ((match (cltpt/base:text-object-match text-obj))
         (pos (organ/utils:current-pos))
         (current-cell
           (cltpt/tree:tree-find-if
            match
            (lambda (submatch)
              (and (>= (1+ pos) (cltpt/combinator:match-begin submatch))
                   (<= pos (cltpt/combinator:match-end submatch))
                   (string= (cltpt/combinator:match-id submatch) 'table-cell)))))
         (current-cell-end (cltpt/combinator:match-end current-cell))
         ;; 'next-cell' (if any) is simply the cell that has a 'match-begin' greater than the
         ;; 'match-end' of 'current-cell'
         ;; TODO: this will not work if we re-order the table though..
         (next-cell (cltpt/tree:tree-find-if
                     match
                     (lambda (submatch)
                       (and (>= (cltpt/combinator:match-begin submatch) current-cell-end)
                            (string= (cltpt/combinator:match-id submatch) 'table-cell))))))
    ;; (lem:message "executing tab in table ~A" (type-of text-obj))
    ;; (lem:message "executing tab in table ~A" match)
    ;; (lem:message "executing tab in table ~A" current-cell)
    (lem:message "pos1 ~A" pos)
    (lem:message "pos2 ~A" (cltpt/combinator:match-begin current-cell))
    (lem:message "executing tab in table ~A" (cltpt/combinator:match-text current-cell))
    ;; (lem:message "executing tab in table ~A" pos)
    )
  t)

;; detect tab and dispatch
(defmethod lem:execute :around ((mode *organ-mode*) command argument)
  (let* ((key-seq (lem:last-read-key-sequence))
         (first-key (first key-seq))
         (text-obj (current-text-obj)))
    (if (and first-key
             text-obj
             (or (and (lem:key-p first-key)
                      (string= (lem:key-sym first-key) "Tab"))
                 (eql (lem:insertion-key-p first-key) #\Tab)))
        ;; if it returns `nil' we 'delegate' the action.
        (unless (text-object-handle-tab text-obj)
          (call-next-method))
        (call-next-method))))

(lem:define-file-type ("org") *organ-mode*)