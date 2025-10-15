(defpackage :organ-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools :cltpt)
  (:export :organ-mode))
(in-package :organ-mode)

;; incremental parsing doesnt yet work correctly.
;; while the functionality mostly already exists in 'cltpt', it hasnt been fully
;; ported to organ, and the buffer is redrawn on every change regardless.
(defvar *organ-enable-incremental-changes*
  nil
  "whether to enable incremental changes in `organ-mode'.")

(defvar *organ-mode-keymap*
  (make-keymap :name '*organ-mode-keymap* :parent *global-keymap*))
(defvar *organ-mode-navigation-keymap*
  (make-keymap :name '*organ-mode-keymap* :parent *global-keymap*))

(define-key *organ-mode-keymap* "C-l" *organ-mode-navigation-keymap*)
(define-keys *organ-mode-navigation-keymap*
  ("n" 'organ-next-element)
  ("p" 'organ-prev-element)
  )

(defvar *organ-syntax-table*
  (let ((table (make-syntax-table)))
    table))

(defvar *organ-mode-hook*
  '((organ-mode-init-all . 0)))

(define-major-mode organ-mode language-mode
  (:name "organ-mode"
   :keymap *organ-mode-keymap*
   :syntax-table *organ-syntax-table*
   :mode-hook *organ-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t))

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
    (setf (buffer-value buf 'cltpt-tree) cltpt-tree)))

(defun update-tree (start-point end-point length)
  "updates the organ-mode AST of the current buffer."
  (let* ((buf (lem:current-buffer))
         (buffer-contents (lem:buffer-text buf))
         (new-text (lem:points-to-string start-point end-point))
         (begin-pos (1- (lem:position-at-point start-point)))
         (end-pos (1- (lem:position-at-point end-point)))
         (cltpt-tree (buffer-value buf 'cltpt-tree)))
    (if *organ-enable-incremental-changes*
        (cltpt/base:handle-change cltpt-tree
                              cltpt/org-mode:*org-mode*
                              begin-pos
                              (lem:buffer-text buf))
        (setf (buffer-value buf 'cltpt-tree)
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
  (buffer-value (current-buffer) 'cltpt-tree))

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

(defun current-pos ()
  (1- (lem:position-at-point (lem:current-point))))

(define-command organ-next-element () ()
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (next (object-closest-after-pos tr pos))
         (new-pos (when next (cltpt/base:text-object-begin-in-root next))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(define-command organ-prev-element () ()
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (prev (object-closest-before-pos tr pos))
         (new-pos (when prev (cltpt/base:text-object-begin-in-root prev))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(defun delete-text-between-positions (buffer start-pos end-pos)
  "delete text between two absolute positions (indices) in the buffer."
  (when (> start-pos end-pos)
    (rotatef start-pos end-pos))
  (let ((start-point (copy-point (buffer-start-point buffer) :temporary))
        (end-point (copy-point (buffer-start-point buffer) :temporary)))
    (move-to-position start-point start-pos)
    (move-to-position end-point end-pos)
    (delete-between-points start-point end-point)))

(defun replace-text-between-positions (buffer start-pos end-pos replacement-text)
  "replace text between two absolute positions in the buffer with replacement text."
  (when (> start-pos end-pos)
    (rotatef start-pos end-pos))
  (let ((start-point (copy-point (buffer-start-point buffer) :temporary)))
    (unless (move-to-position start-point start-pos)
      (error "start position ~A is out of buffer bounds" start-pos))
    (let ((end-point (copy-point start-point :temporary)))
      (unless (move-to-position end-point end-pos)
        (error "end position ~A is out of buffer bounds" end-pos))
      (delete-between-points start-point end-point)
      (insert-string start-point replacement-text))))

(defun format-timestamp (&optional (timestamp (local-time:now)))
  "return a string like <2024-01-30 Tue> for the given TIMESTAMP."
  (let* ((base (local-time:format-timestring
                nil
                timestamp
                :format '(:year "-" (:month 2) "-" (:day 2))))
         (weekday (nth (local-time:timestamp-day-of-week timestamp)
                       '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
    (format nil "<~A ~A>" base weekday)))

(define-command edit-timestamp () ()
  "edit the timestamp at the cursor using `calendar-mode'."
  (let* ((obj (cltpt/base:child-at-pos (current-tree) (current-pos)))
         (source-buffer (current-buffer)))
    ;; (lem:message "DEBUG: ~A" (cltpt/tree/outline:render-tree obj))
    (if (typep obj 'cltpt/org-mode::org-timestamp)
        (organ/calendar-mode:calendar-with-callback
         (lambda (dates)
           (when dates
             (let ((new-date (car dates)))
               (with-current-buffer source-buffer
                 (replace-text-between-positions
                  source-buffer
                  (1+ (cltpt/base:text-object-begin-in-root obj))
                  (1+ (cltpt/base:text-object-end-in-root obj))
                  (format-timestamp new-date)))
               (lem:message "replaced ~A with ~A"
                            (cltpt/base:text-object-text obj)
                            new-date))))
         "*calendar*"
         nil)
        (lem:message "object under cursor (~A) isnt a timestamp."
                     (type-of obj)))))

(define-file-type ("org") organ-mode)