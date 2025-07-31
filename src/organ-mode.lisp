(defpackage :organ-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools :cltpt)
  (:export :organ-mode))
(in-package :organ-mode)

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
  (update-tree nil nil nil)
  (let ((buf (lem:current-buffer)))
    (lem:add-hook
     (lem:variable-value
      'lem:after-change-functions
      :buffer buf)
     'update-tree)))

(defun highlight-region (start-point end-point color-string)
  (lem:make-overlay start-point
                    end-point
                    (lem:make-attribute :background color-string)))

(defun update-tree (start-point end-point length)
  "updates the organ-mode AST of the current buffer."
  (let* ((buf (lem:current-buffer))
         (buffer-contents (lem:buffer-text buf))
         (cltpt-tree (cltpt/base:parse buffer-contents
                                       (cltpt/org-mode:org-mode-text-object-types)
                                       :doc-type 'cltpt/org-mode::org-document)))
    (setf (buffer-value buf 'cltpt-tree) cltpt-tree)
    (lem:clear-overlays buf)
    (cltpt/base:map-text-object
     cltpt-tree
     (lambda (obj)
       (loop for overlay in (text-object-lem-overlays obj buf))))
    ;; (lem:message "custom syntax highlighting triggered in ~A, size is ~A"
    ;;              (lem:buffer-name buf)
    ;;              (length (lem:buffer-text buf)))
    ))

(defun current-tree ()
  (buffer-value (current-buffer) 'cltpt-tree))

(defmethod child-closest-before-pos ((text-obj cltpt/base:text-object) pos)
  "find the leaf-most child that is closest to but before the index POS."
  (let ((best-candidate))
    (loop for child in (cltpt/base:text-object-children text-obj)
          do ;; only consider children that end before the target position
             (when (< (cltpt/base:region-end
                       (cltpt/base:text-object-text-region child))
                      pos)
               ;; recursively search within this child to find the most
               ;; deeply nested candidate.
               (let* ((recursive-candidate (child-closest-before-pos child pos))
                      ;; the best candidate from this branch is either the deeper one we just found,
                      ;; or if there isn't one, the child itself.
                      (current-candidate (or recursive-candidate child)))
                 ;; compare this branch's candidate with the best one we've found so far.
                 (if (null best-candidate)
                     (setf best-candidate current-candidate)
                     (when (> (cltpt/base:region-end
                               (cltpt/base:text-object-text-region
                                current-candidate))
                              (cltpt/base:region-end
                               (cltpt/base:text-object-text-region
                                best-candidate)))
                       (setf best-candidate current-candidate))))))
    best-candidate))

(defmethod child-closest-after-pos ((text-obj cltpt/base:text-object) pos)
  "find the leaf-most child that is closest to but after the index POS."
  (let ((best-candidate))
    (loop for child in (cltpt/base:text-object-children text-obj)
          do ;; only consider children that start after the target position
             (when (> (cltpt/base:region-begin
                       (cltpt/base:text-object-text-region child))
                      pos)
               ;; recursively search within this child to find the most
               ;; deeply nested candidate.
               (let* ((recursive-candidate (child-closest-after-pos child pos))
                      ;; the best candidate from this branch is either the deeper one we just found,
                      ;; or if there isn't one, the child itself.
                      (current-candidate (or recursive-candidate child)))
                 ;; compare this branch's candidate with the best one we've found so far.
                 (if (null best-candidate)
                     (setf best-candidate current-candidate)
                     (when (< (cltpt/base:region-begin
                               (cltpt/base:text-object-text-region
                                current-candidate))
                              (cltpt/base:region-begin
                               (cltpt/base:text-object-text-region
                                best-candidate)))
                       (setf best-candidate current-candidate))))))
    best-candidate))

(define-command organ-next-element () ()
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (next (child-closest-after-pos tr pos))
         (new-pos (when next (cltpt/base:text-object-begin-in-root next))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(define-command organ-prev-element () ()
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (prev (child-closest-before-pos tr pos))
         (new-pos (when prev (cltpt/base:text-object-begin-in-root prev))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(define-file-type ("org") organ-mode)