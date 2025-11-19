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

(lem:define-keys *organ-mode-keymap*
  ("C-c n" 'organ-next-element)
  ("C-c p" 'organ-prev-element)
  ("C-c C-n" 'organ-next-header)
  ("C-c C-p" 'organ-prev-header)
  ("C-c C-x C-n" 'organ-next-link)
  ("C-c C-x C-p" 'organ-prev-link)
  ("C-c C-v C-n" 'organ-next-src-block)
  ("C-c C-v C-p" 'organ-prev-src-block))

(defvar *organ-mode-hook*
  '((organ-mode-init-all . 0)))

(lem:define-major-mode *organ-mode* nil
  (:name "organ-mode"
   :keymap *organ-mode-keymap*
   :mode-hook *organ-mode-hook*)
  (setf (lem:variable-value 'lem:enable-syntax-highlight) t))

(defvar *org-table-add-row-on-tab* t
  "when non-nil, pressing Tab in the last cell of an org-table will
reformat the table and add a new empty row. When nil, it will only
reformat the table and the cursor will remain in the last cell.")

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

;; TODO: why are we accepting a text-object if we are using position-in-root anyway?
(defmethod object-closest-after-pos ((tree cltpt/base:text-object)
                                     pos
                                     &optional (predicate #'cltpt/base:tautology))
  "finds the text object starting closest to but after POS using an efficient."
  ;; if the entire object ends before our position, it and all its children are irrelevant. we can
  ;; prune this entire branch.
  (when (<= (cltpt/base:text-object-end-in-root tree) pos)
    (return-from object-closest-after-pos nil))
  (let ((best-candidate))
    ;; the current object itself is a potential candidate. if it starts after the cursor,
    ;; it's our current best guess.
    (when (and (> (cltpt/base:text-object-begin-in-root tree) pos)
               (funcall predicate tree))
      (setf best-candidate tree))
    ;; search the children to see if one of them contains a better candidate. a "better" candidate
    ;; is one that also starts after `pos` but is closer (has a smaller start position).
    (loop for child in (cltpt/base:text-object-children tree)
          do (let ((candidate-from-child (object-closest-after-pos child pos predicate)))
               (when candidate-from-child
                 (if (or (null best-candidate)
                         (< (cltpt/base:text-object-begin-in-root candidate-from-child)
                            (cltpt/base:text-object-begin-in-root best-candidate)))
                     ;; no need to check predicate here because its guaranteed to have done
                     ;; by the recursive call to the child.
                     (setf best-candidate candidate-from-child)))))
    best-candidate))

(defmethod object-closest-before-pos ((tree cltpt/base:text-object)
                                       pos
                                       &optional (predicate #'cltpt/base:tautology))
  "finds the text object ending closest to but before POS using an efficient search."
  (when (>= (cltpt/base:text-object-begin-in-root tree) pos)
    (return-from object-closest-before-pos nil))
  (let ((best-candidate))
    (when (and (< (cltpt/base:text-object-begin-in-root tree) pos)
               (funcall predicate tree))
      (setf best-candidate tree))
    (loop for child in (cltpt/base:text-object-children tree)
          do (let ((candidate-from-child (object-closest-before-pos child pos predicate)))
               (when candidate-from-child
                 (if (or (null best-candidate)
                         (> (cltpt/base:text-object-begin-in-root candidate-from-child)
                            (cltpt/base:text-object-begin-in-root best-candidate)))
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

(defun organ-next-element-of-type (type)
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (next (object-closest-after-pos
                tr
                pos
                (lambda (obj)
                  (typep obj type))))
         (new-pos (when next
                    (cltpt/base:text-object-begin-in-root next))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(defun organ-prev-element-of-type (type)
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (prev (object-closest-before-pos
                tr
                pos
                (lambda (obj)
                  (typep obj type))))
         (new-pos (when prev
                    (cltpt/base:text-object-begin-in-root prev))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(lem:define-command organ-next-header () ()
  (organ-next-element-of-type 'cltpt/org-mode:org-header))

(lem:define-command organ-prev-header () ()
  (organ-prev-element-of-type 'cltpt/org-mode:org-header))

(lem:define-command organ-next-link () ()
  (organ-next-element-of-type 'cltpt/org-mode:org-link))

(lem:define-command organ-prev-link () ()
  (organ-prev-element-of-type 'cltpt/org-mode:org-link))

(lem:define-command organ-next-src-block () ()
  (organ-next-element-of-type 'cltpt/org-mode:org-src-block))

(lem:define-command organ-prev-src-block () ()
  (organ-prev-element-of-type 'cltpt/org-mode:org-src-block))

(lem:define-command organ-next-block () ()
  (organ-next-element-of-type 'cltpt/org-mode:org-block))

(lem:define-command organ-prev-block () ()
  (organ-prev-element-of-type 'cltpt/org-mode:org-block))

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

(defmethod text-object-handle-tab ((text-obj cltpt/org-mode::org-table))
  (let* ((match (cltpt/base:text-object-match text-obj))
         (initial-pos (organ/utils:current-pos))
         (table-start-pos (cltpt/combinator:match-begin match)))
    (let ((cell (cltpt/tree:tree-find-if
                 match
                 (lambda (submatch)
                   (and (>= (1+ initial-pos) (cltpt/combinator:match-begin submatch))
                        (<= initial-pos (cltpt/combinator:match-end submatch))
                        (string= (cltpt/combinator:match-id submatch) 'table-cell))))))
      (if (not cell)
          ;; if not in a cell, just reformat and don't move the cursor.
          (let ((new-table-str (cltpt/org-mode::reformat-table match)))
            (organ/utils:replace-submatch-text* (lem:current-buffer) match new-table-str))
          (let* ((current-coords (cltpt/org-mode::get-cell-coordinates cell))
                 (new-table-str (cltpt/org-mode::reformat-table match)))
            (multiple-value-bind (new-table-match new-pos-ignored)
                (cltpt/org-mode::org-table-matcher nil new-table-str 0)
              (multiple-value-bind (final-string-to-insert final-cursor-pos)
                  (let ((next-coords (cltpt/org-mode::get-next-data-cell-coords
                                      new-table-match
                                      current-coords)))
                    (if next-coords
                        ;; there is a next cell
                        (let* ((next-cell (cltpt/org-mode::get-cell-at-coordinates
                                           new-table-match
                                           next-coords))
                               (pos (+ table-start-pos
                                       (cltpt/combinator:match-begin next-cell)
                                       1)))
                          (values new-table-str pos))
                        ;; no next cell (at the last cell)
                        (if *org-table-add-row-on-tab*
                            ;; add a new row
                            (let* ((col-widths
                                     (let ((widths (make-array 0 :adjustable t :fill-pointer t)))
                                       (dolist (row (cltpt/org-mode::table-match-to-nested-list
                                                     new-table-match
                                                     nil))
                                         (loop for cell-text in row for i from 0 do
                                           (when (>= i (length widths))
                                             (vector-push-extend 0 widths))
                                           (setf (aref widths i)
                                                 (max (aref widths i)
                                                      (length cell-text)))))
                                       widths))
                                   (new-row-str
                                     (with-output-to-string (s)
                                       (write-char cltpt/org-mode::*table-v-delimiter* s)
                                       (loop for width across col-widths
                                             do (format s " ~vA ~c"
                                                        width
                                                        ""
                                                        cltpt/org-mode::*table-v-delimiter*))))
                                   (final-str (concatenate 'string new-table-str (string #\newline) new-row-str))
                                   (pos (+ table-start-pos (length new-table-str) 1 2)))
                              (values final-str pos))
                            ;; stay in the last cell
                            (let* ((last-cell (cltpt/org-mode::get-cell-at-coordinates
                                               new-table-match
                                               current-coords))
                                   (pos (+ table-start-pos
                                           (cltpt/combinator:match-begin last-cell)
                                           1)))
                              (values new-table-str pos)))))
                (organ/utils:replace-submatch-text*
                 (lem:current-buffer)
                 match
                 final-string-to-insert)
                (lem:move-point (lem:current-point)
                                (organ/utils:char-offset-to-point (lem:current-buffer)
                                                                  final-cursor-pos))))))))
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