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
  (lem:make-keymap :description '*organ-mode-keymap*))

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
    (setf (lem:buffer-value buf 'cltpt-tree)
          (cltpt/base:parse cltpt/org-mode:*org-mode* (lem:buffer-text buf)))
    (organ-redraw-buffer buf)
    ;; (lem:message "custom syntax highlighting triggered in ~A, size is ~A"
    ;;              (lem:buffer-name buf)
    ;;              (length (lem:buffer-text buf)))
    ))

(defun current-tree ()
  (lem:buffer-value (lem:current-buffer) 'cltpt-tree))

;; TODO: why are we accepting a text-object if we are using position-in-root anyway?
(defmethod object-closest-to-pos ((tree cltpt/base:text-object)
                                  pos
                                  direction
                                  &optional (predicate #'cltpt/base:tautology))
  "finds the text object closest to POS in DIRECTION (:forward or :backward).
for :forward, finds the object starting closest after POS.
for :backward, finds the object starting closest before POS."
  (let ((prune-test (ecase direction
                      (:forward #'<=)
                      (:backward #'>=)))
        (candidate-test (ecase direction
                          (:forward #'>)
                          (:backward #'<)))
        (better-test (ecase direction
                       (:forward #'<)
                       (:backward #'>)))
        (prune-accessor (ecase direction
                          (:forward #'cltpt/base:text-object-end-in-root)
                          (:backward #'cltpt/base:text-object-begin-in-root))))
    ;; prune: if the entire object is on the wrong side of pos, skip this branch.
    (when (funcall prune-test (funcall prune-accessor tree) pos)
      (return-from object-closest-to-pos nil))
    (let ((best-candidate))
      (when (and (funcall candidate-test (cltpt/base:text-object-begin-in-root tree) pos)
                 (funcall predicate tree))
        (setf best-candidate tree))
      (loop for child in (cltpt/base:text-object-children tree)
            do (let ((candidate (object-closest-to-pos child pos direction predicate)))
                 (when candidate
                   (if (or (null best-candidate)
                           (funcall better-test
                                    (cltpt/base:text-object-begin-in-root candidate)
                                    (cltpt/base:text-object-begin-in-root best-candidate)))
                       (setf best-candidate candidate)))))
      best-candidate)))

(defun organ-move-to-element (direction &optional (predicate #'cltpt/base:tautology))
  "move point to the nearest element in DIRECTION (:forward or :backward) that satisfies PREDICATE."
  (let* ((tr (current-tree))
         (pos (1- (lem:position-at-point (lem:current-point))))
         (target (object-closest-to-pos tr pos direction predicate))
         (new-pos (when target (cltpt/base:text-object-begin-in-root target))))
    (when new-pos
      (lem:move-point (lem:current-point)
                      (organ/utils:char-offset-to-point
                       (lem:current-buffer)
                       new-pos)))))

(lem:define-command organ-next-element () ()
  (organ-move-to-element :forward))

(lem:define-command organ-prev-element () ()
  (organ-move-to-element :backward))

(lem:define-command organ-next-header () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-header))))

(lem:define-command organ-prev-header () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-header))))

(lem:define-command organ-next-link () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-link))))

(lem:define-command organ-prev-link () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-link))))

(lem:define-command organ-next-src-block () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-src-block))))

(lem:define-command organ-prev-src-block () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-src-block))))

(lem:define-command organ-next-block () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-block))))

(lem:define-command organ-prev-block () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-block))))

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
     "date: "
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

(defmethod org-table-move ((text-obj cltpt/org-mode:org-table) x-shift y-shift)
  (let* ((match (cltpt/base:text-object-match text-obj))
         (table-str (cltpt/base:text-object-match-text text-obj match))
         (initial-pos (organ/utils:current-pos))
         (table-start-pos (cltpt/combinator:match-begin-absolute match))
         (cell (cltpt/tree:tree-find-if
                match
                (lambda (submatch)
                  (and (>= (1+ initial-pos) (cltpt/combinator:match-begin-absolute submatch))
                       (<= initial-pos (cltpt/combinator:match-end-absolute submatch))
                       (string= (cltpt/combinator:match-id submatch) 'table-cell)))))
         (effective-cell (or cell
                             (let ((best))
                               (cltpt/tree:tree-walk
                                match
                                (lambda (submatch)
                                  (when (and (>= (1+ initial-pos)
                                                 (cltpt/combinator:match-begin-absolute submatch))
                                             (string= (cltpt/combinator:match-id submatch)
                                                      'table-cell))
                                    (setf best submatch))))
                               best))))
    (if (not effective-cell)
        ;; if no cell found anywhere, just reformat and don't move the cursor.
        (let ((new-table-str (cltpt/org-mode::reformat-table table-str match)))
          (organ/utils:replace-submatch-text* (lem:current-buffer) match new-table-str))
        (let* ((current-coords (cltpt/org-mode::get-cell-coordinates effective-cell))
               (new-table-str (cltpt/org-mode::reformat-table table-str match)))
          (multiple-value-bind (new-table-match pos)
              (cltpt/org-mode::org-table-matcher
               nil
               (cltpt/reader:reader-from-string new-table-str)
               0)
            (let* ((width (cltpt/org-mode::get-table-width new-table-match))
                   (height (cltpt/org-mode::get-table-height new-table-match))
                   (current-linear (+ (* (cdr current-coords) width) (car current-coords)))
                   (raw-shift (+ x-shift (* y-shift width)))
                   (shift-linear (if (and (null cell) (< raw-shift 0))
                                     0
                                     raw-shift))
                   (target-linear (max 0 (+ current-linear shift-linear)))
                   (new-x (mod target-linear width))
                   (new-y (floor target-linear width))
                   ;; extend table if moving past last row, otherwise reuse
                   (final-table-str
                     (if (>= new-y height)
                         (cltpt/org-mode::nested-list-to-table-string
                          (append (cltpt/org-mode::table-match-to-nested-list new-table-str new-table-match)
                                  (loop repeat (- new-y (1- height))
                                        collect (loop repeat width collect ""))))
                         new-table-str))
                   (final-match
                     (if (string= final-table-str new-table-str)
                         new-table-match
                         (cltpt/org-mode::org-table-matcher
                          nil (cltpt/reader:reader-from-string final-table-str) 0)))
                   (final-height (cltpt/org-mode::get-table-height final-match))
                   (target-coords (cons new-x (min new-y (1- final-height))))
                   (target-cell (cltpt/org-mode::get-cell-at-coordinates final-match target-coords))
                   (cursor-pos (if target-cell
                                   (+ table-start-pos
                                      (cltpt/combinator:match-begin-absolute target-cell)
                                      1)
                                   table-start-pos)))
              (organ/utils:replace-submatch-text* (lem:current-buffer) match final-table-str)
              (lem:move-point (lem:current-point)
                              (organ/utils:char-offset-to-point
                               (lem:current-buffer) cursor-pos))))))))

;; detect tab and dispatch
(defmethod lem:execute :around ((mode *organ-mode*) command argument)
  (let* ((key-seq (lem:last-read-key-sequence))
         (first-key (first key-seq))
         (text-obj (current-text-obj))
         (table-found (loop for node = text-obj then (cltpt/base:text-object-parent node)
                            while node
                            when (typep node 'cltpt/org-mode:org-table)
                              return node)))
    (if text-obj
        (cond
          ((and (equal (lem-core::parse-keyspec "Shift-Tab") key-seq)
                table-found)
           (org-table-move table-found -1 0))
          ((and (equal (lem-core::parse-keyspec "Tab") key-seq)
                table-found)
           (org-table-move table-found 1 0))
          ((and (equal (lem-core::parse-keyspec "Return") key-seq)
                table-found)
           (org-table-move table-found 0 1))
          ;; delegate to default action
          (t (call-next-method)))
        (call-next-method))))

(lem:define-file-type ("org") *organ-mode*)