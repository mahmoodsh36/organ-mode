(in-package :organ/organ-mode)

(defvar *header-stars-attribute*
  (lem:make-attribute :foreground  "#FFFACD"))

(defvar *header-title-attribute*
  (lem:make-attribute :foreground  "#ab3b33"))

(defvar *header-todo-attribute*
  (lem:make-attribute :foreground  "#9d5ee0"))

(defvar *timestamp-attribute*
  (lem:make-attribute :foreground  "#9d5ee0"))

(defvar *block-attribute*
  (lem:make-attribute :foreground  "#FFFACD"))

(defvar *default-attribute*
  (lem:make-attribute :foreground  "#FFFACD"))

;; should return a list of attributes for lem for "syntax highlighting" in a buffer
;; technically could be used for more than just syntax highlighting
(defgeneric text-object-lem-overlays (text-obj buf))

(defun begin-in-root (obj)
  "alternative to `cltpt/base:text-object-begin-in-root' that makes use of :parent-pos property set by `organ-redraw-buffer'."
  (+ (cltpt/base:text-object-property obj :parent-pos)
     (cltpt/base:text-object-begin obj)))

(defun end-in-root (obj)
  "alternative to `cltpt/base:text-object-end-in-root' that makes use of :parent-pos property set by `organ-redraw-buffer'."
  (+ (cltpt/base:text-object-property obj :parent-pos)
     (cltpt/base:text-object-end obj)))

;; this takes an object, finds a cltpt/combinator "submatch", and returns an
;; overlay with the given attribute
(defun overlay-for-submatch (buf obj submatch-id attribute)
  "a function DRYing highilighting of submatches."
  (let* ((match (cltpt/base:text-object-match obj))
         (submatch (cltpt/combinator:find-submatch match submatch-id))
         (submatch-begin (cltpt/combinator:match-begin submatch))
         (submatch-end (cltpt/combinator:match-end submatch)))
    (when submatch
      (lem:make-overlay (organ/utils:char-offset-to-point buf submatch-begin)
                        (organ/utils:char-offset-to-point buf submatch-end)
                        attribute))))

;; like `overlay-for-submatch', except that it acts on all submatches found by id
(defun overlay-for-all-submatches (buf obj submatch-id attribute)
  "a function DRYing highilighting of submatches."
  (let* ((match (cltpt/base:text-object-match obj))
         (submatches (cltpt/combinator:find-submatch-all match submatch-id)))
    (loop for submatch in submatches
          for submatch-begin = (cltpt/combinator:match-begin submatch)
          for submatch-end = (cltpt/combinator:match-end submatch)
          collect (lem:make-overlay
                   (organ/utils:char-offset-to-point buf submatch-begin)
                   (organ/utils:char-offset-to-point buf submatch-end)
                   attribute))))

;; default function for syntax highlighting.
(defmethod text-object-lem-overlays ((obj cltpt/base:text-object) buf)
  (unless (typep obj 'cltpt/base::document)
    (let ((begin (begin-in-root obj))
          (end (end-in-root obj)))
      (list
       (lem:make-overlay (organ/utils:char-offset-to-point buf begin)
                         (organ/utils:char-offset-to-point buf end)
                         *default-attribute*)))))

;; consult cltpt/org-mode:*org-header-rule*
(defmethod text-object-lem-overlays ((obj cltpt/org-mode:org-header) buf)
  (remove-if-not
   #'identity
   (list
    (overlay-for-submatch buf obj 'cltpt/org-mode::title *header-title-attribute*)
    (overlay-for-submatch buf obj 'cltpt/org-mode::stars *header-stars-attribute*)
    (overlay-for-all-submatches buf
                                obj
                                'cltpt/org-mode::timestamp
                                *timestamp-attribute*)
    (overlay-for-all-submatches buf
                                obj
                                'cltpt/org-mode::todo-timestamp
                                *timestamp-attribute*)
    (overlay-for-submatch buf
                          obj
                          'cltpt/org-mode::todo-keyword
                          *header-todo-attribute*))))

;; consult cltpt/org-mode:*org-block-rule*
(defmethod text-object-lem-overlays ((obj cltpt/org-mode:org-block) buf)
  (list
   (overlay-for-submatch buf obj 'cltpt/org-mode::begin *block-attribute*)
   (overlay-for-submatch buf obj 'cltpt/org-mode::end *block-attribute*)))

(defun organ-redraw-buffer (buf)
  (lem:clear-overlays buf)
  (cltpt/base:map-text-object-with-pos-in-root
   (lem:buffer-value buf 'cltpt-tree)
   (lambda (obj parent-pos)
     (setf (cltpt/base:text-object-property obj :parent-pos) parent-pos)
     (loop for overlay in (text-object-lem-overlays obj buf)))))