(in-package :organ-mode)

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

;; should return a list of attributes for lem for "syntax highlighting" in a buffer
;; technically could be used for more than just syntax highlighting
(defgeneric text-object-lem-overlays (text-obj buf))

;; this takes an object, finds a cltpt/combinator "submatch", and returns an
;; overlay with the given attribute
(defun overlay-for-submatch (buf obj submatch-id attribute)
  "a function DRYing highilighting of submatches."
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (submatch (car (cltpt/base:find-submatch match submatch-id)))
         (submatch-begin (getf submatch :begin))
         (submatch-end (getf submatch :end)))
    (when submatch
      (lem:make-overlay (char-offset-to-point buf submatch-begin)
                        (char-offset-to-point buf submatch-end)
                        attribute))))

;; like `overlay-for-submatch', except that it acts on all submatches found by id
(defun overlay-for-all-submatches (buf obj submatch-id attribute)
  "a function DRYing highilighting of submatches."
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (submatches (cltpt/base:find-submatch-all match submatch-id)))
    (loop for submatch in submatches
          for submatch-begin = (getf (car submatch) :begin)
          for submatch-end = (getf (car submatch) :end)
          collect (lem:make-overlay
                   (char-offset-to-point buf submatch-begin)
                   (char-offset-to-point buf submatch-end)
                   attribute))))

;; default function for syntax highlighting.
(defmethod text-object-lem-overlays ((obj cltpt/base:text-object) buf)
  (unless (typep obj 'cltpt/base::document)
    (let ((begin (cltpt/base:text-object-begin-in-root obj))
          (end (cltpt/base:text-object-end-in-root obj)))
      (list
       (lem:make-overlay (char-offset-to-point buf begin)
                         (char-offset-to-point buf end)
                         (lem:make-attribute :foreground  "#FFFACD"))))))

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

(defun char-offset-to-point (buf offset)
  (let ((start (lem:copy-point (lem:buffer-start-point buf))))
    (lem:character-offset start offset)
    start))