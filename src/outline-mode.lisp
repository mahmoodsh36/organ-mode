(defpackage :organ/outline-mode
  (:use :cl)
  (:export :open-outline))

(in-package :organ/outline-mode)

(defvar *outline-mode-keymap* (lem:make-keymap))

(lem:define-major-mode outline-mode ()
    (:name "outline"
     :keymap *outline-mode-keymap*)
  (setf (lem:variable-value 'lem:line-wrap :buffer (lem:current-buffer)) nil)
  (setf (lem:buffer-read-only-p (lem:current-buffer)) t))

(lem:define-key *outline-mode-keymap* "Tab" 'outline-expand-collapse)
(lem:define-key *outline-mode-keymap* "C-c C-c" 'outline-expand-collapse)
(lem:define-key *outline-mode-keymap* "Return" 'outline-action-at-point)
(lem:define-key *outline-mode-keymap* "q" 'outline-quit)
(lem:define-key *outline-mode-keymap* "\t" 'outline-expand-collapse)

;; this is like cltpt/tree/outline:render-tree but for lem
(defgeneric interactive-render-node (node buffer point depth click-handler)
  (:documentation "render a node interactively with clickable regions."))

(defgeneric should-display-children (node)
  (:documentation "determine if children of a node should be displayed (different from should-expand in rendering)."))

;; data structure to represent a node in the outline
(defclass outline-node ()
  ((value :initarg :value
          :accessor outline-node-value)
   (children :initarg :children
             :initform nil
             :accessor outline-node-children)
   (expanded :initarg :expanded
             :initform nil
             :accessor outline-node-expanded-p)
   (parent :initarg :parent
           :initform nil
           :accessor outline-node-parent)))

(defun create-outline-node (value &key children (expanded nil) parent)
  "create an outline node with the given parameters."
  (make-instance 'outline-node
                 :value value
                 :children (or children nil)
                 :expanded expanded
                 :parent parent))

;; implement cltpt/tree interface for outline-node

(defmethod cltpt/tree:tree-value ((node outline-node))
  (outline-node-value node))

(defmethod cltpt/tree:tree-children ((node outline-node))
  (outline-node-children node))

(defmethod cltpt/tree:tree-parent ((node outline-node))
  (outline-node-parent node))

;; implement cltpt/tree/outline interface for outline-node

(defmethod cltpt/tree/outline:should-expand ((node outline-node))
  "check if the node should be expanded in rendering (for static rendering)."
  (outline-node-expanded-p node))

(defmethod cltpt/tree/outline:outline-text ((node outline-node))
  (format nil "~a" (outline-node-value node)))

;; Implement our extension interface for outline-node
(defmethod should-display-children ((node outline-node))
  "determine if children of a node should be displayed in the interactive view."
  (outline-node-expanded-p node))

(defmethod interactive-render-node ((node outline-node) buffer point depth click-handler)
  "render a node interactively with clickable regions."
  (let* ((indent (make-string (* depth 2) :initial-element #\space))
         (line-start-pos (lem:copy-point point :right-inserting))) ;; mark the start of the line
    (lem:insert-string point indent)
    ;; add the tree connector symbol based on whether node has children
    (if (cltpt/tree:tree-children node)
        (lem:insert-string point
                           (if (outline-node-expanded-p node)
                               "▼ "   ;; down arrow for expanded
                               "▶ ")) ;; right arrow for collapsed
        (lem:insert-string point "  ")) ; indentation for leaf
    ;; insert the node text
    (lem:insert-string point (format nil "~a" (cltpt/tree/outline:outline-text node)))
    (let ((node-end-pos (lem:copy-point point :left-inserting))) ;; save position before newline
      (lem:insert-character point #\newline)
      ;; set properties for the line
      (lem:put-text-property line-start-pos node-end-pos
                             :clickable click-handler)
      (lem:put-text-property line-start-pos node-end-pos
                             :outline-node node))))

;; default methods for any object implementing cltpt interfaces
;; this allows arbitrary trees that implement cltpt interfaces to work with our interactive system
(defmethod should-display-children ((node t))
  "default behavior: if it has cltpt/tree/outline:should-expand, use that, otherwise default to true."
  (if (and (typep node 'standard-object)
           (fboundp 'cltpt/tree/outline:should-expand))
      (cltpt/tree/outline:should-expand node)
      ;; default to showing children if not otherwise specified
      t))

(defmethod interactive-render-node ((node t) buffer point depth click-handler)
  "default renderer for any object implementing cltpt interfaces."
  (let* ((indent (make-string (* depth 2) :initial-element #\space))
         (line-start-pos (lem:copy-point point :right-inserting))) ;; mark the start of the line
    (lem:insert-string point indent)
    ;; add a generic indicator (we don't know if it has children in the static interface)
    (lem:insert-string point "• ")
    ;; insert the node text from cltpt interface
    (lem:insert-string point
                       (if (fboundp 'cltpt/tree/outline:outline-text)
                           (format nil "~a" (cltpt/tree/outline:outline-text node))
                           (format nil "~a" node))) ;; fallback to node itself
    (let ((node-end-pos (lem:copy-point point :left-inserting))) ;; save position before newline
      (lem:insert-character point #\newline)
      ;; set properties for the entire line
      (lem:put-text-property line-start-pos node-end-pos
                             :clickable click-handler)
      ;; store the node itself so we can interact with it
      (lem:put-text-property line-start-pos node-end-pos
                             :outline-node node))))

;; buffer value keys for storing outline data
(defparameter +outline-forest+ 'outline-forest)

;; function to get the outline forest for a buffer
(defun get-outline-forest (buffer)
  "get the outline forest associated with a buffer."
  (lem:buffer-value buffer +outline-forest+))

;; function to set the outline forest for a buffer
(defun set-outline-forest (buffer forest)
  "set the outline forest for a buffer."
  (setf (lem:buffer-value buffer +outline-forest+) forest))

;; function to expand a node
(defun expand-node (node)
  "mark a node as expanded."
  (setf (outline-node-expanded-p node) t))

;; function to collapse a node
(defun collapse-node (node)
  "mark a node as collapsed."
  (setf (outline-node-expanded-p node) nil))

;; function to toggle a node between expanded and collapsed
(defun toggle-node (node)
  "toggle the expansion state of a node."
  ;; for outline-node we have expansion state, for other nodes we'll need a different strategy
  (when (typep node 'outline-node)
    (if (outline-node-expanded-p node)
        (progn
          (collapse-node node)
          nil)
        (progn
          (expand-node node)
          t))))

;; function to find the node at the current line in the buffer
(defun node-at-point (point)
  "find the outline node at the given point in the buffer."
  (lem:text-property-at point :outline-node))

;; function to render the outline forest to the buffer
(defun render-outline (buffer forest)
  "render the outline forest to the buffer."
  (lem:with-buffer-read-only buffer nil
    (let ((lem:*inhibit-read-only* t))
      (lem:erase-buffer buffer)
      (let ((point (lem:buffer-point buffer)))
        (dolist (node forest)
          (render-node point node 0))))))

;; function to recursively render a node and its children
(defun render-node (point node depth)
  "Render a single node and its children recursively."
  (interactive-render-node
   node
   (lem:point-buffer point)
   point
   depth
   (lambda (window clicked-point)
     (declare (ignore window))
     (outline-expand-collapse-at-point clicked-point)))
  ;; if the node should display children, render its children
  (when (should-display-children node)
    (dolist (child (cltpt/tree:tree-children node))
      (render-node point child (1+ depth)))))

;; command to expand/collapse the node at the current point
(lem:define-command outline-expand-collapse () ()
  "expand or collapse the node at the current point."
  (let ((point (lem:current-point)))
    (format t "DEBUG: outline-expand-collapse called~%")
    (finish-output)
    (outline-expand-collapse-at-point point)))

;; command to perform an action on the node at the current point
(lem:define-command outline-action-at-point () ()
  "perform an action on the node at the current point."
  (let ((point (lem:current-point)))
    (let ((node (lem:text-property-at point :outline-node)))
      (when node
        (lem:message "got: ~a"
                 (if (fboundp 'cltpt/tree/outline:outline-text)
                     (cltpt/tree/outline:outline-text node)
                     (format nil "~a" node)))))))

(lem:define-command outline-quit () ()
  "kill the outline buffer."
  (lem:kill-buffer (lem:current-buffer)))

;; helper function to expand/collapse at a specific point
(defun outline-expand-collapse-at-point (point)
  "expand or collapse the node at the given point."
  (let* ((buffer (lem:point-buffer point))
         (line-num (lem:line-number-at-point point)))
    (finish-output)
    ;; find the node at this point
    (let ((node (lem:text-property-at point :outline-node)))
      (format t "node found: ~a~%" node)
      (finish-output)
      (when node
        (format t "toggling node: ~a~%"
                (if (fboundp 'cltpt/tree/outline:outline-text)
                    (cltpt/tree/outline:outline-text node)
                    (format nil "~a" node)))
        (finish-output)
        ;; toggle the node - this will work differently based on node type
        (let ((result (toggle-node node)))
          ;; for nodes that don't support expansion state, we might want different behavior
          (unless result
            (format t "DEBUG: node type doesn't support toggle, showing children~%")
            (finish-output)))
        ;; re-render the outline after expanding/collapsing
        (render-outline buffer (get-outline-forest buffer))
        ;; restore cursor position based on the line number
        (lem:move-to-line (lem:buffer-point buffer) line-num)
        (finish-output)))))

;; main function to open an outline with the given forest
(lem:define-command open-outline (forest)
  ((:lisp "forest: "))
  "open an outline buffer with the given forest structure."
  (let ((buffer (lem:make-buffer "*outline*")))
    (lem:change-buffer-mode buffer 'outline-mode)
    (set-outline-forest buffer forest)
    (render-outline buffer forest)
    (lem:switch-to-buffer buffer)
    buffer))

;; helper function to create a sample outline for testing
(defun create-sample-outline ()
  "create a sample outline for testing purposes."
  (let* ((child1 (create-outline-node "child 1"))
         (child2 (create-outline-node "child 2"))
         (subchild1 (create-outline-node "subchild 1"))
         (subchild2 (create-outline-node "subchild 1.2"))
         (node1 (create-outline-node "node 1"
                                     :children (list child1 child2)))
         (node2 (create-outline-node "node 2"))
         (node3 (create-outline-node "node 3"
                                     :children (list subchild1 subchild2))))
    ;; set up parent relationships
    (setf (outline-node-parent child1) node1
          (outline-node-parent child2) node1
          (outline-node-parent subchild1) node3
          (outline-node-parent subchild2) node3)
    (list node1 node2 node3)))

(lem:define-command open-sample-outline () ()
  "open a sample outline for testing purposes."
  (open-outline (create-sample-outline)))

;; command to create an outline with a simple test forest
(lem:define-command test-outline () ()
  "test command to open a sample outline."
  (let* ((subchild1 (create-outline-node "subchild 1.2.1"))
         (subchild2 (create-outline-node "subchild 1.2.2"))
         (child2 (create-outline-node "child 1.2" :children (list subchild1 subchild2)))
         (child1 (create-outline-node "child 1.1"))
         (node1 (create-outline-node "root 1" :children (list child1 child2)))
         (node2 (create-outline-node "root 2"))
         (child3 (create-outline-node "child 3.1"))
         (node3 (create-outline-node "root 3" :children (list child3))))
    ;; set up parent relationships
    (setf (outline-node-parent child1) node1
          (outline-node-parent child2) node1
          (outline-node-parent subchild1) child2
          (outline-node-parent subchild2) child2
          (outline-node-parent child3) node3)
    (open-outline (list node1 node2 node3))))

;; make it work in vim-mode normal state
(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode outline-mode))
  (list *outline-mode-keymap*))