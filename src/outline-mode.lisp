(defpackage :organ/outline-mode
  (:use :cl)
  (:export
   :open-outline
   :outline-mode
   :*outline-mode-keymap*
   :+outline-forest+
   :+outline-action-function+
   :set-outline-forest
   :get-outline-forest
   :set-outline-action-function
   :get-outline-action-function
   :render-outline
   :node-at-point
   :outline-node
   :outline-node-value
   :outline-node-children
   :outline-node-expanded-p
   :outline-node-parent
   :create-outline-node
   :outline-expand-collapse
   :outline-action-at-point
   :outline-quit
   :outline-next-sibling
   :outline-previous-sibling
   :outline-expand-collapse
   :get-outline-forest
   :interactive-render-node
   :outline-mode-toggle))

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
(lem:define-key *outline-mode-keymap* "M-n" 'outline-next-sibling)
(lem:define-key *outline-mode-keymap* "M-p" 'outline-previous-sibling)

;; this is like cltpt/tree/outline:render-tree but for lem
(defgeneric interactive-render-node (node buffer point depth click-handler)
  (:documentation "render a node interactively with clickable regions."))

(defgeneric outline-mode-toggle (node)
  (:documentation "toggle the expansion state of an outline node."))

;; data structure to represent a node in the outline
(defclass outline-node ()
  ((value
    :initarg :value
    :accessor outline-node-value)
   (children
    :initarg :children
    :initform nil
    :accessor outline-node-children)
   (expanded
    :initarg :expanded
    :initform nil
    :accessor outline-node-expanded-p)
   (parent
    :initarg :parent
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

(defmethod cltpt/tree/outline:could-expand ((node outline-node))
  "check if the node should be expanded in rendering (for static rendering)."
  (cltpt/tree:tree-children node))

(defmethod cltpt/tree/outline:outline-text ((node outline-node))
  (format nil "~a" (outline-node-value node)))

(defmethod interactive-render-node ((node t) buffer point depth click-handler)
  "render a node interactively with clickable regions."
  (lem:message "gothere2 ~A~%" node)
  (let* ((indent (make-string (* depth 2) :initial-element #\space))
         (line-start-pos (lem:copy-point point :right-inserting))) ;; mark the start of the line
    (lem:insert-string point indent)
    ;; add the tree connector symbol based on whether node has children
    (if (cltpt/tree/outline:could-expand node)
        (lem:insert-string point
                           (if (cltpt/tree/outline:should-expand node)
                               ;; "▼ "  ;; down arrow for expanded
                               ;; "▶ "  ;; right arrow for collapsed
                               "- "
                               "+ "))
        (lem:insert-string point "  ")) ;; indentation for leaf
    ;; insert the node text
    (lem:insert-string point
                       (format nil "~A" (cltpt/tree/outline:outline-text node)))
    (let ((node-end-pos (lem:copy-point point :left-inserting))) ;; save position before newline
      (lem:insert-character point #\newline)
      ;; set properties for the line
      (lem:put-text-property line-start-pos node-end-pos :clickable click-handler)
      (lem:put-text-property line-start-pos node-end-pos :outline-node node))))

(defmethod outline-mode-toggle ((node outline-node))
  (setf (outline-node-expanded-p node)
        (not (outline-node-expanded-p node))))

;; the default toggle does nothing perhaps.. for static outlines?
(defmethod outline-mode-toggle ((node t))
  nil)

(defmethod outline-mode-toggle ((node cltpt/agenda:agenda-outline-node))
  (if (equal (cltpt/agenda:agenda-outline-node-expansion-state node)
             'cltpt/agenda::expanded)
      (setf (cltpt/agenda:agenda-outline-node-expansion-state node) nil)
      (setf (cltpt/agenda:agenda-outline-node-expansion-state node)
            'cltpt/agenda::expanded)))

;; buffer value keys for storing outline data
(defparameter +outline-forest+ 'outline-forest)
(defparameter +outline-action-function+ 'outline-action-function)

;; function to get the outline forest for a buffer
(defun get-outline-forest (buffer)
  "get the outline forest associated with a buffer."
  (lem:buffer-value buffer +outline-forest+))

;; function to set the outline forest for a buffer
(defun set-outline-forest (buffer forest)
  "set the outline forest for a buffer."
  (setf (lem:buffer-value buffer +outline-forest+) forest))

;; function to set the outline action function for a buffer
(defun set-outline-action-function (buffer action-function)
  "set the outline action function for a buffer."
  (setf (lem:buffer-value buffer +outline-action-function+) action-function))

;; function to get the outline action function for a buffer
(defun get-outline-action-function (buffer)
  "get the outline action function for a buffer."
  (lem:buffer-value buffer +outline-action-function+))

;; helper function to create custom outline modes
(defun create-custom-outline-mode (mode-name key-bindings &optional mode-setup)
  "create a custom outline mode with inherited keymap and custom key bindings.
KEY-BINDINGS is a list of (key-string command-symbol) pairs.
MODE-SETUP is an optional function called when the mode is activated."
  (let ((keymap-name (intern (format nil "*~A-KEYMAP*" (string mode-name))))
        (mode-hook-name (intern (format nil "~A-HOOK" (string mode-name)))))
    `(progn
       (defvar ,keymap-name (lem:make-keymap :name ',keymap-name
                                             :parent *outline-mode-keymap*))
       ,@(mapcar (lambda (binding)
                   `(lem:define-key
                        ,keymap-name
                        ,(first binding)
                      ',(second binding)))
                 key-bindings)
       (defvar ,mode-hook-name nil)
       (lem:define-major-mode ,mode-name (outline-mode)
         (:name ,(string-downcase (string mode-name))
          :keymap ,keymap-name
          :mode-hook ,mode-hook-name)
         ,@(when mode-setup
             `((funcall ,mode-setup)))))))

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
  (when (cltpt/tree/outline:should-expand node)
    (dolist (child (cltpt/tree:tree-children node))
      (render-node point child (1+ depth)))))

;; command to expand/collapse the node at the current point
(lem:define-command outline-expand-collapse () ()
  "expand or collapse the node at the current point."
  (let ((point (lem:current-point)))
    (finish-output)
    (outline-expand-collapse-at-point point)
    (finish-output)))

;; command to perform an action on the node at the current point
(lem:define-command outline-action-at-point () ()
  "perform an action on the node at the current point."
  (let ((point (lem:current-point)))
    (let ((node (lem:text-property-at point :outline-node)))
      (when node
        (let ((action-fn (lem:buffer-value (lem:current-buffer) +outline-action-function+)))
          (if action-fn
              (funcall action-fn node)
              (lem:message "got: ~A"
                           (cltpt/tree/outline:outline-text node))))))))

(lem:define-command outline-quit () ()
  "kill the outline buffer."
  (lem:kill-buffer (lem:current-buffer)))

;; helper function to expand/collapse at a specific point
(defun outline-expand-collapse-at-point (point)
  "expand or collapse the node at the given point."
  (let* ((buffer (lem:point-buffer point))
         (line-num (lem:line-number-at-point point)))
    ;; find the node at this point
    (let ((node (lem:text-property-at point :outline-node)))
      (when node
        (outline-mode-toggle node)
        ;; re-render the outline after expanding/collapsing
        (render-outline buffer (get-outline-forest buffer))
        ;; restore cursor position based on the line number
        (lem:move-to-line (lem:buffer-point buffer) line-num)))))

;; main function to open an outline with the given forest
(defun open-outline (forest &key action-function)
  "open an outline buffer with the given forest structure.
ACTION-FUNCTION is an optional function that takes a node as argument
and is called when Return is pressed on a node."
  (let ((buffer (lem:make-buffer "*outline*")))
    (lem:change-buffer-mode buffer 'outline-mode)
    (set-outline-forest buffer forest)
    (when action-function
      (setf (lem:buffer-value buffer +outline-action-function+) action-function))
    (render-outline buffer forest)
    (lem:switch-to-buffer buffer)
    buffer))

(defun compute-node-depth (node)
  "compute depth by counting parent relationships."
  (let ((depth 0)
        (current node))
    (loop while (setf current (cltpt/tree:tree-parent current))
          do (incf depth))
    depth))

(defun get-node-depth (point)
  "get depth by finding the node and computing its depth."
  (let ((node (lem:text-property-at point :outline-node)))
    (when node
      (compute-node-depth node))))

(defun find-sibling-node (point direction)
  "find the next or previous sibling node."
  (let* ((current-depth (get-node-depth point))
         (current-line (lem:line-number-at-point point))
         (buffer (lem:point-buffer point))
         (max-line (lem:buffer-nlines buffer)))
    (when current-depth
      (ecase direction
        (:next
         (loop for line-num from (1+ current-line) to max-line
               for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
               do (when (and (lem:move-to-line temp-point line-num)
                             (lem:move-to-column temp-point 1))
                    (let ((node (lem:text-property-at temp-point :outline-node)))
                      (when (and node
                                 (= (compute-node-depth node) current-depth))
                        (return line-num))))))
        (:previous
         (loop for line-num from (1- current-line) downto 1
               for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
               do (when (and (lem:move-to-line temp-point line-num)
                             (lem:move-to-column temp-point 1))
                    (let ((node (lem:text-property-at temp-point :outline-node)))
                      (when (and node
                                 (= (compute-node-depth node) current-depth))
                        (return line-num))))))))))

(lem:define-command outline-next-sibling () ()
  "move to next node at same depth."
  (let* ((point (lem:current-point))
         (next-line (find-sibling-node point :next)))
    (when next-line
      (lem:move-to-line point next-line)
      (lem:move-to-column point 1))))

(lem:define-command outline-previous-sibling () ()
  "move to previous node at same depth."
  (let* ((point (lem:current-point))
         (prev-line (find-sibling-node point :previous)))
    (when prev-line
      (lem:move-to-line point prev-line)
      (lem:move-to-column point 1))))

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
    (open-outline (list node1 node2 node3)
                  :action-function
                  (lambda (node)
                    (lem:message "Custom action on: ~A"
                                 (cltpt/tree/outline:outline-text node))))))

;; make it work in vim-mode normal state
(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode outline-mode))
  (list *outline-mode-keymap*))