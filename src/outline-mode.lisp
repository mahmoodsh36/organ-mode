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

   :create-outline-node
   :outline-expand-collapse
   :outline-action-at-point
   :outline-quit
   :outline-next-sibling
   :outline-previous-sibling
   :outline-next-true-sibling
   :outline-previous-true-sibling
   :outline-go-to-parent
   :outline-expand-collapse
   :get-outline-forest
   :interactive-render-node
   :outline-mode-toggle))

(in-package :organ/outline-mode)

;; TODO: navigation operations often take linear time because we dont check for parents explicitly
;; this can be alleviated by trying to execute cltpt/tree:tree-parent and resort to linear search
;; if that errors out. the linear search is nice because it allows us not to have to require
;; that parents be stored explicitly which makes some of the code simpler, especially in cases
;; where a single tree contains different types of nodes.

(defvar *outline-mode-keymap* (lem:make-keymap))

(lem:define-major-mode outline-mode ()
    (:name "outline"
     :keymap *outline-mode-keymap*)
  (setf (lem:variable-value 'lem:line-wrap :buffer (lem:current-buffer)) nil)
  (setf (lem:buffer-read-only-p (lem:current-buffer)) t))

(lem:define-key *outline-mode-keymap* "Tab" 'outline-expand-collapse)
(lem:define-key *outline-mode-keymap* "\t" 'outline-expand-collapse)
(lem:define-key *outline-mode-keymap* "Return" 'outline-action-at-point)
(lem:define-key *outline-mode-keymap* "q" 'outline-quit)
(lem:define-key *outline-mode-keymap* "M-n" 'outline-next-true-sibling)
(lem:define-key *outline-mode-keymap* "M-p" 'outline-previous-true-sibling)
(lem:define-key *outline-mode-keymap* "C-M-n" 'outline-next-sibling)
(lem:define-key *outline-mode-keymap* "C-M-p" 'outline-previous-sibling)
(lem:define-key *outline-mode-keymap* "M-u" 'outline-go-to-parent)

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
    :accessor outline-node-expanded-p)))

(defun create-outline-node (value &key children (expanded nil))
  "create an outline node with the given parameters."
  (make-instance 'outline-node
                 :value value
                 :children (or children nil)
                 :expanded expanded))

;; implement cltpt/tree interface for outline-node

(defmethod cltpt/tree:tree-value ((node outline-node))
  (outline-node-value node))

(defmethod cltpt/tree:tree-children ((node outline-node))
  (outline-node-children node))

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
       (defvar ,keymap-name (lem:make-keymap :description ',keymap-name
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
  "render a single node and its children recursively."
  (interactive-render-node
   node
    (lem:current-buffer)
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
  (let* ((buffer (lem:current-buffer))
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

(defun find-node-in-tree (target-node tree)
  "find a node in the tree and return (values node parent depth)."
  (labels ((find-in-node (current-node parent depth)
             (cond ((eq current-node target-node)
                    (values current-node parent depth))
                   ((cltpt/tree:tree-children current-node)
                    (dolist (child (cltpt/tree:tree-children current-node))
                      (multiple-value-bind (found found-parent found-depth)
                          (find-in-node child current-node (1+ depth))
                        (when found
                          (return-from find-node-in-tree
                            (values found found-parent found-depth)))))))))
    (dolist (root tree)
      (multiple-value-bind (found parent depth)
          (find-in-node root nil 0)
        (when found
          (return-from find-node-in-tree
            (values found parent depth)))))))

(defun compute-node-depth (node &optional buffer)
  "compute depth by finding the node in the tree."
  (let ((target-buffer (or buffer (lem:current-buffer)))
        (forest (get-outline-forest (or buffer (lem:current-buffer)))))
    (when forest
      (multiple-value-bind (found-node parent depth)
          (find-node-in-tree node forest)
        depth))))

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

(defun find-true-sibling-node (point direction)
  "finds next or previous true sibling node (same parent)."
  (let* ((current-node (lem:text-property-at point :outline-node))
         (current-line (lem:line-number-at-point point))
         (buffer (lem:current-buffer))
         (max-line (lem:buffer-nlines buffer)))
    (when current-node
      (multiple-value-bind (found-node parent depth)
          (find-node-in-tree current-node (get-outline-forest buffer))
        (if parent
            ;; we have a parent, look for true siblings (children of same parent)
            (let ((children (cltpt/tree:tree-children parent)))
              (when children
                (let ((current-index (position current-node children :test #'eql)))
                  (when current-index
                    (ecase direction
                      (:next
                       (let ((next-index (1+ current-index)))
                         (if (< next-index (length children))
                             ;; found next sibling, find its line number
                             (let ((next-sibling (elt children next-index)))
                               (loop for line-num from (1+ current-line) to max-line
                                     for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
                                     do (when (and (lem:move-to-line temp-point line-num)
                                                   (lem:move-to-column temp-point 1))
                                          (let ((node (lem:text-property-at temp-point :outline-node)))
                                            (when (eql node next-sibling)
                                              (return line-num))))))
                             ;; if last child, go to next parent
                             (let ((parent-depth (compute-node-depth parent)))
                               (loop for line-num from (1+ current-line) to max-line
                                     for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
                                     do (when (and (lem:move-to-line temp-point line-num)
                                                   (lem:move-to-column temp-point 1))
                                          (let ((node (lem:text-property-at temp-point :outline-node)))
                                            (when (and node
                                                       (= (compute-node-depth node) parent-depth)
                                                       (not (eql node parent)))
                                              (return-from find-true-sibling-node line-num)))))))))
                      (:previous
                       (let ((prev-index (1- current-index)))
                         (if (>= prev-index 0)
                             ;; found previous sibling, find its line number
                             (let ((prev-sibling (elt children prev-index)))
                               (loop for line-num from (1- current-line) downto 1
                                     for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
                                     do (when (and (lem:move-to-line temp-point line-num)
                                                   (lem:move-to-column temp-point 1))
                                          (let ((node (lem:text-property-at temp-point :outline-node)))
                                            (when (eql node prev-sibling)
                                              (return line-num))))))
                             ;; if first child, go to parent
                             (loop for line-num from 1 to (1- current-line)
                                   for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
                                   do (when (and (lem:move-to-line temp-point line-num)
                                                 (lem:move-to-column temp-point 1))
                                        (let ((node (lem:text-property-at temp-point :outline-node)))
                                          (when (eql node parent)
                                            (return-from find-true-sibling-node line-num)))))))))))))
            ;; no parent (root level), look for next/previous root node
            (let ((forest (get-outline-forest buffer)))
              (when forest
                (let ((current-index (position current-node forest :test #'eql)))
                  (when current-index
                    (ecase direction
                      (:next
                       (let ((next-index (1+ current-index)))
                         (when (< next-index (length forest))
                           ;; found next root, find its line number
                           (let ((next-root (elt forest next-index)))
                             (loop for line-num from (1+ current-line) to max-line
                                   for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
                                   do (when (and (lem:move-to-line temp-point line-num)
                                                 (lem:move-to-column temp-point 1))
                                        (let ((node (lem:text-property-at temp-point :outline-node)))
                                          (when (eql node next-root)
                                            (return line-num)))))))))
                      (:previous
                       (let ((prev-index (1- current-index)))
                         (when (>= prev-index 0)
                           ;; found previous root, find its line number
                           (let ((prev-root (elt forest prev-index)))
                             (loop for line-num from (1- current-line) downto 1
                                   for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
                                   do (when (and (lem:move-to-line temp-point line-num)
                                                 (lem:move-to-column temp-point 1))
                                        (let ((node (lem:text-property-at temp-point :outline-node)))
                                          (when (eql node prev-root)
                                            (return line-num)))))))))))))))))))

(lem:define-command outline-next-true-sibling () ()
  "move to next true sibling (same parent)."
  (let* ((point (lem:current-point))
         (next-line (find-true-sibling-node point :next)))
    (when next-line
      (lem:move-to-line point next-line)
      (lem:move-to-column point 1))))

(lem:define-command outline-previous-true-sibling () ()
  "move to previous true sibling (same parent)."
  (let* ((point (lem:current-point))
         (prev-line (find-true-sibling-node point :previous)))
    (when prev-line
      (lem:move-to-line point prev-line)
      (lem:move-to-column point 1))))

(lem:define-command outline-go-to-parent () ()
  "go to parent of current node."
  (let* ((point (lem:current-point))
         (current-node (lem:text-property-at point :outline-node))
         (buffer (lem:current-buffer)))
    (when current-node
      (multiple-value-bind (found-node parent depth)
          (find-node-in-tree current-node (get-outline-forest buffer))
        (declare (ignore found-node depth))
        (when parent
          ;; Find parent's line number by searching from the beginning
          (loop for line-num from 1 to (lem:line-number-at-point point)
                for temp-point = (lem:copy-point point :temporary)
                do (when (and (lem:move-to-line temp-point line-num)
                              (lem:move-to-column temp-point 1))
                     (let ((node (lem:text-property-at temp-point :outline-node)))
                       (when (eql node parent)
                         (lem:move-to-line point line-num)
                         (lem:move-to-column point 1)
                         (return))))))))))

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

    (open-outline (list node1 node2 node3)
                  :action-function
                  (lambda (node)
                    (lem:message "Custom action on: ~A"
                                 (cltpt/tree/outline:outline-text node))))))

;; make it work in vim-mode normal state
(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode outline-mode))
  (list *outline-mode-keymap*))