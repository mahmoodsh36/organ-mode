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
(defgeneric interactive-render-node (node buffer point depth)
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

(defmethod interactive-render-node ((node t) buffer point depth)
  "render a node interactively with clickable regions."
  (let* ((indent (make-string (* depth 2) :initial-element #\space)))
    (lem:insert-string point indent)
    (if (cltpt/tree/outline:could-expand node)
        (lem:insert-string point
                           (if (cltpt/tree/outline:should-expand node)
                               ;; "▼ "  ;; down arrow for expanded
                               ;; "▶ "  ;; right arrow for collapsed
                               "- "
                               "+ "))
        (lem:insert-string point "  "))
    (let ((content-start-pos (lem:copy-point point :right-inserting)))
      ;; insert the node text
      (lem:insert-string point
                         (format nil "~A" (cltpt/tree/outline:outline-text node)))
      (let ((node-end-pos (lem:copy-point point :right-inserting)))
        (lem:insert-character point #\newline)
        ;; set properties for the content area (excluding indentation)
        (lem-core::set-clickable
         content-start-pos
         node-end-pos
         (lambda (window clicked-point)
           (declare (ignore window))
           (outline-expand-collapse-at-point clicked-point)))
        (lem:put-text-property content-start-pos node-end-pos :outline-node node)))))

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

(defun get-outline-forest (buffer)
  "get the outline forest associated with a buffer."
  (lem:buffer-value buffer +outline-forest+))

(defun set-outline-forest (buffer forest)
  "set the outline forest for a buffer."
  (setf (lem:buffer-value buffer +outline-forest+) forest))

(defun set-outline-action-function (buffer action-function)
  "set the outline action function for a buffer."
  (setf (lem:buffer-value buffer +outline-action-function+) action-function))

(defun get-outline-action-function (buffer)
  "get the outline action function for a buffer."
  (lem:buffer-value buffer +outline-action-function+))

(defun create-custom-outline-mode (mode-name key-bindings &optional mode-setup)
  "create a custom outline mode with inherited keymap and custom key bindings.

KEY-BINDINGS is a list of (key-string command-symbol) pairs.
MODE-SETUP is an optional function called when the mode is activated."
  (let ((keymap-name (intern (format nil "*~A-KEYMAP*" (string mode-name))))
        (mode-hook-name (intern (format nil "~A-HOOK" (string mode-name)))))
    `(progn
       (defvar ,keymap-name (lem:make-keymap :description ',keymap-name
                                             :base *outline-mode-keymap*))
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

(defun node-at-point (point)
  "find the outline node at the given point in the buffer."
  (lem:text-property-at point :outline-node))

(defun render-outline (buffer forest)
  "render the outline forest to the buffer."
  (lem:with-buffer-read-only buffer nil
    (let ((lem:*inhibit-read-only* t))
      (lem:erase-buffer buffer)
      (let ((point (lem:buffer-point buffer)))
        (dolist (node forest)
          (render-node point node 0))))))

(defun render-node (point node depth)
  "render a single node and its children recursively."
  (interactive-render-node node (lem:current-buffer) point depth)
  ;; if the node should display children, render its children
  (when (cltpt/tree/outline:should-expand node)
    (dolist (child (cltpt/tree:tree-children node))
      (render-node point child (1+ depth)))))

(lem:define-command outline-expand-collapse () ()
  "expand or collapse the node at the current point."
  (let ((point (lem:current-point)))
    (finish-output)
    (outline-expand-collapse-at-point point)
    (finish-output)))

(lem:define-command outline-action-at-point () ()
  "perform an action on the node at the current point."
  (let ((point (lem:current-point)))
    (let ((node (lem:text-property-at point :outline-node)))
      (when node
        (let ((action-fn (lem:buffer-value (lem:current-buffer) +outline-action-function+)))
          (when action-fn
            (funcall action-fn node)))))))

(lem:define-command outline-quit () ()
  "kill the outline buffer."
  (lem:kill-buffer (lem:current-buffer)))

(defun outline-expand-collapse-at-point (point)
  "expand or collapse the node at the given point."
  (let* ((buffer (lem:current-buffer))
         (line-num (lem:line-number-at-point point)))
    (let ((node (lem:text-property-at point :outline-node)))
      (when node
        (outline-mode-toggle node)
        ;; re-render the outline after expanding/collapsing
        (render-outline buffer (get-outline-forest buffer))
        ;; restore cursor position based on the line number
        (lem:move-to-line (lem:buffer-point buffer) line-num)))))

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

(defun scan-lines (buffer start-line end-line step predicate)
  "scan lines in BUFFER from START-LINE toward END-LINE with STEP increment.
STEP can be positive (forward) or negative (backward).

for each line, call PREDICATE with the node at that line (or NIL).
when PREDICATE returns non-NIL, return that line number."
  (let ((test (if (plusp step)
                  #'<=
                  #'>=)))
    (loop for line-num = start-line then (+ line-num step)
          while (funcall test line-num end-line)
          for temp-point = (lem:copy-point (lem:buffer-point buffer) :temporary)
          when (and (lem:move-to-line temp-point line-num)
                    (lem:move-to-column temp-point 1))
            do (let ((node (lem:text-property-at temp-point :outline-node)))
                 (when (funcall predicate node)
                   (return line-num))))))

(defun scan-lines-forward (buffer start-line predicate)
  "scan lines forward from START-LINE."
  (scan-lines buffer start-line (lem:buffer-nlines buffer) 1 predicate))

(defun scan-lines-backward (buffer start-line predicate)
  "scan lines backward from START-LINE."
  (scan-lines buffer start-line 1 -1 predicate))

(defun move-point-to-line (point line-num)
  "move POINT to LINE-NUM column 1."
  (when line-num
    (lem:move-to-line point line-num)
    (lem:move-to-column point 1)
    line-num))

(defun find-sibling-node (point direction)
  "find the next or previous sibling node (same depth level)."
  (let* ((current-depth (get-node-depth point))
         (current-line (lem:line-number-at-point point))
         (buffer (lem:point-buffer point)))
    (when current-depth
      (let ((predicate (lambda (node)
                         (and node (= (compute-node-depth node) current-depth)))))
        (ecase direction
          (:next (scan-lines-forward buffer (1+ current-line) predicate))
          (:previous (scan-lines-backward buffer (1- current-line) predicate)))))))

(defun find-node-line (buffer target-node start-line direction)
  "find the line number of TARGET-NODE starting from START-LINE in DIRECTION."
  (let ((predicate (lambda (node) (eql node target-node))))
    (ecase direction
      (:next (scan-lines-forward buffer start-line predicate))
      (:previous (scan-lines-backward buffer start-line predicate)))))

(defun find-true-sibling-node (point direction)
  "find next or previous true sibling node (same parent)."
  (let* ((current-node (lem:text-property-at point :outline-node))
         (current-line (lem:line-number-at-point point))
         (buffer (lem:current-buffer)))
    (when current-node
      (multiple-value-bind (found-node parent depth)
          (find-node-in-tree current-node (get-outline-forest buffer))
        (declare (ignore found-node depth))
        (let ((siblings (if parent
                            (cltpt/tree:tree-children parent)
                            (get-outline-forest buffer))))
          (when siblings
            (let ((current-index (position current-node siblings :test #'eql)))
              (when current-index
                (ecase direction
                  (:next (find-next-true-sibling
                          buffer current-line siblings current-index parent))
                  (:previous (find-previous-true-sibling
                              buffer current-line siblings current-index parent)))))))))))

(defun find-next-true-sibling (buffer current-line siblings current-index parent)
  "find the next true sibling's line number."
  (let ((next-index (1+ current-index)))
    (if (< next-index (length siblings))
        (find-node-line buffer (elt siblings next-index) (1+ current-line) :next)
        ;; last child. if we have a parent, go to next node at parent's depth
        (when parent
          (let ((parent-depth (compute-node-depth parent)))
            (scan-lines-forward
             buffer
             (1+ current-line)
             (lambda (node)
               (and node
                    (= (compute-node-depth node) parent-depth)
                    (not (eql node parent))))))))))

(defun find-previous-true-sibling (buffer current-line siblings current-index parent)
  "find the previous true sibling's line number."
  (let ((prev-index (1- current-index)))
    (if (>= prev-index 0)
        (find-node-line buffer (elt siblings prev-index) (1- current-line) :previous)
        ;; first child. go to parent
        (when parent
          (find-node-line buffer parent (1- current-line) :previous)))))

;; this is just to DRY things
(defmacro define-sibling-command (name docstring find-fn direction)
  "define an outline sibling navigation command."
  `(lem:define-command ,name () ()
     ,docstring
     (let* ((point (lem:current-point))
            (target-line (,find-fn point ,direction)))
       (move-point-to-line point target-line))))

(define-sibling-command outline-next-sibling
    "move to next node at same depth."
  find-sibling-node :next)

(define-sibling-command outline-previous-sibling
    "move to previous node at same depth."
  find-sibling-node :previous)

(define-sibling-command outline-next-true-sibling
    "move to next true sibling (same parent)."
  find-true-sibling-node :next)

(define-sibling-command outline-previous-true-sibling
    "move to previous true sibling (same parent)."
  find-true-sibling-node :previous)

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
          (move-point-to-line
           point
           (find-node-line buffer parent (lem:line-number-at-point point) :previous)))))))

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
                    (lem:message "at node: ~A"
                                 (cltpt/tree/outline:outline-text node))))))

;; make it work in vim-mode normal state
(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode outline-mode))
  (list *outline-mode-keymap*))