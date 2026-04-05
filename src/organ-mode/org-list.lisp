(in-package :organ/organ-mode)

(defun find-path-in-list-match (list-match pos)
  "find the path of list-item indices from LIST-MATCH (an org-list match) to the innermost
list-item containing POS. returns a list of indices, e.g. (0 1) for org-list[0][1]."
  (loop for item in (cltpt/combinator:match-children list-match)
        for i from 0
        when (and (>= (1+ pos) (cltpt/combinator:match-begin-absolute item))
                  (<= pos (cltpt/combinator:match-end-absolute item)))
          return (let* ((content (cltpt/combinator/match:find-direct-match-child-by-id
                                  item
                                  'cltpt/org-mode::list-item-content))
                        (sub-list (when content
                                    (cltpt/combinator/match:find-direct-match-child-by-id
                                     content
                                     'cltpt/org-mode:org-list)))
                        (sub-path (when sub-list
                                    (find-path-in-list-match sub-list pos))))
                   (if sub-path
                       (cons i sub-path)
                       (list i)))))

(defun current-item-data (list-obj)
  "find the current list item's position in the nested data structure.
returns (values path data) where path is a list of indices from root to the item,
e.g. (2) for top-level item at index 2, (0 1) for the second child of the first item."
  (let* ((match (cltpt/base:text-object-match list-obj))
         (buf-text (lem:buffer-text (lem:current-buffer)))
         (pos (organ/utils:current-pos-no-newline))
         (path (find-path-in-list-match match pos)))
    (when path
      (values path
              (cltpt/org-mode:list-match-to-list buf-text match)))))

(defun containing-list (data path)
  "return the list containing the item at PATH in the nested list DATA."
  (if (= (length path) 1)
      data
      (let ((item (nth (first path) (getf data :children))))
        (containing-list (getf item :children) (rest path)))))

(defun item-index-in-list (path)
  "return the index of the item within its containing list."
  (car (last path)))

(defun aggregate-child-checkbox-state (children-lst)
  "compute the aggregate checkbox state for CHILDREN-LST.
returns `nil' when none of the children have checkboxes."
  (let ((states (remove nil
                        (mapcar (lambda (item) (getf item :checkbox))
                                (getf children-lst :children)))))
    (when states
      (if (every (lambda (state) (eq state :checked)) states)
          :checked
          (if (some (lambda (state)
                      (or (eq state :checked) (eq state :partial)))
                    states)
              :partial
              :unchecked)))))

(defun update-checkboxes-upward (data path)
  "recompute checkbox states for ancestors in PATH that have checkboxes."
  (loop for current-path = (butlast path) then (butlast current-path)
        while current-path
        do (let* ((current-lst (containing-list data current-path))
                  (current-idx (item-index-in-list current-path))
                  (current-item (nth current-idx (getf current-lst :children)))
                  (children-lst (getf current-item :children))
                  (aggregate-state (and children-lst
                                        (aggregate-child-checkbox-state children-lst))))
             (when (and aggregate-state (getf current-item :checkbox))
               (setf (getf current-item :checkbox) aggregate-state)))))

(defun indent-item-in-list (lst item-idx &key with-children)
  "indent the item at ITEM-IDX in list LST, making it a child of its previous sibling.
when WITH-CHILDREN is nil, the item's children are detached and placed as siblings of the item
at the new level. when WITH-CHILDREN is t, children move along.
modifies LST destructively. returns (values indent-change old-bullet new-bullet)
where indent-change is the content-offset at the new child level, or nil on failure."
  (let ((items (getf lst :children)))
    (if (= item-idx 0)
        (lem:editor-error "cannot indent first item of a list")
        (let* ((item (nth item-idx items))
               (old-bullet (getf item :bullet))
               (prev-item (nth (1- item-idx) items))
               (prev-children-lst (getf prev-item :children))
               (existing-items (when prev-children-lst (getf prev-children-lst :children)))
               (orphaned-lst (unless with-children (getf item :children)))
               (orphaned-items (when orphaned-lst (getf orphaned-lst :children))))
          (unless with-children
            (setf (getf item :children) nil))
          ;; set bullet: continue existing sub-list sequence, or start new sub-list
          (setf (getf item :bullet)
                (if prev-children-lst
                    (cltpt/org-mode:bullet-at-index prev-children-lst
                                                     (length existing-items))
                    (let ((bt (getf lst :type)))
                      (if (and bt (getf bt :first-bullet))
                          (getf bt :first-bullet)
                          (getf prev-item :bullet)))))
          ;; add as last child of previous item
          (if prev-children-lst
              (setf (getf prev-children-lst :children)
                    (append existing-items (list item)))
              (setf (getf prev-item :children)
                    (list :type (getf lst :type) :children (list item))))
          ;; place orphaned children as siblings after the item in the new parent
          (when orphaned-items
            (let ((plst (getf prev-item :children)))
              (setf (getf plst :children)
                    (append (getf plst :children) orphaned-items))
              (cltpt/org-mode:renumber-list-items plst (1+ (length existing-items)))))
          ;; remove from list
          (setf (cdr (nthcdr (1- item-idx) items)) (nthcdr (1+ item-idx) items))
          ;; renumber remaining items after the removed one
          (cltpt/org-mode:renumber-list-items lst item-idx)
          (values (1+ (length (getf prev-item :bullet))) old-bullet (getf item :bullet))))))

(defun org-list-indent (list-obj with-children)
  (let ((col (lem:point-column (lem:current-point)))
        (line (lem:line-number-at-point (lem:current-point))))
    (multiple-value-bind (path data) (current-item-data list-obj)
      (when path
        (let* ((match (cltpt/base:text-object-match list-obj))
               (indent (or (getf (cltpt/combinator:match-props match) :indent) 0))
               (lst (containing-list data path))
               (idx (item-index-in-list path)))
          (multiple-value-bind (indent-change old-bullet new-bullet)
              (indent-item-in-list lst idx :with-children with-children)
            (when indent-change
              (let ((bullet-delta (- (length new-bullet) (length old-bullet)))
                    (new-str (cltpt/org-mode:list-to-list-string data indent)))
                (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
                (lem:move-to-line (lem:current-point) line)
                (lem:move-to-column (lem:current-point)
                                    (+ col indent-change bullet-delta))))))))))

(defun org-list-indent-item (list-obj)
  "indent the list item at point, making it a child of the previous sibling.
children are detached and become siblings at the new level."
  (org-list-indent list-obj nil))

(defun org-list-indent-tree (list-obj)
  "indent the list item at point along with its children, making the whole subtree a child
of the previous sibling."
  (org-list-indent list-obj t))

(defun dedent-item-in-list (children-lst sub-idx parent-item parent-lst parent-idx
                            &key error-on-children)
  "dedent the item at SUB-IDX in CHILDREN-LST out to PARENT-LST after PARENT-IDX.
subsequent siblings always become children of the dedented item.
when ERROR-ON-CHILDREN is true and the item already has children, signals an error.
returns (values indent-change old-bullet new-bullet) where indent-change is the
content-offset at the old child level, or nil on failure."
  (let* ((children (getf children-lst :children))
         (parent-items (getf parent-lst :children))
         (item (nth sub-idx children)))
    (when item
      (when (and error-on-children (getf item :children))
        (lem:editor-error "cannot dedent an item without its children"))
      (let ((old-bullet (getf item :bullet))
            (indent-change (- (getf (first children) :indent) (getf parent-item :indent))))
        ;; adopt subsequent siblings as children
        (let ((subsequent (subseq children (1+ sub-idx))))
          (when subsequent
            (let* ((item-ch (getf item :children))
                   (existing-count (if item-ch (length (getf item-ch :children)) 0)))
              (if item-ch
                  (setf (getf item-ch :children)
                        (append (getf item-ch :children) subsequent))
                  (setf (getf item :children)
                        (list :type (getf children-lst :type) :children subsequent)))
              (cltpt/org-mode:renumber-list-items (getf item :children) existing-count))))
        ;; set bullet to continue the parent level's sequence
        (setf (getf item :bullet)
              (cltpt/org-mode:bullet-at-index parent-lst (1+ parent-idx)))
        ;; remove item and all adopted siblings from parent's children
        (setf (getf parent-item :children)
              (if (> sub-idx 0)
                  (list :type (getf children-lst :type)
                        :children (subseq children 0 sub-idx))
                  nil))
        ;; insert after parent in its containing list
        (let ((cell (nthcdr parent-idx parent-items)))
          (setf (cdr cell) (cons item (cdr cell))))
        ;; renumber subsequent siblings in the parent list
        (cltpt/org-mode:renumber-list-items parent-lst (+ parent-idx 2))
        (values indent-change old-bullet (getf item :bullet))))))

(defun org-list-dedent (list-obj error-on-children)
  "dedent an item or a tree in a list.
always adopts subsequent siblings. when ERROR-ON-CHILDREN, errors if item has children."
  (let ((col (lem:point-column (lem:current-point)))
        (line (lem:line-number-at-point (lem:current-point))))
    (multiple-value-bind (path data) (current-item-data list-obj)
      (when (and path (> (length path) 1))
        (let* ((match (cltpt/base:text-object-match list-obj))
               (indent (or (getf (cltpt/combinator:match-props match) :indent) 0))
               (sub-idx (item-index-in-list path))
               (parent-path (butlast path))
               (parent-lst (containing-list data parent-path))
               (parent-idx (item-index-in-list parent-path))
               (parent-item (nth parent-idx (getf parent-lst :children)))
               (children-lst (getf parent-item :children)))
          (multiple-value-bind (indent-change old-bullet new-bullet)
              (dedent-item-in-list children-lst
                                   sub-idx
                                   parent-item
                                   parent-lst
                                   parent-idx
                                   :error-on-children error-on-children)
            (when indent-change
              (let* ((bullet-delta (- (length new-bullet) (length old-bullet)))
                     (new-str (cltpt/org-mode:list-to-list-string data indent)))
                (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
                (lem:move-to-line (lem:current-point) line)
                (lem:move-to-column (lem:current-point)
                                    (max 0 (+ col (- bullet-delta indent-change))))))))))))

(defun org-list-dedent-item (list-obj)
  "dedent a nested list item, moving it out to the parent level after its parent item.
subsequent siblings become children of the dedented item.
errors if the item already has children (use dedent-tree instead)."
  (org-list-dedent list-obj t))

(defun org-list-dedent-tree (list-obj)
  "dedent a nested list item along with its existing children, moving the whole subtree
to the parent level after the parent item. subsequent siblings also become children."
  (org-list-dedent list-obj nil))

(defun org-list-move-item (list-obj direction)
  "move the list item at point up or down within LIST-OBJ.

DIRECTION is -1 (up) or +1 (down).
swaps only the content portion, keeping bullets and indentation in place."
  (let* ((match (cltpt/base:text-object-match list-obj))
         (buf-text (lem:buffer-text (lem:current-buffer)))
         (data (cltpt/org-mode:list-match-to-list buf-text match))
         (indent (or (getf (cltpt/combinator:match-props match) :indent) 0))
         (items (cltpt/combinator:match-children match))
         (pos (organ/utils:current-pos))
         (idx (loop for item in items
                    for i from 0
                    when (and (<= (cltpt/combinator:match-begin-absolute item) pos)
                              (<= pos (cltpt/combinator:match-end-absolute item)))
                      return i))
         (target (when idx (+ idx direction))))
    (let ((data-items (when data (getf data :children))))
     (when (and data-items idx target (>= target 0) (< target (length data-items)))
      ;; swap content and children, keeping bullets in place
      (let ((item-a (nth idx data-items))
            (item-b (nth target data-items)))
        (rotatef (getf item-a :content) (getf item-b :content))
        (rotatef (getf item-a :children) (getf item-b :children)))
      ;; convert back to string and replace buffer text
      (let ((new-str (cltpt/org-mode:list-to-list-string data indent)))
        (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
        ;; position cursor on the target item
        (let* ((new-match (cltpt/org-mode:org-list-matcher
                           nil
                           (cltpt/reader:reader-from-string new-str)
                           0))
               (new-items (cltpt/combinator:match-children new-match))
               (moved-item (nth target new-items))
               (list-start (1+ (cltpt/combinator:match-begin-absolute match)))
               (cursor-pos (when moved-item
                             (+ list-start
                                (cltpt/combinator:match-begin-absolute moved-item)))))
          (when cursor-pos
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point (lem:current-buffer)
                                                              cursor-pos)))))))))

(defun list-match-item-info (list-match buf-text pos)
  "extract bullet info from LIST-MATCH for the item at POS.
recurses into nested sub-lists when the cursor falls within one.
POS should already be adjusted for end-of-line boundaries (see `current-pos-no-newline').
returns (values indent bullets) where bullets is all bullet strings up to and including the current item, or nil."
  (let ((items (cltpt/combinator:match-children list-match))
        (bullets))
    (loop for item in items
          for bn = (cltpt/combinator/match:find-direct-match-child-by-id
                    item
                    'cltpt/org-mode::list-item-bullet)
          when bn do (push (cltpt/combinator:match-text bn buf-text) bullets)
            when (and (<= (cltpt/combinator:match-begin-absolute item) pos)
                      (<= pos (cltpt/combinator:match-end-absolute item)))
              return (let* ((content-node
                              (cltpt/combinator/match:find-direct-match-child-by-id
                               item
                               'cltpt/org-mode::list-item-content))
                            (sub-list
                              (when content-node
                                (cltpt/combinator/match:find-direct-match-child-by-id
                                 content-node
                                 'cltpt/org-mode:org-list))))
                       (if (and sub-list
                                (<= (cltpt/combinator:match-begin-absolute sub-list) pos)
                                (<= pos (cltpt/combinator:match-end-absolute sub-list)))
                           (list-match-item-info sub-list buf-text pos)
                           (values (or (getf (cltpt/combinator:match-props item) :indent) 0)
                                   (nreverse bullets)))))))

(defun list-item-info (list-obj)
  "extract bullet info from the parsed tree for the list-item at point.
returns (values indent bullets) where bullets is all bullet strings up to and including the current item."
  (list-match-item-info
   (cltpt/base:text-object-match list-obj)
   (lem:buffer-text (lem:current-buffer))
   (organ/utils:current-pos-no-newline)))

(defun org-list-cycle-bullet (list-obj)
  "cycle the bullet type of items at the current level."
  (let ((col (lem:point-column (lem:current-point)))
        (line (lem:line-number-at-point (lem:current-point))))
    (multiple-value-bind (path data) (current-item-data list-obj)
      (when path
        (let* ((match (cltpt/base:text-object-match list-obj))
               (indent (or (getf (cltpt/combinator:match-props match) :indent) 0))
               (lst (containing-list data path))
               (items (getf lst :children))
               (idx (item-index-in-list path))
               (current-bullet (getf (nth idx items) :bullet))
               (next-bt (cltpt/org-mode:cycle-next-bullet (getf lst :type)))
               (next-bullet-str (getf next-bt :first-bullet)))
          (multiple-value-bind (marker suffix) (cltpt/org-mode:bullet-split next-bullet-str)
            ;; update the list type and first item's marker
            (setf (getf lst :type) next-bt)
            (setf (getf (first items) :marker) marker)
            ;; regenerate all bullets
            (loop for item in items
                  for i from 0
                  do (setf (getf item :bullet) (cltpt/org-mode:bullet-at-index lst i)))
            (let* ((new-str (cltpt/org-mode:list-to-list-string data indent))
                   (new-bullet (getf (nth idx items) :bullet))
                   (bullet-delta (- (length new-bullet) (length current-bullet))))
              (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
              (lem:move-to-line (lem:current-point) line)
              (lem:move-to-column (lem:current-point)
                                  (max 0 (+ col bullet-delta))))))))))

(defun org-list-toggle-checkbox (list-obj)
  "toggle the checkbox on the current list item.
leaf items toggle between unchecked and checked.
items with children that have checkboxes recompute from their subtree.
only modifies items that already have a checkbox."
  (let ((col (lem:point-column (lem:current-point)))
        (line (lem:line-number-at-point (lem:current-point))))
    (multiple-value-bind (path data) (current-item-data list-obj)
      (when path
        (let* ((match (cltpt/base:text-object-match list-obj))
               (indent (getf (cltpt/combinator:match-props match) :indent))
               (lst (containing-list data path))
               (idx (item-index-in-list path))
               (item (nth idx (getf lst :children)))
               (current (getf item :checkbox))
               (children-lst (getf item :children))
               (aggregate-state (aggregate-child-checkbox-state children-lst)))
          (when current
            (if aggregate-state
                (setf (getf item :checkbox) aggregate-state)
                (setf (getf item :checkbox)
                      (ecase current
                        (:unchecked :checked)
                        (:checked :unchecked)
                        (:partial :unchecked)))))
          (update-checkboxes-upward data path)
          (cltpt/org-mode:renumber-list-items lst)
          (let ((new-str (cltpt/org-mode:list-to-list-string data indent)))
            (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
            (lem:move-to-line (lem:current-point) line)
            (lem:move-to-column (lem:current-point) col)))))))

(defun find-item-match-at-path (list-match path)
  "navigate LIST-MATCH to return the list-item match at PATH (list of indices)."
  (let ((item (nth (first path) (cltpt/combinator:match-children list-match))))
    (if (or (null item) (= (length path) 1))
        item
        (let* ((content (cltpt/combinator/match:find-direct-match-child-by-id
                         item 'cltpt/org-mode::list-item-content))
               (sub-list (when content
                           (cltpt/combinator/match:find-direct-match-child-by-id
                            content 'cltpt/org-mode:org-list))))
          (when sub-list
            (find-item-match-at-path sub-list (rest path)))))))

(defun org-list-newline ()
  "insert a new list entry on the next line with the correct bullet and indentation."
  (let ((list-obj (current-text-obj-ignore-newline 'cltpt/org-mode:org-list)))
    (multiple-value-bind (path data) (current-item-data list-obj)
      (when path
        (let* ((match (cltpt/base:text-object-match list-obj))
               (root-indent (or (getf (cltpt/combinator:match-props match) :indent) 0))
               (lst (containing-list data path))
               (items (getf lst :children))
               (idx (item-index-in-list path))
               (current-item (nth idx items))
               (new-bullet (cltpt/org-mode:bullet-at-index lst (1+ idx)))
               (new-item (list :bullet new-bullet
                               :content ""
                               :children nil
                               :marker (getf current-item :marker)
                               :suffix (getf current-item :suffix))))
          ;; insert new item after current item in the list
          (let ((cell (nthcdr idx items)))
            (setf (cdr cell) (cons new-item (cdr cell))))
          ;; renumber all siblings after the new item
          (cltpt/org-mode:renumber-list-items lst (+ idx 2))
          ;; serialize, replace buffer, position cursor at new item's content start
          (let* ((new-str (cltpt/org-mode:list-to-list-string data root-indent))
                 (new-path (append (butlast path) (list (1+ idx))))
                 (new-list-match (cltpt/org-mode:org-list-matcher
                                  nil
                                  (cltpt/reader:reader-from-string new-str)
                                  0))
                 (new-item-match (when new-list-match
                                   (find-item-match-at-path new-list-match new-path)))
                 (list-start (cltpt/combinator:match-begin-absolute match))
                 (item-indent (when new-item-match
                                (or (getf (cltpt/combinator:match-props new-item-match) :indent) 0)))
                 (cursor-pos (when new-item-match
                               (+ list-start
                                  (cltpt/combinator:match-begin-absolute new-item-match)
                                  item-indent
                                  (length new-bullet)
                                  1))))
            (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
            (when cursor-pos
              (lem:move-point (lem:current-point)
                              (organ/utils:char-offset-to-point
                               (lem:current-buffer)
                               cursor-pos)))))))))