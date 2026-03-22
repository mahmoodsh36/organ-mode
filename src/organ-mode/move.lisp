(in-package :organ/organ-mode)

;; NOTE: most of the functions only work for direction=+1/-1

(defun swap-text-objects (obj-a obj-b)
  "swap the buffer text of OBJ-A and OBJ-B."
  (let* ((buf     (lem:current-buffer))
         (begin-a (1+ (cltpt/base:text-object-begin-in-root obj-a)))
         (end-a   (1+ (cltpt/base:text-object-end-in-root obj-a)))
         (begin-b (1+ (cltpt/base:text-object-begin-in-root obj-b)))
         (end-b   (1+ (cltpt/base:text-object-end-in-root obj-b)))
         (text-a  (cltpt/base:text-object-text obj-a))
         (text-b  (cltpt/base:text-object-text obj-b)))
    ;; replace the later region first to keep offsets valid.
    (if (< begin-a begin-b)
        (progn
          (organ/utils:replace-text-between-positions buf begin-b end-b text-a)
          (organ/utils:replace-text-between-positions buf begin-a end-a text-b))
        (progn
          (organ/utils:replace-text-between-positions buf begin-a end-a text-b)
          (organ/utils:replace-text-between-positions buf begin-b end-b text-a)))))

(defun org-header-move (header direction)
  "move HEADER up (DIRECTION=-1) or down (+1) past the adjacent same-level sibling.
swaps full subtrees (including body text and sub-headers)."
  (let* ((parent   (cltpt/base:text-object-parent header))
         (level    (cltpt/base:text-object-property header :level))
         (siblings (when parent
                     (sort (remove-if-not
                            (lambda (c)
                              (and (typep c 'cltpt/org-mode:org-header)
                                   (= (cltpt/base:text-object-property c :level) level)))
                            (cltpt/base:text-object-children parent))
                           #'<
                           :key #'cltpt/base:text-object-begin-in-root)))
         (idx        (position header siblings))
         (target-idx (when idx
                       (+ idx direction)))
         (target     (when (and target-idx
                                (>= target-idx 0)
                                (< target-idx (length siblings)))
                       (nth target-idx siblings))))
    (when target
      (let* ((earlier      (if (< idx target-idx) header target))
             (later        (if (< idx target-idx) target header))
             (region-start (cltpt/base:text-object-begin-in-root earlier))
             (split        (cltpt/base:text-object-begin-in-root later))
             (buf          (lem:current-buffer))
             (buf-text     (lem:buffer-text buf))
             (region-end   (cltpt/base:text-object-end-in-root later))
             ;; TODO: its not a good idea to run subseq all the time
             (text-earlier (subseq buf-text region-start split))
             (text-later-raw (subseq buf-text split region-end))
             (at-buffer-end (= region-end (length buf-text)))
             ;; if the last header had no trailing newline, add one
             (text-later (if (and at-buffer-end
                                  (> (length text-later-raw) 0)
                                  (not (char= (char text-later-raw
                                                    (1- (length text-later-raw)))
                                              #\newline)))
                             (concatenate 'string text-later-raw (string #\newline))
                             text-later-raw))
             (replacement (concatenate 'string text-later text-earlier))
             (new-pos (if (= direction 1)
                          (+ region-start (length text-later))
                          region-start))
             (start-point  (organ/utils:char-offset-to-point buf region-start))
             (end-point    (if at-buffer-end
                               (lem:copy-point (lem:buffer-end-point buf) :temporary)
                               (organ/utils:char-offset-to-point buf region-end))))
        (lem:delete-between-points start-point end-point)
        (lem:insert-string start-point replacement)
        (lem:move-point (lem:current-point)
                        (organ/utils:char-offset-to-point buf new-pos))))))

(defun org-block-move (blk direction)
  (let* ((parent   (cltpt/base:text-object-parent blk))
         (siblings (when parent
                     (sort (remove-if-not
                            (lambda (c)
                              (or (typep c 'cltpt/org-mode:org-block)
                                  (typep c 'cltpt/org-mode:org-src-block)))
                            (cltpt/base:text-object-children parent))
                           #'<
                           :key #'cltpt/base:text-object-begin-in-root)))
         (idx        (position blk siblings))
         (target-idx (when idx (+ idx direction)))
         (target     (when (and target-idx
                                (>= target-idx 0)
                                (< target-idx (length siblings)))
                       (nth target-idx siblings))))
    (when target
      (let* ((begin-target (cltpt/base:text-object-begin-in-root target))
             (len-blk (length (cltpt/base:text-object-text blk)))
             (len-target (length (cltpt/base:text-object-text target)))
             ;; when moving down the second (earlier) replacement shifts the block's
             ;; new position by (len-target - len-block).
             (new-pos (if (= direction 1)
                          (+ begin-target (- len-target len-blk))
                          begin-target)))
        (swap-text-objects blk target)
        (lem:move-point (lem:current-point)
                        (organ/utils:char-offset-to-point (lem:current-buffer) new-pos))))))

(defun organ-move-to-element (direction &optional (predicate (lambda (&rest args) t)))
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

(defun org-table-move-row (table-obj direction)
  "move the table row at point up or down within TABLE-OBJ.

DIRECTION is -1 (up) or +1 (down)."
  (let* ((match (cltpt/base:text-object-match table-obj))
         (buf-text (lem:buffer-text (lem:current-buffer)))
         (table-str (cltpt/combinator:match-text match buf-text))
         (table-start-pos (cltpt/combinator:match-begin-absolute match))
         (data (cltpt/org-mode:table-match-to-list table-str match))
         ;; find which data row the cursor is on
         (pos (organ/utils:current-pos))
         (row-nodes (loop for child in (cltpt/combinator:match-children match)
                          when (eq (cltpt/combinator:match-id child)
                                   'cltpt/org-mode::table-row)
                            collect child))
         (row-idx (loop for row in row-nodes
                        for i from 0
                        when (and (<= (cltpt/combinator:match-begin-absolute row) pos)
                                  (<= pos (cltpt/combinator:match-end-absolute row)))
                          return i))
         ;; map row-idx to data index (skipping :hrule entries)
         (data-indices (loop for item in data
                             for i from 0
                             when (listp item)
                               collect i))
         (data-idx (when row-idx
                     (nth row-idx data-indices)))
         (target-row-idx (when row-idx
                           (+ row-idx direction)))
         (target-data-idx (when (and target-row-idx
                                     (>= target-row-idx 0)
                                     (< target-row-idx (length data-indices)))
                            (nth target-row-idx data-indices))))
    (when (and data-idx
               target-data-idx
               (>= target-row-idx 0)
               (< target-row-idx (length data-indices)))
      (rotatef (nth data-idx data) (nth target-data-idx data))
      (let ((new-str (cltpt/org-mode:list-to-table-string data)))
        (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
        ;; position cursor on the target row
        (let* ((new-match (cltpt/org-mode:org-table-matcher
                           nil
                           (cltpt/reader:reader-from-string new-str)
                           0))
               (new-row-nodes (loop for child in (cltpt/combinator:match-children new-match)
                                    when (eq (cltpt/combinator:match-id child)
                                             'cltpt/org-mode::table-row)
                                      collect child))
               (moved-row (nth target-row-idx new-row-nodes))
               (cursor-pos (when moved-row
                             (+ (1+ table-start-pos)
                                (cltpt/combinator:match-begin-absolute moved-row)))))
          (when cursor-pos
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point (lem:current-buffer)
                                                              cursor-pos))))))))

(defun org-table-move-column (table-obj direction)
  "move the table column at point left or right within TABLE-OBJ.

DIRECTION is -1 (left) or +1 (right)."
  (let* ((match (cltpt/base:text-object-match table-obj))
         (buf-text (lem:buffer-text (lem:current-buffer)))
         (table-str (cltpt/combinator:match-text match buf-text))
         (table-start-pos (cltpt/combinator:match-begin-absolute match))
         (fresh-match match)
         (data (cltpt/org-mode:table-match-to-list table-str fresh-match))
         (width (cltpt/org-mode:get-table-width fresh-match))
         ;; find which column the cursor is in using cursor's column position
         (cursor-col (lem:point-column (lem:current-point)))
         (first-row (find-if
                     (lambda (n)
                       (eq (cltpt/combinator:match-id n) 'cltpt/org-mode::table-row))
                     (cltpt/combinator:match-children fresh-match)))
         (row-cells (when first-row
                      (remove-if-not
                       (lambda (n)
                         (eq (cltpt/combinator:match-id n) 'cltpt/org-mode::table-cell))
                       (cltpt/combinator:match-children first-row))))
         (row-begin (when first-row
                      (cltpt/combinator:match-begin first-row)))
         (col-idx (when row-cells
                    (or (loop for c in row-cells
                              for i from 0
                              when (and (>= cursor-col
                                            (+ row-begin (cltpt/combinator:match-begin c)))
                                        (< cursor-col
                                           (+ row-begin (cltpt/combinator:match-end c))))
                                return i)
                        ;; on delimiter: find nearest cell to the right
                        (loop for c in row-cells
                              for i from 0
                              when (>= (+ row-begin (cltpt/combinator:match-begin c))
                                       cursor-col)
                                return i)
                        ;; past last cell: use last column
                        (1- (length row-cells)))))
         (target-col (when col-idx
                       (+ col-idx direction))))
    (when (and col-idx target-col (>= target-col 0) (< target-col width))
      ;; swap columns in every data row
      (dolist (row data)
        (when (listp row)
          (rotatef (nth col-idx row) (nth target-col row))))
      (let ((new-str (cltpt/org-mode:list-to-table-string data)))
        (organ/utils:replace-submatch-text* (lem:current-buffer) match new-str)
        ;; position cursor on the target column, same row
        (let* ((new-match (cltpt/org-mode:org-table-matcher
                           nil
                           (cltpt/reader:reader-from-string new-str)
                           0))
               ;; find current row index from cursor line within the table
               (cur-line (lem:line-number-at-point (lem:current-point)))
               (table-line (lem:line-number-at-point
                            (organ/utils:char-offset-to-point
                             (lem:current-buffer)
                             (1+ table-start-pos))))
               (row-offset (- cur-line table-line))
               ;; count how many data rows precede the cursor line
               (row-coord (let ((data-row-idx -1)
                                (line-idx 0))
                            (dolist (child (cltpt/combinator:match-children fresh-match))
                              (case (cltpt/combinator:match-id child)
                                (cltpt/org-mode::table-row
                                 (incf data-row-idx)
                                 (when (= line-idx row-offset)
                                   (return data-row-idx))
                                 (incf line-idx))
                                (cltpt/org-mode::table-hrule
                                 (incf line-idx))
                                (cltpt/org-mode::table-row-separator
                                 nil)))))
               (target-cell (when row-coord
                              (cltpt/org-mode:get-cell-at-coordinates
                               new-match
                               (cons target-col row-coord))))
               (cursor-pos (when target-cell
                             (+ (1+ table-start-pos)
                                (cltpt/combinator:match-begin-absolute target-cell)))))
          (when cursor-pos
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point (lem:current-buffer)
                                                              cursor-pos))))))))

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
      (containing-list (getf (nth (first path) data) :children) (rest path))))

(defun item-index-in-list (path)
  "return the index of the item within its containing list."
  (car (last path)))

(defun last-two-bullets (items)
  "return (values last-bullet second-to-last-bullet) from ITEMS, or nils."
  (let ((tail (last items 2)))
    (values (when tail (getf (car (last tail)) :bullet))
            (when (cdr tail) (getf (car tail) :bullet)))))

(defun renumber-items (items prev-bullet prev-prev-bullet)
  "renumber ITEMS sequentially starting after PREV-BULLET.
PREV-PREV-BULLET is used to disambiguate roman vs alphabetic markers."
  (let ((prev-b prev-bullet)
        (prev-prev-b prev-prev-bullet))
    (dolist (item items)
      (let ((new-b (if prev-b
                       (next-bullet prev-b prev-prev-b)
                       (first-bullet (getf item :bullet)))))
        (setf prev-prev-b prev-b)
        (setf prev-b new-b)
        (setf (getf item :bullet) new-b)))))

(defun indent-item-in-list (items item-idx &key with-children)
  "indent the item at ITEM-IDX in ITEMS, making it a child of its previous sibling.
when WITH-CHILDREN is nil, the item's children are detached and placed as siblings of the item
at the new level. when WITH-CHILDREN is t, children move along.
modifies ITEMS destructively. returns (values indent-change old-bullet new-bullet)
where indent-change is the content-offset at the new child level, or nil on failure."
  (if (= item-idx 0)
      (lem:editor-error "cannot indent first item of a list")
      (let* ((item (nth item-idx items))
             (old-bullet (getf item :bullet))
             ;; prev-item is where the item we're indenting gets attached as a new sibling.
             (prev-item (nth (1- item-idx) items))
             (existing-children (getf prev-item :children))
             (orphaned-children (unless with-children
                                  (getf item :children))))
        ;; detach children if not moving them along. orphaned children are re-attached as siblings
        ;; at the new level later below.
        (unless with-children
          (setf (getf item :children) nil))
        ;; set bullet: continue existing sub-list sequence, or reset
        (setf (getf item :bullet)
              (if existing-children
                  (multiple-value-bind (last-b prev-b) (last-two-bullets existing-children)
                    (next-bullet last-b prev-b))
                  (first-bullet (getf item :bullet))))
        ;; add as last child of previous item
        (setf (getf prev-item :children)
              (append existing-children (list item)))
        ;; place orphaned children as siblings after the item in the new parent
        (when orphaned-children
          (renumber-items orphaned-children
                          (getf item :bullet)
                          (when existing-children
                            (getf (car (last existing-children)) :bullet)))
          (setf (getf prev-item :children)
                (append (getf prev-item :children) orphaned-children)))
        ;; remove from list
        (setf (cdr (nthcdr (1- item-idx) items)) (nthcdr (1+ item-idx) items))
        ;; renumber remaining items after the removed one
        (renumber-items (nthcdr item-idx items)
                        (getf prev-item :bullet)
                        (when (> item-idx 1)
                          (getf (nth (- item-idx 2) items) :bullet)))
        (let ((first-child-bullet (getf (car (getf prev-item :children)) :bullet)))
          (values (1+ (length first-child-bullet)) old-bullet (getf item :bullet))))))

(defun org-list-indent (list-obj with-children)
  (let ((col (lem:point-column (lem:current-point)))
        (line (lem:line-number-at-point (lem:current-point))))
    (multiple-value-bind (path data) (current-item-data list-obj)
      (when path
        (let* ((match (cltpt/base:text-object-match list-obj))
               (indent (or (getf (cltpt/combinator:match-props match) :indent) 0))
               (items (containing-list data path))
               (idx (item-index-in-list path)))
          (multiple-value-bind (indent-change old-bullet new-bullet)
              (indent-item-in-list items idx :with-children with-children)
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

(defun dedent-item-in-list (children sub-idx parent-item parent-list parent-idx
                            &key error-on-children)
  "dedent the item at SUB-IDX in CHILDREN out to PARENT-LIST after PARENT-IDX.
subsequent siblings always become children of the dedented item.
when ERROR-ON-CHILDREN is true and the item already has children, signals an error.
returns (values indent-change old-bullet new-bullet) where indent-change is the
content-offset at the old child level, or nil on failure."
  (let ((item (nth sub-idx children)))
    (when item
      (when (and error-on-children (getf item :children))
        (lem:editor-error "cannot dedent an item without its children"))
      (let ((old-bullet (getf item :bullet))
            (indent-change (1+ (length (getf (car children) :bullet)))))
        ;; adopt subsequent siblings as children
        (let ((subsequent (subseq children (1+ sub-idx))))
          (when subsequent
            (multiple-value-bind (prev-b prev-prev-b) (last-two-bullets (getf item :children))
              (renumber-items subsequent prev-b prev-prev-b))
            (setf (getf item :children)
                  (append (getf item :children) subsequent))))
        ;; set bullet to continue the parent level's sequence
        (setf (getf item :bullet)
              (next-bullet (getf parent-item :bullet)
                           (when (> parent-idx 0)
                             (getf (nth (1- parent-idx) parent-list) :bullet))))
        ;; remove item and all adopted siblings from parent's children
        (setf (getf parent-item :children)
              (subseq children 0 sub-idx))
        ;; insert after parent in its containing list
        (let ((cell (nthcdr parent-idx parent-list)))
          (setf (cdr cell) (cons item (cdr cell))))
        ;; renumber subsequent siblings in the parent list
        (renumber-items (nthcdr (+ parent-idx 2) parent-list)
                        (getf item :bullet)
                        (getf parent-item :bullet))
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
               (parent-list (containing-list data parent-path))
               (parent-idx (item-index-in-list parent-path))
               (parent-item (nth parent-idx parent-list))
               (children (getf parent-item :children)))
          (multiple-value-bind (indent-change old-bullet new-bullet)
              (dedent-item-in-list children
                                   sub-idx
                                   parent-item
                                   parent-list
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
    (when (and data idx target (>= target 0) (< target (length data)))
      ;; swap content and children, keeping bullets in place
      (let ((item-a (nth idx data))
            (item-b (nth target data)))
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
                                                              cursor-pos))))))))