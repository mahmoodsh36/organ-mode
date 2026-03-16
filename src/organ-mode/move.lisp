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