(in-package :organ/organ-mode)

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

(defun parse-table-string (table-str)
  "parse TABLE-STR into a match tree, returning the match object."
  (cltpt/org-mode:org-table-matcher
   nil
   (cltpt/reader:reader-from-string table-str)
   0))

(defun extend-table-if-needed (table-str table-match target-y)
  "if TARGET-Y exceeds the table height, append empty rows and return new string and match.
otherwise return the original string and match."
  (let ((height (cltpt/org-mode:get-table-height table-match))
        (width (cltpt/org-mode:get-table-width table-match)))
    (if (< target-y height)
        (values table-str table-match)
        (let ((extended-str
                (cltpt/org-mode:list-to-table-string
                 (append (cltpt/org-mode:table-match-to-list table-str table-match)
                         (loop repeat (- target-y (1- height))
                               collect (loop repeat width
                                             collect ""))))))
          (values extended-str (parse-table-string extended-str))))))

(defmethod org-table-navigate ((text-obj cltpt/org-mode:org-table) x-shift y-shift)
  (let* ((match (cltpt/base:text-object-match text-obj))
         (table-str (cltpt/base:text-object-match-text text-obj match))
         (table-start-pos (cltpt/combinator:match-begin-absolute match))
         (new-table-str (cltpt/org-mode:reformat-table table-str match)))
    (multiple-value-bind (effective-cell exact-p)
        (find-submatch-at-pos text-obj 'table-cell)
      ;; if no cell found, just reformat and don't move the cursor.
      (when (not effective-cell)
        (organ/utils:replace-submatch-text* (lem:current-buffer) match new-table-str)
        (return-from org-table-navigate))
      (let* ((current-coords (cltpt/org-mode:get-cell-coordinates effective-cell))
             (new-table-match (parse-table-string new-table-str))
             (width (cltpt/org-mode:get-table-width new-table-match))
             (current-linear (+ (* (cdr current-coords) width) (car current-coords)))
             (total-shift (if exact-p
                              (+ x-shift (* y-shift width))
                              (let ((raw (+ x-shift (* y-shift width))))
                                (if (minusp raw)
                                    0
                                    1))))
             (target-linear (max 0 (+ current-linear total-shift)))
             (new-x (mod target-linear width))
             (new-y (floor target-linear width)))
        (multiple-value-bind (final-table-str final-match)
            (extend-table-if-needed new-table-str new-table-match new-y)
          (let* ((final-height (cltpt/org-mode:get-table-height final-match))
                 (target-cell (cltpt/org-mode:get-cell-at-coordinates
                               final-match
                               (cons new-x (min new-y (1- final-height)))))
                 (cursor-pos (if target-cell
                                 (+ table-start-pos
                                    (cltpt/combinator:match-begin-absolute target-cell)
                                    1)
                                 table-start-pos)))
            (organ/utils:replace-submatch-text* (lem:current-buffer) match final-table-str)
            (lem:move-point (lem:current-point)
                            (organ/utils:char-offset-to-point (lem:current-buffer)
                                                              cursor-pos))))))))