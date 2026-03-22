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

(defun list-match-item-info (list-match buf-text pos)
  "extract bullet info from LIST-MATCH for the item at POS.
recurses into nested sub-lists when the cursor falls within one.
POS should already be adjusted for end-of-line boundaries (see `current-pos-no-newline').
returns (values indent bullet-str prev-bullet-str) or nil."
  (let ((items (cltpt/combinator:match-children list-match)))
    (loop for item in items
          for i from 0
          when (and (<= (cltpt/combinator:match-begin-absolute item) pos)
                    (<= pos (cltpt/combinator:match-end-absolute item)))
            return
            (let* ((content-node
                     (cltpt/combinator/match:find-direct-match-child-by-id
                      item
                      'cltpt/org-mode::list-item-content))
                   (sub-list
                     (when content-node
                       (cltpt/combinator/match:find-direct-match-child-by-id
                        content-node
                        'cltpt/org-mode::org-list))))
              (if (and sub-list
                       (<= (cltpt/combinator:match-begin-absolute sub-list) pos)
                       (<= pos (cltpt/combinator:match-end-absolute sub-list)))
                  (list-match-item-info sub-list buf-text pos)
                  (let* ((bullet-node
                           (cltpt/combinator/match:find-direct-match-child-by-id
                            item
                            'cltpt/org-mode::list-item-bullet))
                         (bullet-str (when bullet-node
                                       (cltpt/combinator:match-text bullet-node buf-text)))
                         (indent (or (getf (cltpt/combinator:match-props item) :indent) 0))
                         (prev-item (when (> i 0)
                                      (nth (1- i) items)))
                         (prev-bullet-node
                           (when prev-item
                             (cltpt/combinator/match:find-direct-match-child-by-id
                              prev-item
                              'cltpt/org-mode::list-item-bullet)))
                         (prev-bullet-str
                           (when prev-bullet-node
                             (cltpt/combinator:match-text prev-bullet-node buf-text))))
                    (values indent bullet-str prev-bullet-str)))))))

(defun list-item-info (list-obj)
  "extract bullet info from the parsed tree for the list-item at point. returns (values indent bullet-str prev-bullet-str) or nil."
  (list-match-item-info
   (cltpt/base:text-object-match list-obj)
   (lem:buffer-text (lem:current-buffer))
   (organ/utils:current-pos-no-newline)))

(defun bullet-marker (bullet)
  "extract the marker part of a bullet (everything before the trailing dot). returns nil for unordered bullets."
  (when (and (> (length bullet) 1)
             (char= (char bullet (1- (length bullet))) #\.))
    (subseq bullet 0 (1- (length bullet)))))

(defvar *roman-values*
  '((1000 "m") (900 "cm") (500 "d") (400 "cd")
    (100 "c") (90 "xc") (50 "l") (40 "xl")
    (10 "x") (9 "ix") (5 "v") (4 "iv") (1 "i"))
  "roman numeral value-to-string mapping, descending order.")

(defvar *roman-char-values*
  (loop for (val str) in *roman-values*
        when (= (length str) 1)
          collect (cons (char str 0) val))
  "char-to-value alist derived from `*roman-values*'.")

(defun roman-char-value (ch)
  (cdr (assoc (char-downcase ch) *roman-char-values*)))

(defun roman-to-int (str)
  "parse a roman numeral string. returns the integer value or nil."
  (when (zerop (length str))
    (return-from roman-to-int nil))
  (let ((total 0)
        (prev 0))
    (loop for i from (1- (length str)) downto 0
          for val = (roman-char-value (char str i))
          do (unless val
               (return-from roman-to-int nil))
             (if (< val prev)
                 (decf total val)
                 (incf total val))
             (setf prev val))
    total))

(defun int-to-roman (n &optional uppercase)
  "convert integer to a roman numeral string."
  (let ((result (with-output-to-string (s)
                  (dolist (pair *roman-values*)
                    (loop while (>= n (first pair))
                          do (write-string (second pair) s)
                             (decf n (first pair)))))))
    (if uppercase
        (string-upcase result)
        result)))

(defun alphabetic-successor-p (marker prev-marker)
  "true if MARKER is the single-char alphabetic successor of PREV-MARKER."
  (and prev-marker
       (= (length marker) 1)
       (= (length prev-marker) 1)
       (char= (char marker 0)
              (code-char (1+ (char-code (char prev-marker 0)))))))

(defun increment-marker (marker prev-marker)
  "return the next marker string, using PREV-MARKER to disambiguate roman vs alphabetic."
  (let ((num (ignore-errors (parse-integer marker))))
    (cond
      (num (format nil "~A" (1+ num)))
      ;; single-char that's both roman and alpha: check context
      ((and (= (length marker) 1)
            (roman-to-int marker)
            (alphabetic-successor-p marker prev-marker))
       (string (code-char (1+ (char-code (char marker 0))))))
      ;; roman numeral
      ((roman-to-int marker)
       (int-to-roman (1+ (roman-to-int marker))
                     (upper-case-p (char marker 0))))
      ;; alphabetic fallback
      (t (string (code-char (1+ (char-code (char marker (1- (length marker)))))))))))

(defun first-bullet (bullet)
  "return the first bullet of the same type as BULLET.
\"-\" stays \"-\", ordered bullets reset to their first value (\"1.\", \"a.\", \"i.\", etc.).
multi-character roman numerals (e.g. \"ii.\") are detected unambiguously.
single-character markers that are both roman and alphabetic (e.g. \"i.\") default to alphabetic."
  (if (string= bullet "-")
      "-"
      (let ((marker (bullet-marker bullet)))
        (cond
          ((null marker) bullet)
          ((digit-char-p (char marker 0)) "1.")
          ;; multi-char roman numerals are unambiguous
          ((and (> (length marker) 1) (roman-to-int marker))
           (if (lower-case-p (char marker 0)) "i." "I."))
          ((lower-case-p (char marker 0)) "a.")
          ((upper-case-p (char marker 0)) "A.")
          (t "1.")))))

(defun next-bullet (bullet &optional prev-bullet)
  "given a bullet like \"-\", \"1.\", \"ii.\", \"a.\", return the next bullet.

PREV-BULLET is used to disambiguate single-char roman vs alphabetic markers."
  (if (string= bullet "-")
      "-"
      (let ((marker (bullet-marker bullet))
            (prev-marker (bullet-marker prev-bullet)))
        (if marker
            (format nil "~A." (increment-marker marker prev-marker))
            bullet))))

(defun org-list-newline ()
  "insert a new list entry on the next line with the correct bullet and indentation."
  (let ((list-obj (current-text-obj-ignore-newline 'cltpt/org-mode:org-list)))
    (multiple-value-bind (indent bullet prev-bullet) (list-item-info list-obj)
      (when (and indent bullet)
        (let ((new-bullet (next-bullet bullet prev-bullet))
              (indent-str (make-string indent :initial-element #\space))
              (pt (lem:current-point)))
          (lem:line-end pt)
          (lem:insert-character pt #\newline)
          (lem:insert-string pt (format nil "~A~A " indent-str new-bullet)))))))