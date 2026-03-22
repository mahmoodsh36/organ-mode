(in-package :organ/organ-mode)

(defmethod object-closest-to-pos ((tree cltpt/base:text-object)
                                  pos
                                  direction
                                  &optional (predicate (lambda (&rest args) t)))
  "finds the text object closest to POS in DIRECTION (:forward or :backward).
for :forward, finds the object starting closest after POS.
for :backward, finds the object starting closest before POS."
  (let ((prune-test (ecase direction
                      (:forward #'<=)
                      (:backward #'>=)))
        (candidate-test (ecase direction
                          (:forward #'>)
                          (:backward #'<)))
        (better-test (ecase direction
                       (:forward #'<)
                       (:backward #'>)))
        (prune-accessor (ecase direction
                          (:forward #'cltpt/base:text-object-end-in-root)
                          (:backward #'cltpt/base:text-object-begin-in-root))))
    ;; prune: if the entire object is on the wrong side of pos, skip this branch.
    (when (funcall prune-test (funcall prune-accessor tree) pos)
      (return-from object-closest-to-pos nil))
    (let ((best-candidate))
      (when (and (funcall candidate-test (cltpt/base:text-object-begin-in-root tree) pos)
                 (funcall predicate tree))
        (setf best-candidate tree))
      (loop for child in (cltpt/base:text-object-children tree)
            do (let ((candidate (object-closest-to-pos child pos direction predicate)))
                 (when candidate
                   (if (or (null best-candidate)
                           (funcall better-test
                                    (cltpt/base:text-object-begin-in-root candidate)
                                    (cltpt/base:text-object-begin-in-root best-candidate)))
                       (setf best-candidate candidate)))))
      best-candidate)))

(lem:define-command organ-next-element () ()
  (organ-move-to-element :forward))

(lem:define-command organ-prev-element () ()
  (organ-move-to-element :backward))

(lem:define-command organ-next-header () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-header))))

(lem:define-command organ-prev-header () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-header))))

(lem:define-command organ-next-link () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-link))))

(lem:define-command organ-prev-link () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-link))))

(lem:define-command organ-next-src-block () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-src-block))))

(lem:define-command organ-prev-src-block () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-src-block))))

(lem:define-command organ-next-block () ()
  (organ-move-to-element :forward (lambda (obj) (typep obj 'cltpt/org-mode:org-block))))

(lem:define-command organ-prev-block () ()
  (organ-move-to-element :backward (lambda (obj) (typep obj 'cltpt/org-mode:org-block))))

(defun find-submatch-at-pos (text-obj match-id)
  "find a submatch with MATCH-ID at the cursor position within TEXT-OBJ.
returns (values submatch exact-p) where EXACT-P is t when the cursor is directly
inside the submatch, nil when the closest preceding submatch was used as fallback."
  (let ((match (cltpt/base:text-object-match text-obj))
        (pos (organ/utils:current-pos)))
    (let ((exact (cltpt/tree:tree-find-if
                  match
                  (lambda (submatch)
                    (and (>= (1+ pos) (cltpt/combinator:match-begin-absolute submatch))
                         (<= pos (cltpt/combinator:match-end-absolute submatch))
                         (string= (cltpt/combinator:match-id submatch) match-id))))))
      (if exact
          (values exact t)
          (let ((best))
            (cltpt/tree:tree-walk
             match
             (lambda (submatch)
               (when (and (>= (1+ pos) (cltpt/combinator:match-begin-absolute submatch))
                          (string= (cltpt/combinator:match-id submatch) match-id))
                 (setf best submatch))))
            (values best nil))))))

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