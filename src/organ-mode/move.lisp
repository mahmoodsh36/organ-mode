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