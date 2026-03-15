(defpackage :organ/organ-mode
  (:use :cl :lem/transient)
  (:export
   :organ-mode :current-tree :*log-reschedule*
   :*organ-mode-keymap*))

(in-package :organ/organ-mode)

;; organ-redraw-buffer is written in highlighting.lisp
(declaim (ftype function organ-redraw-buffer))

;; incremental parsing doesnt yet work correctly.
;; while the functionality mostly already exists in 'cltpt', it hasnt been fully
;; ported to organ, and the buffer is redrawn on every change regardless.
(defvar *organ-enable-incremental-changes*
  nil
  "whether to enable incremental changes in `organ-mode'.")

(defvar *log-reschedule*
  t
  "when non-nil, log rescheduling and redeadlining events under the header.")

(defvar *org-table-add-row-on-tab* t
  "whether pressing tab in the last cell of an org-table will reformat the table and add a new empty row.

when nil, it will only reformat the table and the cursor will remain in the last cell.")

(define-transient *organ-mode-export-keymap*
  :display-style :row
  :description "organ-mode export keymap"
  (:key "d" :type :choice :description "output directory")
  (:key "l"
   :description "latex export dispatch"
   :suffix (:keymap
            :display-style :column
            :description "organ-mode latex export"
            (:key "l"
             :suffix (lambda ()
                       (convert-to-file cltpt:*latex*))
             :description "export to latex file")
            (:key "L"
             :suffix (lambda ()
                       (convert-to-buffer cltpt:*latex*))
             :description "export to latex buffer")
            (:key "p" :active-p nil :suffix 'test :description "export to pdf")
            (:key "o" :active-p nil :suffix 'test :description "export to latex file, convert to pdf, open the pdf.")))
  (:key "h"
   :description "html export dispatch"
   :suffix (:keymap
            :display-style :column
            :description "organ-mode html export"
            (:key "h"
             :suffix (lambda ()
                       (convert-to-file cltpt:*html*))
             :description "export to html file")
            (:key "H"
             :suffix (lambda ()
                       (convert-to-buffer cltpt:*html*))
             :description "export to html buffer")
            (:key "o" :active-p nil :suffix 'test :description "export to html file, open it."))))

(define-prefix *swap-up-prefix*
  :key "M-k"
  :behavior :drop
  :description "swap up (element-specific)")

(define-prefix *swap-down-prefix*
  :key "M-j"
  :behavior :drop
  :description "swap down (element-specific)")

(define-prefix *swap-right-prefix*
  :key "M-l"
  :behavior :drop
  :description "swap right (element-specific)")

(define-prefix *swap-left-prefix*
  :key "M-h"
  :behavior :drop
  :description "swap left (element-specific)")

;; this doesnt work properly yet, vi-mode's normal-mode return key takes priority over it.
(define-prefix *return-prefix*
  :key "Return"
  :behavior :drop
  :description "context-sensitive return")

(define-prefix *tab-prefix*
  :key "Tab"
  :behavior :drop
  :description "context-sensitive tab")

(define-prefix *shift-tab-prefix*
  :key "Shift-Tab"
  :behavior :drop
  :description "context-sensitive shift-tab")

(define-transient *organ-dwim-keymap*
  :description "dwim keymap"
  *swap-up-prefix*
  *swap-down-prefix*
  *swap-right-prefix*
  *swap-left-prefix*
  *return-prefix*
  *tab-prefix*
  *shift-tab-prefix*)

(define-transient *organ-mode-keymap*
  :display-style :row
  :description "organ-mode keymap"
  (:key "C-c n" :suffix 'organ-next-element)
  (:key "C-c p" :suffix 'organ-prev-element)
  (:key "C-c C-n" :suffix 'organ-next-header)
  (:key "C-c C-p" :suffix 'organ-prev-header)
  (:key "C-c C-x C-n" :suffix 'organ-next-link)
  (:key "C-c C-x C-p" :suffix 'organ-prev-link)
  (:key "C-c C-s" :suffix 'organ-schedule)
  (:key "C-c C-d" :suffix 'organ-deadline)
  (:key "C-c C-v C-n" :suffix 'organ-next-src-block)
  (:key "C-c C-v C-p" :suffix 'organ-prev-src-block)
  (:key "C-c C-e" :suffix *organ-mode-export-keymap* :description "export dispatch")
  *organ-dwim-keymap*)

(defvar *organ-mode-hook*
  '((organ-mode-init-all . 0)))

(lem:define-major-mode organ-mode nil
  (:name "organ-mode"
   :keymap *organ-mode-keymap*
   :mode-hook *organ-mode-hook*)
  (setf (lem:variable-value 'lem:enable-syntax-highlight) t))

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode organ-mode))
  (list *organ-mode-keymap*))

(lem:define-file-type ("org") organ-mode)

(defmethod lem/transient:mode-transient-keymap ((mode organ-mode))
  *organ-mode-keymap*)

(defun organ-mode-init-all ()
  "called when organ-mode is started, adds modification hooks to reparse buffer."
  (init-tree)
  (let ((buf (lem:current-buffer)))
    (lem:add-hook
     (lem:variable-value
      'lem:after-change-functions
      :buffer buf)
     'update-tree)))

(defun init-tree ()
  (let* ((buf (lem:current-buffer))
         (buffer-contents (lem:buffer-text buf))
         (cltpt-tree (cltpt/base:parse cltpt/org-mode:*org-mode* buffer-contents)))
    (setf (lem:buffer-value buf 'cltpt-tree) cltpt-tree)
    (organ-redraw-buffer buf)))

(defun update-tree (start-point end-point length)
  "updates the organ-mode AST of the current buffer."
  (let* ((buf (lem:current-buffer))
         (buffer-contents (lem:buffer-text buf))
         (new-text (lem:points-to-string start-point end-point))
         (begin-pos (1- (lem:position-at-point start-point)))
         (end-pos (1- (lem:position-at-point end-point)))
         (cltpt-tree (lem:buffer-value buf 'cltpt-tree)))
    (setf (lem:buffer-value buf 'cltpt-tree)
          (cltpt/base:parse cltpt/org-mode:*org-mode* (lem:buffer-text buf)))
    (organ-redraw-buffer buf)))

(defun current-tree ()
  (lem:buffer-value (lem:current-buffer) 'cltpt-tree))

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

;; this used calendar-mode.lisp directly instead of the popup
;; (lem:define-command edit-timestamp () ()
;;   "edit the timestamp at the cursor using `calendar-mode'."
;;   (let* ((obj (cltpt/base:child-at-pos (current-tree) (current-pos)))
;;          (source-buffer (lem:current-buffer)))
;;     ;; (lem:message "DEBUG: ~A" (cltpt/tree/outline:render-tree obj))
;;     (if (typep obj 'cltpt/org-mode::org-timestamp)
;;         (organ/calendar-mode:calendar-with-callback
;;          (lambda (dates)
;;            (when dates
;;              (let ((new-date (car dates)))
;;                (lem:with-current-buffer source-buffer
;;                  (organ/utils:replace-text-between-positions
;;                   source-buffer
;;                   (1+ (cltpt/base:text-object-begin-in-root obj))
;;                   (1+ (cltpt/base:text-object-end-in-root obj))
;;                   (organ/utils:format-timestamp new-date)))
;;                (lem:message "replaced ~A with ~A"
;;                             (cltpt/base:text-object-text obj)
;;                             new-date))))
;;          "*calendar*"
;;          nil)
;;         (lem:message "object under cursor (~A) isnt a timestamp."
;;                      (type-of obj)))))

(defun current-text-obj ()
  (let ((text-obj (cltpt/base:child-at-pos
                   (current-tree)
                   (organ/utils:current-pos))))
    text-obj))

(lem:define-command organ-insert-timestamp () ()
  "if there is a timestamp at the cursor, edit it using `popup-calendar', otherwise insert new timestamp."
  (let* ((obj (current-text-obj))
         (pt (lem:buffer-point (lem:current-buffer)))
         (source-buffer (lem:current-buffer))
         (new-date (organ/popup-calendar:popup-calendar-prompt "date: ")))
    (when new-date
      (lem:with-current-buffer source-buffer
        (if (typep obj 'cltpt/org-mode::org-timestamp)
            (progn
              (organ/utils:replace-text-between-positions
               source-buffer
               (1+ (cltpt/base:text-object-begin-in-root obj))
               (1+ (cltpt/base:text-object-end-in-root obj))
               (organ/utils:format-timestamp new-date))
              (lem:message "replaced ~A with ~A" (cltpt/base:text-object-text obj) new-date))
            (lem:insert-string pt (organ/utils:format-timestamp new-date)))))))

(defun organ-set-action-timestamp (action header source-buffer record-type log-keyword)
  "prompt for a date and insert/update an ACTION (e.g., SCHEDULED, DEADLINE) timestamp under the HEADER."
  (let ((new-date (organ/popup-calendar:popup-calendar-prompt
                   (format nil "~A date: " action))))
    (when new-date
      (lem:with-current-buffer source-buffer
        (let* ((task (cltpt/base:text-object-property header :task))
               (records (when task
                          (cltpt/agenda:task-records task)))
               (record (find-if
                        (lambda (r)
                          (typep r record-type))
                        records)))
          (if record
              ;; replace existing action timestamp text
              (let ((action-match (organ/utils:find-header-action header action)))
                (if action-match
                    (let* ((ts-match (cltpt/combinator:find-submatch
                                      action-match
                                      'cltpt/org-mode::timestamp))
                           (begin-pos (cltpt/combinator:match-begin-absolute ts-match))
                           (end-pos (cltpt/combinator:match-end-absolute ts-match))
                           (old-ts-text (cltpt/base:text-object-match-text header ts-match)))
                      ;; log entry first then timestamp
                      (when *log-reschedule*
                        (organ/utils:insert-header-log-entry
                         source-buffer
                         header
                         (format nil "- ~A from \"[~A]\" on ~A"
                                 log-keyword
                                 (remove #\> (remove #\< old-ts-text))
                                 (organ/utils:format-inactive-timestamp-with-time))))
                      (organ/utils:replace-text-between-positions
                       source-buffer
                       (1+ begin-pos)
                       (1+ end-pos)
                       (organ/utils:format-timestamp new-date)))
                    (lem:editor-error "could not locate ~A timestamp in header." action)))
              ;; insert new action timestamp
              (organ/utils:append-header-action
               source-buffer
               header
               (format nil "~A: ~A" action (organ/utils:format-timestamp new-date)))))))))

(lem:define-command organ-schedule () ()
  "prompt for a date and insert/update a SCHEDULED timestamp under the current org-header."
  (let ((header (organ/utils:find-node-at-pos
                 (current-tree)
                 (organ/utils:current-pos)
                 'cltpt/org-mode:org-header))
        (source-buffer (lem:current-buffer)))
    (if (not header)
        (lem:editor-error "not inside an org-header.")
        (organ-set-action-timestamp
         "SCHEDULED"
         header
         source-buffer
         'cltpt/agenda/task::record-scheduled
         "Rescheduled"))))

(lem:define-command organ-deadline () ()
  "prompt for a date and insert/update a DEADLINE timestamp under the current org-header."
  (let ((header (organ/utils:find-node-at-pos
                 (current-tree)
                 (organ/utils:current-pos)
                 'cltpt/org-mode:org-header))
        (source-buffer (lem:current-buffer)))
    (if (not header)
        (lem:editor-error "not inside an org-header.")
        (organ-set-action-timestamp
         "DEADLINE"
         header
         source-buffer
         'cltpt/agenda/task::record-deadline
         "New deadline"))))

;; this currently only works for links that 'resolve' to filepaths
(lem:define-command organ-open-at-point () ()
  "open the link at point."
  (let* ((obj (cltpt/base:child-at-pos (current-tree)
                                       (organ/utils:current-pos)))
         (resolved (cltpt/base:text-link-resolve obj))
         (link (cltpt/base:text-link-link obj))
         (dest (cltpt/base:link-dest link))
         (source-buffer (lem:current-buffer))
         (cltpt/base:text-link-link obj))
    (when (typep obj 'cltpt/base::text-link)
      (let ((dest-filepath
              (typecase resolved
                (pathname (cltpt/file-utils:ensure-filepath-string resolved))
                (cltpt/roam:node (cltpt/roam:node-file resolved))
                (t dest))))
        (if (probe-file dest-filepath)
            (lem:find-file dest-filepath)
            (lem:editor-error "file ~A doesnt exist" dest-filepath))))))

(defmethod org-table-navigate ((text-obj cltpt/org-mode:org-table) x-shift y-shift)
  (let* ((match (cltpt/base:text-object-match text-obj))
         (table-str (cltpt/base:text-object-match-text text-obj match))
         (initial-pos (organ/utils:current-pos))
         (table-start-pos (cltpt/combinator:match-begin-absolute match))
         (cell (cltpt/tree:tree-find-if
                match
                (lambda (submatch)
                  (and (>= (1+ initial-pos) (cltpt/combinator:match-begin-absolute submatch))
                       (<= initial-pos (cltpt/combinator:match-end-absolute submatch))
                       (string= (cltpt/combinator:match-id submatch) 'table-cell)))))
         (effective-cell (or cell
                             (let ((best))
                               (cltpt/tree:tree-walk
                                match
                                (lambda (submatch)
                                  (when (and (>= (1+ initial-pos)
                                                 (cltpt/combinator:match-begin-absolute submatch))
                                             (string= (cltpt/combinator:match-id submatch)
                                                      'table-cell))
                                    (setf best submatch))))
                               best))))
    (if (not effective-cell)
        ;; if no cell found anywhere, just reformat and don't move the cursor.
        (let ((new-table-str (cltpt/org-mode:reformat-table table-str match)))
          (organ/utils:replace-submatch-text* (lem:current-buffer) match new-table-str))
        (let* ((current-coords (cltpt/org-mode:get-cell-coordinates effective-cell))
               (new-table-str (cltpt/org-mode:reformat-table table-str match)))
          (multiple-value-bind (new-table-match pos)
              (cltpt/org-mode:org-table-matcher
               nil
               (cltpt/reader:reader-from-string new-table-str)
               0)
            (let* ((width (cltpt/org-mode:get-table-width new-table-match))
                   (height (cltpt/org-mode:get-table-height new-table-match))
                   (current-linear (+ (* (cdr current-coords) width) (car current-coords)))
                   (raw-shift (+ x-shift (* y-shift width)))
                   (shift-linear (if (and (null cell) (< raw-shift 0))
                                     0
                                     raw-shift))
                   (target-linear (max 0 (+ current-linear shift-linear)))
                   (new-x (mod target-linear width))
                   (new-y (floor target-linear width))
                   ;; extend table if moving past last row, otherwise reuse
                   (final-table-str
                     (if (>= new-y height)
                         (cltpt/org-mode:list-to-table-string
                          (append (cltpt/org-mode:table-match-to-list
                                   new-table-str
                                   new-table-match)
                                  (loop repeat (- new-y (1- height))
                                        collect (loop repeat width collect ""))))
                         new-table-str))
                   (final-match
                     (if (string= final-table-str new-table-str)
                         new-table-match
                         (cltpt/org-mode:org-table-matcher
                          nil
                          (cltpt/reader:reader-from-string final-table-str)
                          0)))
                   (final-height (cltpt/org-mode:get-table-height final-match))
                   (target-coords (cons new-x (min new-y (1- final-height))))
                   (target-cell (cltpt/org-mode:get-cell-at-coordinates
                                 final-match
                                 target-coords))
                   (cursor-pos (if target-cell
                                   (+ table-start-pos
                                      (cltpt/combinator:match-begin-absolute target-cell)
                                      1)
                                   table-start-pos)))
              (organ/utils:replace-submatch-text* (lem:current-buffer) match final-table-str)
              (lem:move-point (lem:current-point)
                              (organ/utils:char-offset-to-point (lem:current-buffer)
                                                                cursor-pos))))))))

(defun list-item-info (list-obj)
  "extract bullet info from the parsed tree for the list-item at point. returns (values indent bullet-str prev-bullet-str) or nil."
  (let* ((match (cltpt/base:text-object-match list-obj))
         (buf-text (lem:buffer-text (lem:current-buffer)))
         (pos (organ/utils:current-pos))
         (items (cltpt/combinator:match-children match)))
    (loop for item in items
          for i from 0
          when (and (<= (cltpt/combinator:match-begin-absolute item) pos)
                    (<= pos (cltpt/combinator:match-end-absolute item)))
            return
            (let* ((bullet-node (cltpt/combinator/match:find-direct-match-child-by-id
                                 item
                                 'cltpt/org-mode::list-item-bullet))
                   (bullet-str (when bullet-node
                                 (cltpt/combinator:match-text bullet-node buf-text)))
                   (indent (or (getf (cltpt/combinator:match-props item) :indent) 0))
                   (prev-item (when (> i 0)
                                (nth (1- i) items)))
                   (prev-bullet-node (when prev-item
                                       (cltpt/combinator/match:find-direct-match-child-by-id
                                        prev-item
                                        'cltpt/org-mode::list-item-bullet)))
                   (prev-bullet-str (when prev-bullet-node
                                      (cltpt/combinator:match-text prev-bullet-node buf-text))))
              (values indent bullet-str prev-bullet-str)))))

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

(defun organ-list-newline (list-obj)
  "insert a new list entry on the next line with the correct bullet and indentation.

LIST-OBJ is the org-list text object at point."
  (multiple-value-bind (indent bullet prev-bullet) (list-item-info list-obj)
    (when (and indent bullet)
      (let ((new-bullet (next-bullet bullet prev-bullet))
            (indent-str (make-string indent :initial-element #\space))
            (pt (lem:current-point)))
        (lem:line-end pt)
        (lem:insert-character pt #\newline)
        (lem:insert-string pt (format nil "~A~A " indent-str new-bullet))))))

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

(defun convert-buffer (dest-format)
  (let* ((tree (current-tree))
         (output (cltpt:convert-document cltpt/org-mode:*org-mode* dest-format tree)))
    output))

(defun convert-to-buffer (dest-format)
  (let ((buffer (lem:make-buffer "*organ-mode-out*"))
        (out (convert-buffer dest-format)))
    (lem:with-current-buffer buffer
      (lem:insert-string (lem:buffer-start-point buffer) out))
    (lem:pop-to-buffer buffer)))

(defun convert-to-file (dest-format)
  (let* ((extension (cond ((eq dest-format cltpt:*html*) "html")
                          ((eq dest-format cltpt:*latex*) "tex")
                          (t "txt")))
         (temp-file (cltpt/file-utils:temp-file "organ-export" extension))
         (buffer (lem:find-file-buffer temp-file))
         (out (convert-buffer dest-format)))
    (cltpt/file-utils:write-file temp-file out)
    ;; make buffer re-read the stuff we wrote
    (lem:with-current-buffer buffer
      (lem-core/commands/file:revert-buffer t))
    (lem:pop-to-buffer buffer)))

;; this is a special case where we also care about current_pos-1
(defun find-node-at-current-pos (type)
  (let ((pos (organ/utils:current-pos))
        (tree (current-tree)))
    ;; find enclosing element: try text-obj parent-walk first, fall back to pos-1 for
    ;; the boundary case (end of last list item where the org-list region doesnt include
    ;; the trailing newline).
    (when tree
      (or (organ/utils:find-node-at-pos
           tree
           pos
           type)
          (when (> pos 0)
            (organ/utils:find-node-at-pos
             tree
             (1- pos)
             type))))))

(defun pos-on-first-line-of-obj-p (obj pos)
  "return T if POS is on the first line of OBJ."
  (let* ((begin (cltpt/base:text-object-begin-in-root obj))
         (text (cltpt/base:text-object-text obj))
         (nl (position #\newline text)))
    (and (>= pos begin)
         (< pos (+ begin (or nl (length text)))))))

(defun pos-on-last-line-of-obj-p (obj pos)
  "return T if POS is on the last content line of OBJ. skips a trailing newline if present."
  (let* ((begin (cltpt/base:text-object-begin-in-root obj))
         (text (cltpt/base:text-object-text obj))
         (trail (if (and (> (length text) 0)
                         (char= (char text (1- (length text))) #\newline))
                    1 0))
         (search-end (- (length text) trail))
         (last-nl (position #\newline text :end search-end :from-end t)))
    (and last-nl
         (>= pos (+ begin last-nl 1))
         (< pos (+ begin search-end)))))

(defun find-header-at-title-line ()
  "return the org-header under the cursor if the cursor is on its title line, else return nil."
  (let ((pos (organ/utils:current-pos))
        (header (find-node-at-current-pos 'cltpt/org-mode:org-header)))
    (when (and header (pos-on-first-line-of-obj-p header pos))
      header)))

(defun find-block-at-delimiter-line ()
  "return the org-block or org-src-block under the cursor if on its opening or closing delimiter line."
  (let ((pos (organ/utils:current-pos))
        (blk (or (find-node-at-current-pos 'cltpt/org-mode:org-src-block)
                 (find-node-at-current-pos 'cltpt/org-mode:org-block))))
    (when (and blk
               (or (pos-on-first-line-of-obj-p blk pos)
                   (pos-on-last-line-of-obj-p blk pos)))
      blk)))

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
             (text-later   (subseq buf-text split region-end))
             (at-buffer-end (= region-end (length buf-text)))
             ;; when at buffer end, text-later moves to middle (gains separator
             ;; newline) and text-earlier moves to end (loses separator newline).
             ;; add one newline after text-later, strip one trailing \n from
             ;; text-earlier.
             (earlier-for-swap
               (if (and at-buffer-end
                        (> (length text-earlier) 0)
                        (char= (char text-earlier (1- (length text-earlier)))
                               #\newline))
                   (subseq text-earlier 0 (1- (length text-earlier)))
                   text-earlier))
             (replacement (if at-buffer-end
                              (concatenate 'string
                                           text-later
                                           (string #\newline)
                                           earlier-for-swap)
                              (concatenate 'string text-later text-earlier)))
             (new-pos (if (= direction 1)
                          (+ region-start (length text-later) (if at-buffer-end 1 0))
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
                          (+ begin-target (- len-target len-block))
                          begin-target)))
        (swap-text-objects blk target)
        (lem:move-point (lem:current-point)
                        (organ/utils:char-offset-to-point (lem:current-buffer) new-pos))))))

(defmethod prefix-active-p ((p (eql *swap-up-prefix*)))
  (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
        (header-found (find-header-at-title-line))
        (block-found  (find-block-at-delimiter-line)))
    (or table-found list-found header-found block-found)))

(defmethod prefix-suffix ((p (eql *swap-up-prefix*)))
  (lambda ()
    (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
          (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
          (header-found (find-header-at-title-line))
          (block-found  (find-block-at-delimiter-line)))
      (cond
        (table-found  (org-table-move-row table-found -1))
        (list-found   (org-list-move-item list-found -1))
        (header-found (org-header-move header-found -1))
        (block-found  (org-block-move block-found -1))))))

(defmethod prefix-active-p ((p (eql *swap-down-prefix*)))
  (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
        (header-found (find-header-at-title-line))
        (block-found  (find-block-at-delimiter-line)))
    (or table-found list-found header-found block-found)))

(defmethod prefix-suffix ((p (eql *swap-down-prefix*)))
  (lambda ()
    (let ((table-found  (find-node-at-current-pos 'cltpt/org-mode:org-table))
          (list-found   (find-node-at-current-pos 'cltpt/org-mode:org-list))
          (header-found (find-header-at-title-line))
          (block-found  (find-block-at-delimiter-line)))
      (cond
        (table-found  (org-table-move-row table-found 1))
        (list-found   (org-list-move-item list-found 1))
        (header-found (org-header-move header-found 1))
        (block-found  (org-block-move block-found 1))))))

(defmethod prefix-active-p ((p (eql *swap-left-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *swap-left-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-move-column table-found -1)))))

(defmethod prefix-active-p ((p (eql *swap-right-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *swap-right-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-move-column table-found 1)))))

(defmethod prefix-active-p ((p (eql *return-prefix*)))
  (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table))
        (list-found (find-node-at-current-pos 'cltpt/org-mode:org-list)))
    (or table-found
        (and list-found (lem:end-line-p (lem:current-point))))))

(defmethod prefix-active-p ((p (eql *tab-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *tab-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-navigate table-found 1 0)))))

(defmethod prefix-active-p ((p (eql *shift-tab-prefix*)))
  (find-node-at-current-pos 'cltpt/org-mode:org-table))

(defmethod prefix-suffix ((p (eql *shift-tab-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table)))
      (when table-found
        (org-table-navigate table-found -1 0)))))

(defmethod prefix-suffix ((p (eql *return-prefix*)))
  (lambda ()
    (let ((table-found (find-node-at-current-pos 'cltpt/org-mode:org-table))
          (list-found (find-node-at-current-pos 'cltpt/org-mode:org-list)))
      (cond
        (table-found (org-table-navigate table-found 0 1))
        (list-found (organ-list-newline list-found))))))