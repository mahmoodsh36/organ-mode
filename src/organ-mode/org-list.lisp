(in-package :organ/organ-mode)

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

(defun org-list-newline ()
  "insert a new list entry on the next line with the correct bullet and indentation."
  (let ((list-obj (find-node-at-current-pos 'cltpt/org-mode:org-list)))
    (multiple-value-bind (indent bullet prev-bullet) (list-item-info list-obj)
      (when (and indent bullet)
        (let ((new-bullet (next-bullet bullet prev-bullet))
              (indent-str (make-string indent :initial-element #\space))
              (pt (lem:current-point)))
          (lem:line-end pt)
          (lem:insert-character pt #\newline)
          (lem:insert-string pt (format nil "~A~A " indent-str new-bullet)))))))