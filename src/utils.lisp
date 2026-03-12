(defpackage :organ/utils
  (:use :cl)
  (:export
   :char-offset-to-point :point-to-char-offset :current-pos :replace-text-between-positions
   :*weekday-names*
   :format-timestamp :format-inactive-timestamp-with-time
   :replace-submatch-text :replace-submatch-text*
   :find-parent-of-type :find-node-at-pos :find-node-at-point
   :insert-header-log-entry
   :append-header-action :find-header-action :remove-header-action))

(in-package :organ/utils)

(defun char-offset-to-point (buf offset)
  (let ((start (lem:copy-point (lem:buffer-start-point buf) :temporary)))
    (lem:character-offset start offset)
    start))

(defun point-to-char-offset (point)
  (1- (lem:position-at-point point)))

(defun current-pos ()
  (point-to-char-offset (lem:current-point)))

(defun delete-text-between-positions (buffer start-pos end-pos)
  "delete text between two absolute positions (indices) in the buffer."
  (when (> start-pos end-pos)
    (rotatef start-pos end-pos))
  (let ((start-point (lem:copy-point (lem:buffer-start-point buffer) :temporary))
        (end-point (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
    (lem:move-to-position start-point start-pos)
    (lem:move-to-position end-point end-pos)
    (lem:delete-between-points start-point end-point)))

(defun replace-text-between-positions (buffer start-pos end-pos replacement-text)
  "replace text between two absolute positions in the buffer with replacement text."
  (when (> start-pos end-pos)
    (rotatef start-pos end-pos))
  (let ((start-point (lem:copy-point (lem:buffer-start-point buffer) :temporary)))
    (unless (lem:move-to-position start-point start-pos)
      (error "start position ~A is out of buffer bounds" start-pos))
    (let ((end-point (lem:copy-point start-point :temporary)))
      (unless (lem:move-to-position end-point end-pos)
        (error "end position ~A is out of buffer bounds" end-pos))
      (lem:delete-between-points start-point end-point)
      (lem:insert-string start-point replacement-text))))

(defun replace-submatch-text* (buffer submatch new-text)
  (replace-text-between-positions
   buffer
   (1+ (cltpt/combinator:match-begin-absolute submatch))
   (1+ (cltpt/combinator:match-end-absolute submatch))
   new-text))

(defun replace-submatch-text (buffer text-obj submatch-id new-text)
  (let* ((match (cltpt/base:text-object-match text-obj))
         (submatch (cltpt/combinator:find-submatch match submatch-id)))
    (replace-submatch-text*
     buffer
     submatch
     new-text)))

(defparameter *weekday-names*
  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  "short weekday names indexed by day-of-week (0=Sunday).")

(defun format-timestamp (&optional (timestamp (local-time:now)))
  "return a string like <2024-01-30 Tue> for the given TIMESTAMP."
  (let* ((base (local-time:format-timestring
                nil
                timestamp
                :format '(:year "-" (:month 2) "-" (:day 2))))
         (weekday (nth (local-time:timestamp-day-of-week timestamp)
                       *weekday-names*)))
    (format nil "<~A ~A>" base weekday)))

(defun format-inactive-timestamp-with-time (&optional (timestamp (local-time:now)))
  "return an inactive timestamp with time like [2024-01-30 Tue 15:02:11] for the given TIMESTAMP."
  (let* ((base (local-time:format-timestring
                nil
                timestamp
                :format '(:year "-" (:month 2) "-" (:day 2))))
         (time-str (local-time:format-timestring
                    nil
                    timestamp
                    :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))
         (weekday (nth (local-time:timestamp-day-of-week timestamp)
                       *weekday-names*)))
    (format nil "[~A ~A ~A]" base weekday time-str)))

(defun find-parent-of-type (node type)
  "walk up from NODE using `cltpt/base:text-object-parent' to find the first ancestor of TYPE."
  (loop for current = node then (cltpt/base:text-object-parent current)
        while current
        when (typep current type)
          return current))

(defun find-node-at-pos (tree pos type)
  "find the node of TYPE at the given character offset POS in TREE."
  (let ((node (cltpt/base:child-at-pos tree pos)))
    (find-parent-of-type node type)))

(defun find-node-at-point (tree point type)
  "find the node of TYPE at the given lem POINT in TREE."
  (find-node-at-pos tree (point-to-char-offset point) type))

(defun last-header-action (header)
  "return the last action match (active or inactive) in HEADER, or nil."
  (let* ((header-match (cltpt/base:text-object-match header))
         (all-active (cltpt/combinator:find-submatch-all
                       header-match
                       'cltpt/org-mode::action-active))
         (all-inactive (cltpt/combinator:find-submatch-all
                         header-match
                         'cltpt/org-mode::action-inactive)))
    (car (last (append all-active all-inactive)))))

(defun header-after-title-pos (header)
  "return the absolute position at the end of HEADER's title line."
  (let* ((header-str (cltpt/base:text-object-text header))
         (nl (position #\newline header-str)))
    (+ (cltpt/base:text-object-begin-in-root header)
       (or nl (length header-str)))))

(defun append-header-action (buffer header action-str)
  "append ACTION-STR on HEADER's action line, or create a new line after the title."
  (let ((last-action (last-header-action header)))
    (if last-action
        (let ((insert-pos (cltpt/combinator:match-end-absolute last-action)))
          (replace-text-between-positions
           buffer
           (1+ insert-pos)
           (1+ insert-pos)
           (format nil " ~A" action-str)))
        (let ((insert-pos (header-after-title-pos header)))
          (replace-text-between-positions
           buffer
           (1+ insert-pos)
           (1+ insert-pos)
           (format nil "~%~A" action-str))))))

(defun find-header-action (header action-name)
  "find an action match by ACTION-NAME in HEADER."
  (let* ((header-match (cltpt/base:text-object-match header))
         (matches (append
                   (cltpt/combinator:find-submatch-all
                    header-match
                    'cltpt/org-mode::action-active)
                   (cltpt/combinator:find-submatch-all
                    header-match
                    'cltpt/org-mode::action-inactive))))
    (find-if
     (lambda (m)
       (string-equal
        (cltpt/base:text-object-match-text
         header
         (cltpt/combinator:find-submatch m 'cltpt/org-mode::name))
        action-name))
     matches)))

(defun remove-header-action (buffer header action-name)
  "remove an action by ACTION-NAME from HEADER's action line."
  (let ((action-match (find-header-action header action-name)))
    (when action-match
      (let* ((begin-pos (cltpt/combinator:match-begin-absolute action-match))
             (end-pos (cltpt/combinator:match-end-absolute action-match))
             (header-offset (cltpt/base:text-object-begin-in-root header))
             (rel-begin (- begin-pos header-offset)))
        ;; also remove the leading space if there is one
        (let ((adjusted-begin
                (if (and (> rel-begin 0)
                         (char= (char (cltpt/base:text-object-text header)
                                      (1- rel-begin))
                                #\space))
                    (1- begin-pos)
                    begin-pos)))
          (replace-text-between-positions
           buffer
           (1+ adjusted-begin)
           (1+ end-pos)
           ""))))))

(defun insert-header-log-entry (buffer header log-text)
  "insert LOG-TEXT as a new log line under HEADER's metadata in BUFFER."
  (let* ((last-action (last-header-action header))
         (insert-pos
           (if last-action
               (cltpt/combinator:match-end-absolute last-action)
               (header-after-title-pos header)))
         (line-end-point
           (lem:copy-point
            (lem:buffer-start-point buffer)
            :temporary)))
    (lem:move-to-position line-end-point (1+ insert-pos))
    (lem:line-end line-end-point)
    (lem:insert-string line-end-point (format nil "~%~A" log-text))))