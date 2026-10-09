(in-package :organ/organ-mode)

(defun current-text-obj-ignore-newline (type)
  (let ((tree (current-tree)))
    (when tree
      (organ/utils:find-node-at-pos-ignore-newline
       tree
       (organ/utils:current-pos)
       type))))

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
                    1
                    0))
         (search-end (- (length text) trail))
         (last-nl (position #\newline text :end search-end :from-end t)))
    (and last-nl
         (>= pos (+ begin last-nl 1))
         (< pos (+ begin search-end)))))

(defun find-header-at-title-line ()
  "return the org-header under the cursor if the cursor is on its title line, else return nil."
  (let ((pos (organ/utils:current-pos))
        (header (current-text-obj-ignore-newline 'cltpt/org-mode:org-header)))
    (when (and header (pos-on-first-line-of-obj-p header pos))
      header)))

(defun find-block-at-delimiter-line ()
  "return the org-block or org-src-block under the cursor if on its opening or closing delimiter line."
  (let ((pos (organ/utils:current-pos))
        (blk (or (current-text-obj-ignore-newline 'cltpt/org-mode:org-src-block)
                 (current-text-obj-ignore-newline 'cltpt/org-mode:org-block))))
    (when (and blk
               (or (pos-on-first-line-of-obj-p blk pos)
                   (pos-on-last-line-of-obj-p blk pos)))
      blk)))

(defun show-text-buffer (name text)
  "show TEXT in the buffer called NAME, replacing whatever was in it, and return the buffer."
  (let ((buffer (lem:make-buffer name)))
    (lem:erase-buffer buffer)
    (lem:with-current-buffer buffer
      (lem:insert-string (lem:buffer-start-point buffer) text))
    (lem:pop-to-buffer buffer)
    buffer))