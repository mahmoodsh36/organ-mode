(in-package :organ/organ-mode)

(defun src-block-at-point ()
  (current-text-obj-ignore-newline 'cltpt/org-mode:org-src-block))

(defun eval-src-block-async (blk callback)
  "evaluate BLK in a thread and call CALLBACK with its (output errors) on the editor thread.
BLK is read from the thread while the user keeps editing. this is safe because after every edit
we reparse the buffer into a new tree instead of modifying the old one."
  (bt2:make-thread
   (lambda ()
     (let ((output)
           (errors))
       (handler-case
           (multiple-value-bind (out-rdr err-rdr)
               (cltpt/org-mode:eval-block blk)
             (setf output (and out-rdr (cltpt/reader:reader-to-string out-rdr))
                   errors (and err-rdr (cltpt/reader:reader-to-string err-rdr))))
         (error (e)
           (setf errors (princ-to-string e))))
       (lem:send-event
        (lambda ()
          (funcall callback output errors)
          (lem:redraw-display)))))
   :name "organ-babel"))

(defun insert-src-block-results (buffer marker output)
  "write OUTPUT as the results of the src block at MARKER in BUFFER."
  (let ((blk (organ/utils:find-node-at-point (lem:buffer-value buffer 'cltpt-tree)
                                             marker
                                             'cltpt/org-mode:org-src-block)))
    (if blk
        (organ/utils:apply-change buffer
                                  (cltpt/org-mode:org-src-block-results-change blk output))
        (lem:message "src block is gone, results discarded."))))

(lem:define-command organ-babel-execute-src-block () ()
  "evaluate the src block at point and insert its results."
  (let ((blk (src-block-at-point))
        (buffer (lem:current-buffer)))
    (cond
      ((null blk)
       (lem:editor-error "not inside a src block."))
      ((not (cltpt/babel:babel-supported-p (cltpt/org-mode:org-src-block-lang blk)))
       (lem:editor-error "unsupported language for ~A."
                         (cltpt/org-mode:org-src-block-lang blk)))
      (t
       ;; the buffer may change while the block runs, so track the block with a point that moves
       ;; with edits (:left-inserting moves past text inserted exactly at it).
       (let ((marker (organ/utils:char-offset-to-point
                      buffer
                      (cltpt/base:text-object-begin-in-root blk))))
         (setf marker (lem:copy-point marker :left-inserting))
         (lem:message "evaluating...")
         (eval-src-block-async
          blk
          (lambda (output errors)
            (unwind-protect
                 (unless (lem:deleted-buffer-p buffer)
                   (when output
                     (insert-src-block-results buffer marker output))
                   (if (and errors (string/= errors ""))
                       (show-text-buffer "*organ-babel-errors*" errors)
                       (lem:message "evaluation done.")))
              (lem:delete-point marker)))))))))