(in-package :organ/organ-mode)

(defun src-block-at-point ()
  (current-text-obj-ignore-newline 'cltpt/org-mode:org-src-block))

(defun eval-src-block-async (blk on-output on-done)
  "evaluate BLK in a thread, calling ON-OUTPUT with the output so far as it streams in and ON-DONE
with (output errors) at the end, both on the editor thread.
BLK is read from the thread while the user keeps editing. this is safe because after every edit
we reparse the buffer into a new tree instead of modifying the old one."
  (let ((lock (bt2:make-lock))
        ;; output the editor has not taken yet, and all the output it has taken so far.
        (unsent (make-string-output-stream))
        (sent "")
        (update-queued))
    (labels ((take-unsent ()
               "move the unsent output to SENT and return all of it. runs on the editor thread."
               (bt2:with-lock-held (lock)
                 (setf update-queued nil
                       sent (concatenate 'string sent (get-output-stream-string unsent)))))
             (on-chunk (chunk)
               (bt2:with-lock-held (lock)
                 (write-string chunk unsent)
                 ;; queue at most one update at a time. otherwise fast output floods the event queue
                 ;; and keystrokes wait behind every update. a queued update takes this chunk too
                 ;; when it runs.
                 (unless update-queued
                   (setf update-queued t)
                   (lem:send-event
                    (lambda ()
                      (funcall on-output (take-unsent))))))))
      (bt2:make-thread
       (lambda ()
         (let ((final-output "")
               (errors))
           (handler-case
               (multiple-value-bind (out errs)
                   (cltpt/org-mode:eval-block-streaming blk #'on-chunk)
                 (setf final-output (or out "")
                       errors errs))
             (error (e)
               (setf errors (princ-to-string e))))
           (lem:send-event
            (lambda ()
              (funcall on-done final-output errors)
              (lem:redraw-display)))))
       :name "organ-babel"))))

(defun insert-src-block-results (buffer marker output)
  "write OUTPUT as the results of the src block at MARKER in BUFFER. returns NIL if the block is gone."
  (lem:with-current-buffer buffer
    (let ((blk (organ/utils:find-node-at-point (lem:buffer-value buffer 'cltpt-tree)
                                               marker
                                               'cltpt/org-mode:org-src-block)))
      (when blk
        (organ/utils:apply-change buffer
                                  (cltpt/org-mode:org-src-block-results-change blk output))
        t))))

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
          (lambda (output)
            ;; value results (e.g. tables) are only meaningful once complete.
            (when (and (eq (cltpt/org-mode:org-src-block-result-type blk) :output)
                       (not (lem:deleted-buffer-p buffer)))
              (insert-src-block-results buffer marker output)))
          (lambda (output errors)
            (unwind-protect
                 (unless (lem:deleted-buffer-p buffer)
                   (when (and (string/= output "")
                              (not (insert-src-block-results buffer marker output)))
                     (lem:message "src block is gone, results discarded."))
                   (if (and errors (string/= errors ""))
                       (show-text-buffer "*organ-babel-errors*" errors)
                       (lem:message "evaluation done.")))
              (lem:delete-point marker)))))))))