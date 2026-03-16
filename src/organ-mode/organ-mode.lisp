(defpackage :organ/organ-mode
  (:use :cl :lem/transient)
  (:export
   :organ-mode
   :current-tree
   :*log-reschedule*
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
  (:key "C-c C-o" :suffix 'organ-open-at-point)
  )

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

(defun set-action-timestamp (action header source-buffer record-type log-keyword)
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
        (set-action-timestamp
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
        (set-action-timestamp
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