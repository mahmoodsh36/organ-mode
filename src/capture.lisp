(defpackage :organ/capture
  (:use :cl)
  (:export
   :unique-timestamp
   :*organ-capture-templates*))

(in-package :organ/capture)

(defun unique-timestamp (&optional dirpath)
  "return a unique timestamp for the default capture template."
  (let ((timestamp (local-time:timestamp-to-unix (local-time:now))))
    (if dirpath
        (loop
          (if (probe-file (merge-pathnames (format nil "~A.org" timestamp)
                                           dirpath))
              (incf timestamp)
              (return timestamp)))
        timestamp)))

(defvar *organ-capture-templates*
  '((:key #\d
     :name "default"
     :dir "notes/"
     :if-new "#+title:
#+date: %(organ/utils:format-timestamp)"
     :entry "
* TODO %organ/capture::cursor"
     :filename "todos.org")
    (:key #\n
     :name "new note"
     :dir "notes/"
     :if-new "#+title:
#+date: %(organ/utils:format-timestamp)
#+filetags: :note:
#+identifier: %(organ/capture:unique-timestamp \"notes/\")"
     :filename "%(organ/capture:unique-timestamp \"notes/\").org")))

(defun capture-file (template)
  (let* ((filename-format (getf template :filename))
         (filename (cltpt:convert-simple-format filename-format))
         (dest-filepath (cltpt:join-paths (getf template :dir) filename))
         (buffer (lem:find-file-buffer dest-filepath)))
    (lem:switch-to-buffer buffer)
    (unless (probe-file dest-filepath)
      (lem:insert-string (lem:buffer-start-point buffer)
                         (cltpt:convert-simple-format (getf template :if-new))))
    (let ((p (lem:current-point)))
      (when (getf template :entry)
        (lem:buffer-end p)
        (let* ((start-pos (organ/utils:point-to-char-offset (lem:copy-point p :temporary)))
               (entry (cltpt/base:bind-and-eval*
                       '((cursor ""))
                       (lambda ()
                         (cltpt:convert-simple-format (getf template :entry)))
                       :organ/capture))
               (parse-result (cltpt:parse cltpt/base:*simple-format* (getf template :entry)))
               (cursor-pos (loop for obj in (cltpt/base:list-children-recursively parse-result)
                                 when (and (typep obj 'cltpt/base:post-lexer-text-macro)
                                           ;; this wouldnt work because we havent bound cursor
                                           ;; (eq (cltpt/base:eval-post-lexer-macro obj)
                                           ;;     'cursor)
                                           )
                                   return (cltpt:text-object-begin-in-root obj))))
          (lem:insert-string p entry)
          (lem:move-point p (organ/utils:char-offset-to-point
                             buffer
                             (+ start-pos cursor-pos))))))))

(lem/transient:define-transient *organ-capture-keymap*
  :display-style :row
  :prefixes-func (loop for template in *organ-capture-templates*
                       collect (lem:make-prefix
                                :key (car (lem:parse-keyspec (string (getf template :key))))
                                ;; we have to use the local current-template binding in the lambda
                                :suffix (let ((current-template template))
                                          (lambda ()
                                            (capture-file current-template))))))

(lem/transient:define-prefix *capture-prefix*
  :key "n"
  :suffix *organ-capture-keymap*
  :description "+capture prefix")

;; this doesnt really work on re-evaluations because im using defparameter
;; in define-prefix/define-transient. maybe i shouldnt be.
(unless (member *capture-prefix* (lem:keymap-prefixes organ:*organ-keymap*))
  (lem:keymap-add-prefix organ:*organ-keymap* *capture-prefix*))