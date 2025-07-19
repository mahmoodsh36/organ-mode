(defpackage :organ-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools :cltpt)
  (:export :organ-mode))
(in-package :organ-mode)

(defun make-tmlanguage-organ ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-match "^#\\+.*$"
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *organ-syntax-table*
  (let ((table (make-syntax-table))
        (tmlanguage (make-tmlanguage-organ)))
    (set-syntax-parser table tmlanguage)
    table))

(defvar *organ-mode-hook*
  '((organ-mode-init-all . 0))
  "The list of functions to be called when my-cool-mode is activated.
This is a customizable hook.")

(define-major-mode organ-mode language-mode
  (:name "Organ"
   :keymap *organ-mode-keymap*
   :syntax-table *organ-syntax-table*
   :mode-hook *organ-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t))

(defun organ-mode-init-all ()
  ;; (update-tree nil nil nil)
  (let ((buf (lem:current-buffer)))
    (lem:add-hook
     (lem:variable-value
      'lem:after-change-functions
      :buffer buf)
     'update-tree)))

(defun highlight-region (start-point end-point color-string)
  (lem:make-overlay start-point
                    end-point
                    (lem:make-attribute :background color-string)))

(defun update-tree (start-point end-point length)
  (format t "hey~%")
  (let* ((buf (lem:current-buffer))
         (buffer-contents (lem:buffer-text buf))
         (cltpt-tree (cltpt/base:parse buffer-contents
                                       (cltpt/org-mode:org-mode-text-object-types)
                                       :doc-type 'cltpt/org-mode::org-document)))
    (setf (buffer-value buf 'cltpt-tree) cltpt-tree)
    (lem:clear-overlays buf)
    (cltpt/base:map-text-object
     cltpt-tree
     (lambda (obj)
       (loop for overlay in (text-object-lem-overlays obj buf)
             do (format t "hey~%"))))
    (lem:message "custom syntax highlighting triggered in ~A, size is ~A"
                 (lem:buffer-name buf)
                 (length (lem:buffer-text buf)))))

(define-file-type ("org") organ-mode)