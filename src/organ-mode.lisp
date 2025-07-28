(defpackage :organ-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools :cltpt)
  (:export :organ-mode))
(in-package :organ-mode)

;; (defun make-tmlanguage-organ ()
;;   (let* ((patterns (make-tm-patterns
;;                     (make-tm-match "^#\\+.*$"
;;                                    :name 'syntax-builtin-attribute))))
;;     (make-tmlanguage :patterns patterns)))

(defvar *organ-syntax-table*
  (let ((table (make-syntax-table)))
    table))

(defvar *organ-mode-hook*
  '((organ-mode-init-all . 0))
  "The list of functions to be called when my-cool-mode is activated.
This is a customizable hook.")

(define-major-mode organ-mode language-mode
  (:name "organ-mode"
   :keymap *organ-mode-keymap*
   :syntax-table *organ-syntax-table*
   :mode-hook *organ-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t))

(defun organ-mode-init-all ()
  (update-tree nil nil nil)
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
       (loop for overlay in (text-object-lem-overlays obj buf))))
    ;; (lem:message "custom syntax highlighting triggered in ~A, size is ~A"
    ;;              (lem:buffer-name buf)
    ;;              (length (lem:buffer-text buf)))
    ))

(defun string-starts-with-p (prefix string)
  (let ((len (length prefix)))
    (and (<= len (length string))
         (string= prefix (subseq string 0 len)))))

(define-command roam-find () ()
  (let* ((rmr (organ-roamer))
         (items
           (mapcar
            (lambda (node)
              (if (cltpt/roam:node-text-obj node)
                  (lem/completion-mode:make-completion-item
                   :label (cltpt/roam:node-title node)
                   :detail (symbol-name (class-name (class-of (cltpt/roam:node-text-obj node)))))
                  (lem/completion-mode:make-completion-item
                   :label (cltpt/roam:node-title node))))
            (cltpt/roam:roamer-nodes rmr))))
    (lem:prompt-for-string
     "roam-find (node) "
     :completion-function
     (lambda (str1)
       (remove-if-not
        (lambda (item)
          (string-starts-with-p str1 (lem/completion-mode:completion-item-label item)))
        items)))))

(defun organ-roamer ()
  (let* ((rmr (cltpt/roam:from-files
               '((:path ("/home/mahmooz/brain/notes/")
                  :regex ".*\\.org"
                  :format "org-mode")))))
    rmr))

(define-command open-agenda () ()
  (organ/agenda::my-show-weekly-agenda))

(defun organ-setup-keys ()
  (define-key *global-keymap* "C-c a" 'open-agenda)
  (define-key *global-keymap* "C-c r" 'roam-find)
  )

(organ-setup-keys)

(define-file-type ("org") organ-mode)