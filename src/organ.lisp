(defpackage :organ
  (:use :cl :lem :cltpt)
  (:export :*organ-files*))

(in-package :organ)

(defvar *organ-files*
  nil
  "a list of rules according to which to look for and parse files. see `cltpt/roam:find-files'.")

(defun string-starts-with-p (prefix string)
  (let ((len (length prefix)))
    (and (<= len (length string))
         (string= prefix (subseq string 0 len)))))

(define-command roam-find () ()
  (let* ((rmr (cltpt/roam:from-files *organ-files*))
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
          (string-starts-with-p
           str1
           (lem/completion-mode:completion-item-label item)))
        items)))))

(define-command organ-agenda-open () ()
  "opens the organ-agenda buffer."
  (if *organ-files*
      (progn
        (let* ((rmr (cltpt/roam:from-files *organ-files*))
               (agenda (cltpt/agenda:from-roamer rmr))
               (agenda-forest (cltpt/agenda:build-agenda-forest agenda)))
          (organ/outline-mode:open-outline agenda-forest)
          (lem:message "loading agenda.")))
      (lem:message "you must customize *organ-files* first.")))

(defun organ-setup-keys ()
  (define-key *global-keymap* "C-c a" 'organ-agenda-open)
  (define-key *global-keymap* "C-c r" 'roam-find)
  )

(organ-setup-keys)