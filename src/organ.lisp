(defpackage :organ
  (:use :cl)
  (:export :*organ-files*))

(in-package :organ)

(defvar *organ-files*
  nil
  "a list of rules according to which to look for and parse files. see `cltpt/roam:find-files'.")

(defun string-starts-with-p (prefix string)
  (let ((len (length prefix)))
    (and (<= len (length string))
         (string= prefix (subseq string 0 len)))))

(lem:define-command roam-find () ()
  (let* ((rmr (cltpt/roam:from-files *organ-files*))
         (items
           (mapcar
            (lambda (node)
              (if (cltpt/roam:node-text-obj node)
                  (lem/completion-mode:make-completion-item
                   :label (cltpt/roam:node-title node)
                   :detail (symbol-name
                            (class-name
                             (class-of
                              (cltpt/roam:node-text-obj node)))))
                  (lem/completion-mode:make-completion-item
                   :label (cltpt/roam:node-title node))))
            (cltpt/roam:roamer-nodes rmr)))
         (choice-str (lem:prompt-for-string
                      "roam-find (node) "
                      :completion-function
                      (lambda (str1)
                        (remove-if-not
                         (lambda (item)
                           (string-starts-with-p
                            str1
                            (lem/completion-mode:completion-item-label item)))
                         items))))
         ;; this is problematic because it doesnt work well with duplicates
         (choice-idx (position choice-str
                               items
                               :key #'lem/completion-mode:completion-item-label
                               :test #'string=))
         (choice (elt (cltpt/roam:roamer-nodes rmr) choice-idx))
         (dest-file (cltpt/roam:node-file choice)))
    (lem:find-file dest-file)))

(lem:define-command agenda-mode-open () ()
  (if *organ-files*
      (let* ((rmr (cltpt/roam:from-files *organ-files*))
             (agenda (cltpt/agenda:from-roamer rmr)))
        (organ/agenda-mode:agenda-mode-open agenda)
        (lem:message "loaded agenda."))
      (lem:message "you must customize *organ-files* first.")))

(defun organ-setup-keys ()
  (lem:define-key lem:*global-keymap* "C-c a" 'agenda-mode-open)
  (lem:define-key lem:*global-keymap* "C-c r" 'roam-find)
  )

(organ-setup-keys)