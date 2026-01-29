(defpackage :organ
  (:use :cl :lem :lem/transient)
  (:export :*organ-files*))

(in-package :organ)

(defvar *organ-files*
  nil
  "a list of rules according to which to look for and parse files. see `cltpt/roam:find-files'.")

(define-transient *organ-keymap*
  :display-style :row
  :description "keys for organ-mode that can be invoked from outside the mode itself."
  (:keymap
   :display-style :column
   (:keymap
    :display-style :column
    :description "agenda options"
    (:key "d" :suffix test :description "display DONE tasks")
    (:key "r" :suffix test :description "range of dates to be displayed"))
   (:keymap
    :display-style :column
    :description "roam options"
    (:key "f" :suffix test :description "roam files (not yet implemented)" :active-p nil)))
  (:keymap
   :display-style :column
   :description "quick agenda actions"
   (:key "a" :suffix agenda-mode-open :description "open agenda")
   (:key "r" :suffix roam-find :description "find roam node"))
  (:key "c"
   :description "publish (export-all)"
   :suffix (:keymap
            :display-style :column
            (:key "o" :suffix test :description "output dir")
            (:key "i" :suffix test :description "tags to include")
            (:key "x" :suffix test :description "tags to exclude (undoes inclusion)")
            (:key "s" :suffix test :description "static file output dir")
            (:key "S" :suffix test :description "copy static files")
            (:key "h" :suffix test :description "convert all files to html")
            (:key "l" :suffix test :description "convert all files to latex"))))

(lem:define-key lem:*global-keymap* "C-c r" *organ-keymap*)

(defun string-starts-with-p (prefix string)
  (let ((len (length prefix)))
    (and (<= len (length string))
         (string= prefix (subseq string 0 len)))))

(lem:define-command roam-find () ()
  (if *organ-files*
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
        (lem:find-file dest-file))
      (lem:message "you must customize *organ-files* first.")))

(lem:define-command agenda-mode-open () ()
  (if *organ-files*
      (let* ((rmr (cltpt/roam:from-files *organ-files*))
             (agenda (cltpt/agenda:from-roamer rmr)))
        (organ/agenda-mode:agenda-mode-open agenda)
        (lem:message "loaded agenda."))
      (lem:message "you must customize *organ-files* first.")))