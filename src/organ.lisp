(defpackage :organ
  (:use :cl :lem :lem/transient)
  (:export :*organ-files* :*agenda-timestamp-range*))

(in-package :organ)

(defvar *organ-files*
  nil
  "a list of rules according to which to look for and parse files. see `cltpt/roam:find-files'.")

(defvar *agenda-timestamp-range*
  nil
  "a `cltpt/agenda:time-range' constraining the agenda view, or nil for the default range.")

;; custom infix type for prompting a timestamp range via two date inputs.
(defclass lem/transient::timestamp-range (lem/transient::infix)
  ())

(defmethod lem:prefix-suffix ((prefix lem/transient::timestamp-range))
  (lambda ()
    (lem:with-last-read-key-sequence
        (let ((begin-ts (organ/popup-calendar:popup-calendar-with-callback
                         "begin date: "
                         nil)))
          (when begin-ts
            (let ((end-ts (organ/popup-calendar:popup-calendar-with-callback
                           "end date (optional): "
                           nil
                           begin-ts)))
              (setf (lem/transient:prefix-value prefix)
                    (cltpt/agenda:make-time-range
                     :begin begin-ts
                     :end end-ts))))))))

(defmethod lem/transient::prefix-render ((prefix lem/transient::timestamp-range)
                                         &optional matched-depth)
  (let* ((key-str (lem/transient::prefix-effective-display-key prefix))
         (range (lem/transient::prefix-value prefix))
         (desc (lem/transient::get-description prefix))
         (value-str (cond
                      ((null range) "none")
                      ((cltpt/agenda:time-range-end range)
                       (format nil "~A -- ~A"
                               (organ/utils:format-timestamp
                                (cltpt/agenda:time-range-begin range))
                               (organ/utils:format-timestamp
                                (cltpt/agenda:time-range-end range))))
                      (t (format nil "~A --"
                                 (organ/utils:format-timestamp
                                  (cltpt/agenda:time-range-begin range)))))))
    (lem/transient::make-layout-item
     :key (lem/transient::make-key-with-highlight key-str matched-depth)
     :description (list (cons desc nil)
                        (cons " " nil)
                        (cons "[" 'lem/transient::transient-bracket-attribute)
                        (cons value-str 'lem/transient::transient-value-attribute)
                        (cons "]" 'lem/transient::transient-bracket-attribute)))))

(define-transient *organ-keymap*
  :display-style :row
  :description "keys for organ-mode that can be invoked from outside the mode itself."
  (:keymap
   :display-style :row
   (:keymap
    :display-style :column
    (:keymap
     :description "agenda actions"
     (:key "a" :suffix agenda-open :description "open agenda"))
    (:keymap
     :description "agenda options"
     (:key "d"
      :type toggle
      :description "display DONE tasks"
      :variable cltpt:*agenda-include-done*)
     (:key "R"
      :type timestamp-range
      :description "timestamp range"
      :variable *agenda-timestamp-range*)))
   (:keymap
    :display-style :column
    (:keymap
     :description "roam actions"
     (:key "r" :suffix roam-find :description "browse nodes")
     (:key "l" :suffix test :description "list nodes"))
    (:keymap
     :description "roam options"
     (:key "f" :suffix test :description "roam files (not yet implemented)" :active-p nil))))
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

(lem:define-command agenda-open () ()
  (if *organ-files*
      (let* ((rmr (cltpt/roam:from-files *organ-files*))
             (agenda (cltpt/agenda:from-roamer rmr))
             (range *agenda-timestamp-range*))
        (organ/agenda-mode:agenda-mode-open
         agenda
         :begin-ts (when range (cltpt/agenda:time-range-begin range))
         :end-ts (when range (cltpt/agenda:time-range-end range)))
        (lem:message "loaded agenda."))
      (lem:message "you must customize *organ-files* first.")))
