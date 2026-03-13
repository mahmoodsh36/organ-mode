(defpackage :organ
  (:use :cl :lem :lem/transient)
  (:export :*organ-files* :*agenda-timestamp-range* :*organ-keymap*))

(in-package :organ)

(defvar *organ-files*
  nil
  "a list of rules according to which to look for and parse files. see `cltpt/roam:find-files'.")

(defvar *agenda-timestamp-range*
  nil
  "a `cltpt/agenda:time-range' constraining the agenda view, or nil for the default range.")

(defvar *agenda-include-done*
  nil
  "when non-nil, include tasks in a terminal (done) state in the agenda view.")

(defvar *agenda-first-repeat-only*
  nil
  "when non-nil, show only the first occurrence of each repeating task in the agenda view.")

;; custom infix type for prompting a timestamp range via two date inputs.
(defclass lem/transient::timestamp-range (lem/transient::infix)
  ())

(defmethod lem:prefix-suffix ((prefix lem/transient::timestamp-range))
  (lambda ()
    (lem:with-last-read-key-sequence
        (let ((begin-ts (organ/popup-calendar:popup-calendar-prompt "begin date: ")))
          (when begin-ts
            (let ((end-ts (organ/popup-calendar:popup-calendar-prompt
                           "end date (optional): "
                           begin-ts)))
              (setf (lem/transient:prefix-value prefix)
                    (cltpt/agenda:make-time-range
                     :begin begin-ts
                     :end end-ts))))))))

(defmethod lem/transient:prefix-render ((prefix lem/transient::timestamp-range)
                                        &optional matched-depth)
  (let* ((key-str (lem/transient::prefix-effective-display-key prefix))
         (range (lem/transient:prefix-value prefix))
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
    (lem/transient:make-layout-item
     :key (lem/transient:make-key-with-highlight key-str matched-depth)
     :description (list (cons desc nil)
                        (cons " " nil)
                        (cons "[" 'lem/transient:transient-bracket-attribute)
                        (cons value-str 'lem/transient:transient-value-attribute)
                        (cons "]" 'lem/transient:transient-bracket-attribute)))))

(define-transient *organ-keymap*
  :display-style :row
  :description "keys for organ-mode that can be invoked from outside the mode itself."
  (:keymap
   :display-style :row
   (:keymap
    :display-style :column
    (:keymap
     :description "agenda actions"
     (:key "a" :suffix 'agenda-open :description "open agenda"))
    (:keymap
     :description "agenda options"
     (:key "d"
      :type 'toggle
      :description "display DONE tasks"
      :variable '*agenda-include-done*)
     (:key "u"
      :type 'toggle
      :description "show only first repeat"
      :variable '*agenda-first-repeat-only*)
     (:key "R"
      :type 'timestamp-range
      :description "timestamp range"
      :variable '*agenda-timestamp-range*)))
   (:keymap
    :display-style :column
    (:keymap
     :description "roam actions"
     (:key "r" :suffix 'roam-find :description "browse nodes")
     (:key "l" :suffix 'test :description "list nodes"))
    (:keymap
     :description "roam options"
     (:key "f" :suffix 'test :description "roam files (not yet implemented)" :active-p nil)))))

(lem:define-key lem:*global-keymap* "C-c r" *organ-keymap*)

(lem:define-command roam-find () ()
  (if *organ-files*
      (let* ((rmr (cltpt/roam:roamer-from-files *organ-files*))
             (titled-nodes
               (remove-if-not #'cltpt/roam:node-title (cltpt/roam:roamer-nodes rmr)))
             (type-width
               (loop for node in titled-nodes
                     when (cltpt/roam:node-text-obj node)
                       maximize (length (symbol-name
                                         (class-name
                                          (class-of
                                           (cltpt/roam:node-text-obj node)))))))
             (items
               (loop for node in titled-nodes
                     collect (lem/completion-mode:make-completion-item
                              :label (cltpt/roam:node-title node)
                              :detail (format nil "~v@<~@[~A~]~>  ~A"
                                              type-width
                                              (when (cltpt/roam:node-text-obj node)
                                                (symbol-name
                                                 (class-name
                                                  (class-of
                                                   (cltpt/roam:node-text-obj node)))))
                                              (file-namestring (cltpt/roam:node-file node))))))
             (choice-str
               (lem:prompt-for-string
                "roam-find (node) "
                :completion-function (lambda (x)
                                       (lem:completion-strings
                                        x
                                        items
                                        :key #'lem/completion-mode:completion-item-label))))
             ;; this is problematic because it doesnt work well with duplicates
             (choice-idx (position choice-str
                                   items
                                   :key #'lem/completion-mode:completion-item-label
                                   :test #'string=))
             (choice (elt titled-nodes choice-idx))
             (dest-file (cltpt/roam:node-file choice))
             (text-obj (cltpt/roam:node-text-obj choice)))
        (let ((buffer (lem:find-file-buffer dest-file)))
          (lem:switch-to-buffer buffer)
          (when text-obj
            (lem:move-to-position (lem:current-point)
                                  (1+ (cltpt/base:text-object-begin-in-root text-obj))))))
      (lem:message "you must customize *organ-files* first.")))

(lem:define-command agenda-open () ()
  (if *organ-files*
      (let* ((rmr (cltpt/roam:roamer-from-files *organ-files*))
             (agenda (cltpt/agenda:from-roamer rmr))
             (range *agenda-timestamp-range*))
        (organ/agenda-mode:agenda-mode-open
         agenda
         :begin-ts (when range (cltpt/agenda:time-range-begin range))
         :end-ts (when range (cltpt/agenda:time-range-end range))
         :include-done *agenda-include-done*
         :first-repeat-only *agenda-first-repeat-only*)
        (lem:message "loaded agenda."))
      (lem:message "you must customize *organ-files* first.")))