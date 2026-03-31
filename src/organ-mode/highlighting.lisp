(in-package :organ/organ-mode)

(lem:define-attribute organ-header-stars-attribute
  (t :foreground :base03))

(lem:define-attribute organ-header-title-attribute
  (t :foreground :base07 :bold t))

(lem:define-attribute organ-header-todo-attribute
  (t :foreground :base0e))

(lem:define-attribute organ-timestamp-attribute
  (t :foreground :base09))

(lem:define-attribute organ-block-attribute
  (t :foreground :base03))

(lem:define-attribute organ-list-bullet-attribute
  (t :foreground :base09))

(lem:define-attribute organ-checkbox-unchecked-attribute
  (t :foreground :base03))

(lem:define-attribute organ-checkbox-checked-attribute
  (t :foreground :base0b :bold t))

(lem:define-attribute organ-checkbox-partial-attribute
  (t :foreground :base0a))

(lem:define-attribute organ-table-delimiter-attribute
  (t :foreground :base03))

(lem:define-attribute organ-keyword-attribute
  (t :foreground :base0d))

(lem:define-attribute organ-latex-env-attribute
  (t :foreground :base0a))

(lem:define-attribute organ-block-keyword-attribute
  (t :foreground :base08))

(lem:define-attribute organ-default-attribute
  (t :foreground :base05))

(lem:define-attribute organ-link-attribute
  (t :foreground :base0d))

(lem:define-attribute organ-underline-attribute
  (t :underline t))

(lem:define-attribute organ-strike-through-attribute
  (t :foreground :base03))

;; should return a list of attributes for lem for "syntax highlighting" in a buffer.
;; technically could be used for more than just syntax highlighting.
(defgeneric text-object-overlays (text-obj buf))

(defun begin-in-root (obj)
  "alternative to `cltpt/base:text-object-begin-in-root' that makes use of :parent-pos property set by `organ-redraw-buffer'."
  (+ (cltpt/base:text-object-property obj :parent-pos)
     (cltpt/base:text-object-begin obj)))

(defun end-in-root (obj)
  "alternative to `cltpt/base:text-object-end-in-root' that makes use of :parent-pos property set by `organ-redraw-buffer'."
  (+ (cltpt/base:text-object-property obj :parent-pos)
     (cltpt/base:text-object-end obj)))

;; this takes an object, finds a cltpt/combinator "submatch", and returns an
;; overlay with the given attribute
(defun overlay-for-submatch (buf obj submatch-id attribute)
  "a function DRYing highilighting of submatches."
  (let* ((match (cltpt/base:text-object-match obj))
         (submatch (cltpt/combinator:find-submatch match submatch-id)))
    (when submatch
      (lem:make-overlay (organ/utils:char-offset-to-point
                         buf
                         (cltpt/combinator:match-begin-absolute submatch))
                        (organ/utils:char-offset-to-point
                         buf
                         (cltpt/combinator:match-end-absolute submatch))
                        attribute))))

;; like `overlay-for-submatch', except that it acts on all submatches found by id
(defun overlays-for-submatches (buf obj submatch-id attribute)
  "a function DRYing highilighting of submatches."
  (let* ((match (cltpt/base:text-object-match obj))
         (submatches (cltpt/combinator:find-submatch-all match submatch-id)))
    (loop for submatch in submatches
          for submatch-begin = (cltpt/combinator:match-begin-absolute submatch)
          for submatch-end = (cltpt/combinator:match-end-absolute submatch)
          collect (lem:make-overlay
                   (organ/utils:char-offset-to-point buf submatch-begin)
                   (organ/utils:char-offset-to-point buf submatch-end)
                   attribute))))

;; TODO: most of the time we shouldnt have to provide delimiter-len explicitly. it can be
;; extracted from the rules
(defun overlays-for-delimiter-pair (buf obj attribute delimiter-len)
  "highlight DELIMITER-LEN chars at the start and end of OBJ."
  (let ((begin (begin-in-root obj))
        (end (end-in-root obj)))
    (list
     (lem:make-overlay (organ/utils:char-offset-to-point buf begin)
                       (organ/utils:char-offset-to-point buf (+ begin delimiter-len))
                       attribute)
     (lem:make-overlay (organ/utils:char-offset-to-point buf (- end delimiter-len))
                       (organ/utils:char-offset-to-point buf end)
                       attribute))))

(defun overlays-for-block-keywords (buf obj)
  "highlight :keyword names in a block's begin line."
  (let* ((match (cltpt/base:text-object-match obj))
         (kw-submatches (cltpt/combinator:find-submatch-all
                         match 'cltpt/org-mode::keyword)))
    (loop for kw in kw-submatches
          for kw-begin = (1- (cltpt/combinator:match-begin-absolute kw))
          for kw-end = (cltpt/combinator:match-end-absolute kw)
          collect (lem:make-overlay
                   (organ/utils:char-offset-to-point buf kw-begin)
                   (organ/utils:char-offset-to-point buf kw-end)
                   'organ-block-keyword-attribute))))

;; default: highlight with buffer's foreground color
(defmethod text-object-overlays ((obj cltpt/base:text-object) buf)
  (unless (typep obj 'cltpt/base::document)
    (let ((begin (begin-in-root obj))
          (end (end-in-root obj)))
      (list
       (lem:make-overlay (organ/utils:char-offset-to-point buf begin)
                         (organ/utils:char-offset-to-point buf end)
                         'organ-default-attribute)))))

;; consult cltpt/org-mode:*org-header-rule*
(defmethod text-object-overlays ((obj cltpt/org-mode:org-header) buf)
  (remove-if-not
   #'identity
   (list
    (overlay-for-submatch buf obj 'cltpt/org-mode::title 'organ-header-title-attribute)
    (overlay-for-submatch buf obj 'cltpt/org-mode::stars 'organ-header-stars-attribute)
    (overlays-for-submatches buf
                             obj
                             'cltpt/org-mode::timestamp
                             'organ-timestamp-attribute)
    (overlays-for-submatches buf
                             obj
                             'cltpt/org-mode::todo-timestamp
                             'organ-timestamp-attribute)
    (overlay-for-submatch buf
                          obj
                          'cltpt/org-mode::todo-keyword
                          'organ-header-todo-attribute))))

;; consult cltpt/org-mode:*org-block-rule*
;; highlight only #+begin_<type> and #+end_<type>, plus :keyword names
(defmethod text-object-overlays ((obj cltpt/org-mode:org-block) buf)
  (let* ((match (cltpt/base:text-object-match obj))
         (begin-match (cltpt/combinator:find-submatch match 'cltpt/org-mode::begin))
         (begin-type-match (cltpt/combinator:find-submatch match 'cltpt/org-mode::begin-type))
         (end-match (cltpt/combinator:find-submatch-last match 'cltpt/org-mode::end))
         (end-type-match (cltpt/combinator:find-submatch-last match 'cltpt/org-mode::end-type)))
    (append
     (remove-if-not
      #'identity
      (list
       (when (and begin-match begin-type-match)
         (lem:make-overlay
          (organ/utils:char-offset-to-point
           buf
           (cltpt/combinator:match-begin-absolute begin-match))
          (organ/utils:char-offset-to-point
           buf
           (cltpt/combinator:match-end-absolute begin-type-match))
          'organ-block-attribute))
       (when (and end-match end-type-match)
         (lem:make-overlay
          (organ/utils:char-offset-to-point
           buf
           (cltpt/combinator:match-begin-absolute end-match))
          (organ/utils:char-offset-to-point
           buf
           (cltpt/combinator:match-end-absolute end-type-match))
          'organ-block-attribute))))
     (overlays-for-block-keywords buf obj))))

;; highlight :PROPERTIES:, :END:, and :key: names in property drawers
(defmethod text-object-overlays ((obj cltpt/org-mode:org-prop-drawer) buf)
  (let ((end (end-in-root obj)))
    (append
     (remove-if-not
      #'identity
      (list
       (overlay-for-submatch buf obj 'cltpt/org-mode::drawer-open-tag 'organ-block-attribute)
       ;; :end: has no submatch id, it's always the last 5 chars of the object
       (lem:make-overlay (organ/utils:char-offset-to-point buf (- end (length ":END:")))
                         (organ/utils:char-offset-to-point buf end)
                         'organ-block-attribute)))
     ;; highlight :key: names in drawer entries
     (overlays-for-submatches buf
                              obj
                              'cltpt/org-mode::drawer-key
                              'organ-block-keyword-attribute))))

;; highlight :name: and :END: in generic drawers
(defmethod text-object-overlays ((obj cltpt/org-mode:org-drawer) buf)
  (remove-if-not
   #'identity
   (list
    (overlay-for-submatch buf obj 'cltpt/org-mode::drawer-open-tag 'organ-block-attribute)
    (overlay-for-submatch buf obj 'cltpt/org-mode::drawer-close-tag 'organ-block-attribute))))

(defmethod text-object-overlays ((obj cltpt/org-mode:org-list) buf)
  (append
   (overlays-for-submatches buf
                            obj
                            'cltpt/org-mode::list-item-bullet
                            'organ-list-bullet-attribute)
   (let ((checkboxes (cltpt/combinator:find-submatch-all
                      (cltpt/base:text-object-match obj)
                      'cltpt/org-mode::list-item-checkbox)))
     (loop for cb in checkboxes
           for state = (getf (cltpt/combinator:match-props cb) :state)
           for attr = (ecase state
                        (:unchecked 'organ-checkbox-unchecked-attribute)
                        (:checked 'organ-checkbox-checked-attribute)
                        (:partial 'organ-checkbox-partial-attribute))
           collect (lem:make-overlay
                    (organ/utils:char-offset-to-point
                     buf
                     (cltpt/combinator:match-begin-absolute cb))
                    (organ/utils:char-offset-to-point
                     buf
                     (cltpt/combinator:match-end-absolute cb))
                    attr)))))

;; highlight vertical delimiters (pipes) and horizontal rules (separators) in tables
(defmethod text-object-overlays ((obj cltpt/org-mode:org-table) buf)
  (append
   (overlays-for-submatches buf
                            obj
                            'cltpt/org-mode::table-cell-delimiter
                            'organ-table-delimiter-attribute)
   (overlays-for-submatches buf
                            obj
                            'cltpt/org-mode::table-hrule
                            'organ-table-delimiter-attribute)))

(defmethod text-object-overlays ((obj cltpt/org-mode:org-link) buf)
  (list (lem:make-overlay (organ/utils:char-offset-to-point buf (begin-in-root obj))
                          (organ/utils:char-offset-to-point buf (end-in-root obj))
                          'organ-link-attribute)))

(defmethod text-object-overlays ((obj cltpt/org-mode:org-underline) buf)
  (list (lem:make-overlay (organ/utils:char-offset-to-point buf (begin-in-root obj))
                          (organ/utils:char-offset-to-point buf (end-in-root obj))
                          'organ-underline-attribute)))

(defmethod text-object-overlays ((obj cltpt/org-mode:org-strike-through) buf)
  (list (lem:make-overlay (organ/utils:char-offset-to-point buf (begin-in-root obj))
                          (organ/utils:char-offset-to-point buf (end-in-root obj))
                          'organ-strike-through-attribute)))


;; highlight \begin{...} and \end{...} tags in latex environments
(defmethod text-object-overlays ((obj cltpt/latex:latex-env) buf)
  (remove-if-not
   #'identity
   (list
    (overlay-for-submatch buf obj 'cltpt/latex::open-tag 'organ-latex-env-attribute)
    (overlay-for-submatch buf obj 'cltpt/latex::close-tag 'organ-latex-env-attribute))))

;; highlight #+keyword: part
(defmethod text-object-overlays ((obj cltpt/org-mode:org-keyword) buf)
  (let* ((match (cltpt/base:text-object-match obj))
         (kw-submatch (cltpt/combinator:find-submatch match 'cltpt/org-mode::keyword)))
    (when kw-submatch
      (let ((kw-begin (- (cltpt/combinator:match-begin-absolute kw-submatch) 2))
            (kw-end (1+ (cltpt/combinator:match-end-absolute kw-submatch))))
        (list
         (lem:make-overlay (organ/utils:char-offset-to-point buf kw-begin)
                           (organ/utils:char-offset-to-point buf kw-end)
                           'organ-keyword-attribute))))))

;; highlight \( \) and \[ \] delimiters in inline/display math
(defmethod text-object-overlays ((obj cltpt/latex:inline-math) buf)
  (overlays-for-delimiter-pair buf obj 'organ-latex-env-attribute 2))

(defmethod text-object-overlays ((obj cltpt/latex:display-math) buf)
  (overlays-for-delimiter-pair buf obj 'organ-latex-env-attribute 2))

;; highlight #+begin_export and #+end_export tags
(defmethod text-object-overlays ((obj cltpt/org-mode:org-export-block) buf)
  (remove-if-not
   #'identity
   (list
    (overlay-for-submatch buf obj 'cltpt/org-mode::open-tag 'organ-block-attribute)
    (overlay-for-submatch buf obj 'cltpt/org-mode::end 'organ-block-attribute))))

(defun overlays-for-results (buf match)
  "highlight #+RESULTS header and ': ' prefixes on output lines."
  (let ((results-match (cltpt/combinator:find-submatch match 'cltpt/org-mode::results)))
    (when results-match
      (let* ((results-begin (cltpt/combinator:match-begin-absolute results-match))
             (results-content (cltpt/combinator:find-submatch
                               results-match
                               'cltpt/org-mode::results-content))
             (header-end (when results-content
                           (1- (cltpt/combinator:match-begin-absolute results-content)))))
        (append
         (when header-end
           (list (lem:make-overlay
                  (organ/utils:char-offset-to-point buf results-begin)
                  (organ/utils:char-offset-to-point buf header-end)
                  'organ-block-attribute)))
         (let ((output-lines (cltpt/combinator:find-submatch-all
                              match
                              'cltpt/org-mode::output-line)))
           (loop for line in output-lines
                 for line-begin = (cltpt/combinator:match-begin-absolute line)
                 collect (lem:make-overlay
                          (organ/utils:char-offset-to-point buf line-begin)
                          (organ/utils:char-offset-to-point buf (+ line-begin 2))
                          'organ-block-attribute))))))))

;; highlight #+begin_src/#+end_src, :keyword, #+RESULTS, and ": " prefix in results
(defmethod text-object-overlays ((obj cltpt/org-mode:org-src-block) buf)
  (let ((match (cltpt/base:text-object-match obj)))
    (append
     (remove-if-not
      #'identity
      (list
       (overlay-for-submatch buf obj 'cltpt/org-mode::open-tag 'organ-block-attribute)
       (overlay-for-submatch buf obj 'cltpt/org-mode::end 'organ-block-attribute)))
     (overlays-for-block-keywords buf obj)
     (overlays-for-results buf match))))

;; mapping from org src block language names to lem mode names
(defvar *language-mode-alist*
  '(("elisp" . "Emacs Lisp")
    ("emacs-lisp" . "Emacs Lisp")
    ("common-lisp" . "Lisp")
    ("lisp" . "Lisp")
    ("cl" . "Lisp")
    ("js" . "JavaScript")
    ("javascript" . "JavaScript")
    ("python" . "Python")
    ("sh" . "posix-shell")
    ("bash" . "posix-shell")
    ("shell" . "posix-shell")
    ("c" . "C")
    ("html" . "HTML")
    ("css" . "CSS")
    ("json" . "JSON")
    ("java" . "Java")))

(defun find-mode-for-language (lang-name)
  "find a lem major mode for a src block language name."
  (when lang-name
    (let* ((mapped (cdr (assoc lang-name *language-mode-alist* :test #'string-equal)))
           (mode-name (or mapped lang-name)))
      (lem:find-mode mode-name))))

(defun apply-src-block-syntax-highlighting (obj buf)
  "apply language-specific syntax highlighting to the contents of a src block."
  (let* ((match (cltpt/base:text-object-match obj))
         (lang-submatch (cltpt/combinator:find-submatch match 'cltpt/org-mode::lang)))
    (when lang-submatch
      (let* ((lang-name (cltpt/base:text-object-match-text obj lang-submatch))
             (mode (find-mode-for-language lang-name)))
        (when mode
          (let* ((syntax-table (lem:mode-syntax-table mode))
                 (contents-begin (+ (begin-in-root obj)
                                    (cltpt/base:text-object-contents-begin obj)))
                 (contents-end (+ (begin-in-root obj)
                                  (cltpt/base:text-object-contents-end obj)))
                 ;; apply 1+ to contents-begin because the contents of the block begin
                 ;; at the end of the line with the opening tag and we want the next line instead.
                 (start-point (organ/utils:char-offset-to-point buf (1+ contents-begin)))
                 (end-point (organ/utils:char-offset-to-point buf contents-end)))
            (when syntax-table
              (lem:set-region-major-mode start-point end-point mode)
              (lem:syntax-scan-region start-point
                                      end-point
                                      :syntax-table syntax-table
                                      :recursive-check nil))))))))

(defun organ-redraw-buffer (buf)
  (lem:clear-overlays buf)
  ;; clear previous syntax highlighting done by syntax-scan-region
  (let ((start (lem:buffer-start-point buf))
        (end (lem:buffer-end-point buf)))
    (lem:remove-text-property start end :attribute)
    (lem:clear-region-major-mode start end))
  (cltpt/base:map-text-object-with-pos-in-root
   (lem:buffer-value buf 'cltpt-tree)
   (lambda (obj parent-pos)
     (setf (cltpt/base:text-object-property obj :parent-pos) parent-pos)
     (text-object-overlays obj buf)
     ;; apply language-specific syntax highlighting for src blocks
     (when (typep obj 'cltpt/org-mode:org-src-block)
       (apply-src-block-syntax-highlighting obj buf)))))