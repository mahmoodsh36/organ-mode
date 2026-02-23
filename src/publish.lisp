(defpackage :organ/publish
  (:use :cl :lem :lem/transient)
  (:export *publish-keymap*))

(in-package :organ/publish)

(defvar *output-dir*
  "out")

(defvar *tags-to-include*
  nil)

(defvar *tags-to-exclude*
  nil)

(defvar *static-output-dir*
  "out/static/")

(defvar *filepath-format*
  cltpt/publish:*default-filepath-format*)

(defvar *static-filepath-format*
  cltpt/publish:*default-static-filepath-format*)

(defvar *publish-files*
  nil)

(defvar *static-route*
  cltpt/publish:*default-html-static-route*)

(defvar *theme*
  "gruvbox")

(defvar *files-to-include*
  nil)

(defvar *files-to-exclude*
  nil)

(define-transient *publish-keymap*
  :display-style :column
  (:key "o"
   :type :choice
   :description "output dir"
   :value nil
   :variable '*output-dir*)
  (:key "s"
   :type :choice
   :description "static files output dir"
   :variable '*static-output-dir*)
  (:key "f"
   :type :choice
   :description "org files (or roam rules) destined for publishing."
   :variable '*publish-files*
   :value nil)
  (:key "I"
   :type :choice
   :description "tags to include"
   :variable '*tags-to-include*)
  (:key "X"
   :type :choice
   :description "tags to exclude (undoes inclusion)"
   :variable '*tags-to-exclude*)
  (:key "r"
   :type :choice
   :description "filepath format"
   :variable '*filepath-format*)
  (:key "R"
   :type :choice
   :description "static filepath format"
   :variable '*static-filepath-format*)
  (:key "S"
   :type :choice
   :description "static route"
   :variable '*static-route*)
  (:key "m"
   :type :choice
   :variable '*theme*
   :description "theme")
  (:key "i"
   :type :choice
   :variable '*files-to-include*
   :description "files to include")
  (:key "x"
   :type :choice
   :variable '*files-to-exclude*
   :description "files to exclude")
  (:key "p"
   :description "publish"
   :suffix 'organ-publish))

(defun get-publish-files ()
  "get files for publishing, falling back to organ:*organ-files* if *publish-files* is not set."
  (or *publish-files* organ:*organ-files*))

(defun resolve-theme ()
  "resolve *theme* to a theme directory path."
  (when *theme*
    (cltpt/publish:load-theme-by-name *theme*)))

(defun theme-templates (theme-dir)
  "collect all html template files from THEME-DIR."
  (let ((template-dir (cltpt/file-utils:join-paths theme-dir "template")))
    (loop for path in (uiop:directory-files (cltpt/file-utils:as-dir-path template-dir))
          when (string= (cltpt/file-utils:file-ext path) "html")
            collect (uiop:native-namestring path))))

(defun copy-theme-static-assets (theme-dir static-output-dir)
  "copy static assets from THEME-DIR into STATIC-OUTPUT-DIR."
  (let ((dest-dir-static static-output-dir))
    (cltpt/file-utils:ensure-dir-exists (cltpt/file-utils:as-dir-path dest-dir-static))
    (mapc
     (lambda (item)
       (setf item (uiop:unix-namestring item))
       (uiop:copy-file
        item
        (cltpt/file-utils:join-paths
         dest-dir-static
         (cltpt/file-utils:file-basename item))))
     (uiop:directory-files
      (cltpt/file-utils:as-dir-path (cltpt/file-utils:join-paths theme-dir "static"))))))

(define-command organ-publish () ()
  (let ((files (get-publish-files)))
    (if (null files)
        (lem:message "no files to publish. set publish files (f) or organ:*organ-files* first.")
        (let* ((output-dir (cltpt/file-utils:ensure-absolute *output-dir*))
               (theme-dir (resolve-theme))
               (template-file
                 (when theme-dir
                   (cltpt/file-utils:join-paths theme-dir "template" "page.html")))
               (templates
                 (when theme-dir
                   (theme-templates theme-dir)))
               (spinner (lem/loading-spinner:start-loading-spinner
                         :modeline
                         :buffer (lem:current-buffer)
                         :loading-message "publishing...")))
          (lem:call-background-job
           (lambda ()
             (when theme-dir
               (copy-theme-static-assets theme-dir *static-output-dir*))
             (cltpt/publish:publish
              output-dir
              files
              :include-files *files-to-include*
              :exclude-files *files-to-exclude*
              :static-output-dir *static-output-dir*
              :templates templates
              :template-file template-file
              :theme-dir theme-dir
              :filepath-format *filepath-format*
              :static-filepath-format *static-filepath-format*
              :html-static-route *static-route*))
           (lambda (result)
             (lem/loading-spinner:stop-loading-spinner spinner)
             (lem:message "published to ~A." output-dir)))))))

;; register publish keymap into the global organ keymap.
(lem/transient:define-transient-key
 organ:*organ-keymap*
 "c"
 (list :description "publish (export-all)" :suffix *publish-keymap*))