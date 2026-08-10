(in-package :organ/organ-mode)

;; `cltpt/org-mode:org-latex-env' is covered by `cltpt/latex:latex-env', which it subclasses.
(defvar *organ-latex-preview-types*
  '(cltpt/latex:inline-math
    cltpt/latex:display-math
    cltpt/latex:latex-env)
  "text object types that get a latex preview.")

(defvar *organ-latex-preview-foreground*
  nil
  "color for previews. NIL follows the current color theme's foreground,
a \"#rrggbb\" string forces a color, :none leaves them as latex produced them (black).")

(defvar *organ-latex-preview-debounce*
  500
  "milliseconds after the last buffer change before previews are resynced and recompiled.")

(defvar *organ-latex-preview-auto*
  nil
  "when non-nil, `organ-latex-preview-mode' is enabled in every organ-mode buffer.")

;; never drawn, the display layer replaces it with the image. it cannot be empty.
(defvar *organ-latex-preview-placeholder*
  " "
  "virtual-text underlying a preview image.")

(defvar *organ-latex-preview-last-output*
  nil
  "output of the last latex run, kept for debugging failed previews.")

;; not really used, passed to make-overlay because it requires an attribute.
(lem:define-attribute latex-preview-source-attribute)

(defun preview-foreground-color ()
  (cond ((stringp *organ-latex-preview-foreground*) *organ-latex-preview-foreground*)
        ((eq *organ-latex-preview-foreground* :none) nil)
        ;; a theme is free to leave its foreground unset, as lem-default does.
        (t (let ((color (lem:foreground-color)))
             (and (stringp color) color)))))

(defun compile-snippets (snippets color recompile)
  "synchronously compile SNIPPETS in one latex run, returning cltpt's `preview' structs."
  (let ((cltpt/latex-previews:*latex-preview-preamble*
          (if color
              (format nil
                      "~A~%\\usepackage{xcolor}"
                      cltpt/latex-previews:*latex-preview-preamble*)
              cltpt/latex-previews:*latex-preview-preamble*)))
    (cltpt/latex-previews:generate-previews-for-latex
     snippets
     :recompile recompile)))

(defun compile-failure-summary ()
  "a one-line hint at why the last run produced nothing, for the echo area."
  (let ((line (find-if (lambda (line)
                         ;; latex's own errors start with "!". a crashed run instead starts
                         ;; with "error: ..."
                         (and (plusp (length line))
                              (or (char= #\! (char line 0))
                                  (eql 0 (search "error: " line)))))
                       (uiop:split-string (or *organ-latex-preview-last-output* "")
                                          :separator '(#\newline)))))
    (if line
        (format nil "~A (M-x organ-latex-preview-log for the rest)" line)
        "M-x organ-latex-preview-log to see why")))

(defun compile-snippets-async (snippets color recompile callback)
  "compile SNIPPETS in a thread and call CALLBACK with the resulting `preview' structs.
CALLBACK runs on the editor thread."
  (bt2:make-thread
   (lambda ()
     (let* ((output (make-string-output-stream))
            (failure)
            (results
              (handler-case
                  (let ((*standard-output* output)
                        (*error-output* output))
                    (compile-snippets snippets color recompile))
                (error (e)
                  (setf failure (princ-to-string e))
                  nil))))
       (setf *organ-latex-preview-last-output*
             (format nil "~@[error: ~A~%~%~]~A" failure (get-output-stream-string output)))
       (lem:send-event
        (lambda ()
          (funcall callback results)
          (lem:redraw-display)))))
   :name "organ-latex-preview"))

(defstruct preview
  ;; overlay spanning the fragment's source. carries this struct under :latex-preview.
  overlay
  ;; the fragment's latex source as it currently stands in the buffer.
  snippet
  ;; the last compile's `cltpt/latex-previews:preview' (image path, and the typeset size and depth
  ;; it carries), NIL until one lands.
  compiled
  ;; the source `compiled' was made from, which is stale once `snippet' moves away from it.
  ;; the cltpt preview struct already stores the compiled snippet but it might contain extra
  ;; stuff such as the color macro.
  compiled-snippet
  inline-p
  focused-p)

(defun buffer-previews (&optional (buffer (lem:current-buffer)))
  (loop :for overlay :in (lem:buffer-overlays buffer)
        :for preview := (lem:overlay-get overlay :latex-preview)
        :when preview
          :collect preview))

(defun preview-alive-p (preview)
  (let ((overlay (preview-overlay preview)))
    (and overlay (lem-core::overlay-alive-p overlay))))

(defun preview-offset (preview)
  "the 0-based buffer offset PREVIEW's fragment starts at."
  (organ/utils:point-to-char-offset (lem:overlay-start (preview-overlay preview))))

(defun preview-image (preview)
  "the path of PREVIEW's image, or NIL while it has yet to be compiled."
  (let ((compiled (preview-compiled preview)))
    (and compiled (cltpt/latex-previews:preview-path compiled))))

(defun preview-image-ascent (preview)
  "how much of PREVIEW's image sits above the text baseline, as a percentage of its height."
  (let* ((compiled (preview-compiled preview))
         (height (and compiled (cltpt/latex-previews:preview-height compiled)))
         (depth (and compiled (cltpt/latex-previews:preview-depth compiled))))
    (round (* 100 (/ (- height depth) height)))))

(defun editor-em-pixels ()
  "pixels one em of editor text takes, the unit cltpt measures a fragment in.
the fallback is for a pixel frontend that does not report an em: the cell height is the glyph
bounding box rather than the em, so previews come out a little large."
  (or (lem-if:font-em-pixels (lem:implementation))
      (lem-if:cell-height (lem:implementation))))

(defun preview-image-attribute (preview)
  "the attribute carrying PREVIEW's image and the pixel size to draw it at.
one em of latex is drawn as one em of editor text, so a fragment is typeset at the size of the
text around it."
  (let* ((em (editor-em-pixels))
         (compiled (preview-compiled preview))
         (width (and compiled (cltpt/latex-previews:preview-width compiled)))
         (height (and compiled (cltpt/latex-previews:preview-height compiled)))
         (ascent (preview-image-ascent preview)))
    (lem:make-attribute
     :plist (append (list :image (preview-image preview))
                    (when width (list :width (max 1 (round (* em width)))))
                    (when height (list :height (max 1 (round (* em height)))))
                    (when ascent (list :ascent ascent))))))

(defun preview-virtual-runs (preview)
  "the virtual text runs standing in for PREVIEW's image, see `lem-core::virtual-text-runs'.
a focused none-inlined fragment shows its image in an isolated line below the snippet, and is the
one case with row breaks: we insert newlines into the sequence of virtual text to have the
isolated row for the image without modifying the buffer itself. the image for an inlined fragment
is shown on the same row as the origin snippet (to its right), so it takes the row the buffer
already gave it and we dont need newlines."
  (let ((image-run (list *organ-latex-preview-placeholder*
                         (preview-image-attribute preview)))
        (row-break (list (string #\newline) nil)))
    (if (or (preview-inline-p preview)
            ;; when the fragment is unfocused, regardless of whether its inlined or not,
            ;; :after-string should only be the game itself, the text will be invisible.
            (not (preview-focused-p preview)))
        (list image-run)
        (append (list row-break)
                (list image-run)
                (unless (lem:end-line-p (lem:overlay-end (preview-overlay preview)))
                  (list row-break))))))

(defun preview-images-drawable-p ()
  "whether the frontend can draw preview images."
  (lem:image-support-p (lem:implementation)))

(defun render-preview (preview)
  "install the virtual text and visibility PREVIEW's current state calls for."
  (let ((overlay (preview-overlay preview)))
    (when overlay
      (let ((image (preview-image preview)))
        (lem:overlay-put overlay :invisible (and image (not (preview-focused-p preview))))
        (when image
          (lem:overlay-put overlay :after-string (preview-virtual-runs preview)))))))

(defun set-preview-compiled (preview result)
  (setf (preview-compiled preview) result
        (preview-compiled-snippet preview) (preview-snippet preview))
  (render-preview preview))

(defun delete-preview (preview)
  "remove PREVIEW, putting its fragment's source back on screen."
  (let ((overlay (preview-overlay preview)))
    (when overlay
      (setf (preview-overlay preview) nil)
      (lem:delete-overlay overlay))))

(defun latex-object-type ()
  "the type specifier matching every previewable fragment. passed to `typep' down the line."
  (cons 'or *organ-latex-preview-types*))

(defun latex-object-region (obj)
  "the region of OBJ to preview, as (values begin end) 0-based buffer offsets.
this is useful for example in the case of `org-latex-env' which contains more than just latex"
  (let ((base (cltpt/base:text-object-begin-in-root obj)))
    (values (+ base (cltpt/base:text-object-contents-begin obj))
            (+ base (cltpt/base:text-object-contents-end obj)))))

(defun latex-object-at-pos (tree pos)
  "the innermost previewable latex fragment covering POS in TREE."
  (when tree
    (organ/utils:find-node-at-pos tree pos (latex-object-type))))

(defun latex-objects (tree)
  "every previewable latex fragment in TREE, in document order."
  (let ((type (latex-object-type))
        (objects))
    (when tree
      (cltpt/base:map-text-object
       tree
       (lambda (obj)
         (when (typep obj type)
           (push obj objects)))))
    (nreverse objects)))

(defun cursor-inside-overlay-p (overlay)
  "whether a cursor currently sits in OVERLAY."
  (loop :for point :in (lem:buffer-cursors (lem:overlay-buffer overlay))
        :thereis (member overlay (lem:overlays-with-cursor-hooks-covering point))))

(defun preview-at-offset (buffer offset)
  "the preview whose fragment starts at OFFSET (a 0-based buffer offset), or NIL."
  (find offset (buffer-previews buffer) :key #'preview-offset))

(defun place-preview (buffer obj)
  "put a preview on the fragment OBJ in BUFFER, without checking for one already there."
  (multiple-value-bind (begin end) (latex-object-region obj)
    (let* ((overlay (lem:make-overlay
                     (organ/utils:char-offset-to-point buffer begin)
                     (organ/utils:char-offset-to-point buffer end)
                     'latex-preview-source-attribute
                     ;; the point kinds keep the overlay from swallowing text typed against
                     ;; either edge.
                     :start-point-kind :left-inserting
                     :end-point-kind :right-inserting))
           (preview (make-preview
                     :overlay overlay
                     :snippet (cltpt/base:text-object-contents obj)
                     :inline-p (cltpt/base:text-object-property obj :is-inline))))
      (lem:overlay-put overlay :latex-preview preview)
      (lem:overlay-put overlay
                       :cursor-enter-functions (list 'preview-cursor-enter))
      (lem:overlay-put overlay
                       :cursor-leave-functions (list 'preview-cursor-leave))
      (setf (preview-focused-p preview)
            (cursor-inside-overlay-p overlay))
      (ensure-preview-hook buffer)
      preview)))

(defun ensure-preview (buffer obj)
  "create a preview for the fragment OBJ in BUFFER, or return the one already on it."
  (or (preview-at-offset buffer (latex-object-region obj))
      (place-preview buffer obj)))

(defun preview-stale-p (preview)
  "whether PREVIEW's image no longer matches the source it sits on."
  (or (null (preview-image preview))
      (not (equal (preview-compiled-snippet preview)
                  (preview-snippet preview)))))

(defun update-preview-images (previews &key recompile)
  "compile PREVIEWS, in one latex run, and install the results.
cltpt's cache serves anything seen before unless RECOMPILE says otherwise."
  (flet ((colored-snippet (preview color)
           (if color
               (format nil
                       "\\color[HTML]{~A}%~%~A"
                       (string-upcase (string-left-trim "#" color))
                       (preview-snippet preview))
               (preview-snippet preview))))
    (let* ((color (preview-foreground-color))
           (targets (if recompile
                        previews
                        (remove-if-not (lambda (preview)
                                         (preview-stale-p preview))
                                       previews)))
           (snippets (remove-duplicates
                      (mapcar (lambda (preview) (colored-snippet preview color)) targets)
                      :test #'string=)))
      (when snippets
        (compile-snippets-async
         snippets
         color
         recompile
         (lambda (results)
           (dolist (preview targets)
             (let ((result (find (colored-snippet preview color)
                                 results
                                 :key #'cltpt/latex-previews:preview-snippet
                                 :test #'string=)))
               (when (and result
                          (cltpt/latex-previews:preview-path result)
                          (preview-alive-p preview))
                 (set-preview-compiled preview result))))
           (when (notany #'cltpt/latex-previews:preview-path results)
             (lem:message "latex preview: ~D snippet~:P produced no image. ~A"
                          (length snippets)
                          (compile-failure-summary)))))))))

(defun place-buffer-previews (buffer &key recompile)
  (let ((previews (mapcar (lambda (obj) (ensure-preview buffer obj))
                          (latex-objects (lem:buffer-value buffer 'cltpt-tree)))))
    (update-preview-images previews :recompile recompile)
    previews))

(defun preview-cursor-enter (point overlay direction)
  (let ((preview (lem:overlay-get overlay :latex-preview)))
    (when (and preview (not (preview-focused-p preview)))
      (setf (preview-focused-p preview) t)
      (render-preview preview))))

(defun preview-cursor-leave (point overlay direction)
  (let ((preview (lem:overlay-get overlay :latex-preview)))
    (when (and preview (preview-focused-p preview))
      (setf (preview-focused-p preview) nil)
      (render-preview preview))))

(defun move-preview-overlay (overlay begin end)
  "move OVERLAY onto [BEGIN, END), 0-based buffer offsets."
  (let ((start (lem:overlay-start overlay))
        (finish (lem:overlay-end overlay)))
    (and (lem:move-to-position start (1+ begin))
         (lem:move-to-position finish (1+ end)))))

(defun sync-preview (preview tree)
  "line PREVIEW back up with TREE after an edit.
returns `:gone' when its fragment is gone (and deletes it), `:changed' when the source changed,
else `:ok'."
  (let* ((overlay (preview-overlay preview))
         (obj (latex-object-at-pos tree (preview-offset preview))))
    (if (null obj)
        (progn
          (delete-preview preview)
          :gone)
        (progn
          (multiple-value-bind (begin end) (latex-object-region obj)
            (move-preview-overlay overlay begin end))
          (setf (preview-inline-p preview)
                (cltpt/base:text-object-property obj :is-inline))
          (render-preview preview)
          (let ((snippet (cltpt/base:text-object-contents obj)))
            (if (string= snippet (preview-snippet preview))
                :ok
                (progn
                  (setf (preview-snippet preview) snippet)
                  :changed)))))))

(defun preview-index (buffer)
  "BUFFER's previews, keyed by the offset their fragment starts at.
duplicates are deleted on the way: two converge when an edit merges their fragments into one."
  (let ((index (make-hash-table :test 'eql)))
    (dolist (preview (buffer-previews buffer) index)
      (let ((offset (preview-offset preview)))
        (if (gethash offset index)
            (delete-preview preview)
            (setf (gethash offset index) preview))))))

(defun sync-buffer-previews (buffer)
  "resync every preview in BUFFER against its tree and recompile the ones whose source changed."
  (let ((tree (lem:buffer-value buffer 'cltpt-tree))
        (stale))
    (when tree
      (dolist (preview (buffer-previews buffer))
        (when (eq :changed (sync-preview preview tree))
          (push preview stale)))
      (let ((index (preview-index buffer)))
        (dolist (obj (latex-objects tree))
          (unless (gethash (latex-object-region obj) index)
            (push (place-preview buffer obj) stale))))
      (when stale
        (update-preview-images stale)))))

(defun buffer-quiet-ms (buffer)
  "milliseconds since BUFFER last changed, or the full delay when no change was ever recorded."
  (let ((last (lem:buffer-value buffer 'latex-preview-last-change)))
    (if last
        (round (* 1000 (- (get-internal-real-time) last))
               internal-time-units-per-second)
        *organ-latex-preview-debounce*)))

(defun schedule-preview-sync (buffer &optional (delay *organ-latex-preview-debounce*))
  "arrange for BUFFER's previews to be resynced once the current burst of edits settles.
edits themselves only record when they happened. a timer that goes off to find the buffer changed
in the meantime sleeps again for the rest of the quiet period, instead of being cancelled and
replaced on every keystroke."
  (unless (lem:buffer-value buffer 'latex-preview-timer)
    (setf (lem:buffer-value buffer 'latex-preview-timer)
          (lem:start-timer
           (lem:make-timer
            (lambda ()
              (setf (lem:buffer-value buffer 'latex-preview-timer) nil)
              (when (member buffer (lem:buffer-list))
                (let ((remaining (- *organ-latex-preview-debounce* (buffer-quiet-ms buffer))))
                  (if (plusp remaining)
                      (schedule-preview-sync buffer remaining)
                      (sync-buffer-previews buffer)))))
            :name "organ-latex-preview-debounce")
           delay
           :repeat nil))))

(defun preview-after-change (start end old-len)
  (let ((buffer (lem:point-buffer start)))
    (when (or (lem:mode-active-p buffer 'organ-latex-preview-mode)
              (buffer-previews buffer))
      (setf (lem:buffer-value buffer 'latex-preview-last-change)
            (get-internal-real-time))
      (schedule-preview-sync buffer))))

(defun ensure-preview-hook (buffer)
  "make sure BUFFER reports its changes to the preview machinery."
  (unless (lem:buffer-value buffer 'latex-preview-hook)
    (setf (lem:buffer-value buffer 'latex-preview-hook) t)
    (lem:add-hook (lem:variable-value 'lem:after-change-functions :buffer buffer)
                  'preview-after-change
                  -1)))

(defun release-preview-hook (buffer)
  (setf (lem:buffer-value buffer 'latex-preview-hook) nil)
  (lem:remove-hook (lem:variable-value 'lem:after-change-functions :buffer buffer)
                   'preview-after-change))

(lem:define-command organ-latex-preview-regenerate () ()
  "recompile the previews in the current buffer, ignores and modifies the preview cache."
  (if (preview-images-drawable-p)
      (let ((previews (buffer-previews)))
        (if previews
            (update-preview-images previews :recompile t)
            (place-buffer-previews (lem:current-buffer) :recompile t)))
      (lem:message "latex previews need a frontend that draws images")))

(lem:define-command organ-latex-preview-log () ()
  (show-text-buffer
   "*organ-latex-preview-log*"
   (or *organ-latex-preview-last-output*
       "no latex run has happened yet in this session.")))

(lem:define-minor-mode organ-latex-preview-mode
    (:name "latex-preview"
     :description "shows latex previews and keeps them in sync across edits."
     :enable-hook 'latex-preview-mode-enable
     :disable-hook 'latex-preview-mode-disable))

(defun latex-preview-mode-enable ()
  (if (preview-images-drawable-p)
      (let ((buffer (lem:current-buffer)))
        (ensure-preview-hook buffer)
        (place-buffer-previews buffer))
      (progn
        (lem:disable-minor-mode 'organ-latex-preview-mode)
        (lem:message "latex previews need a frontend that draws images"))))

(defun latex-preview-mode-disable ()
  (let ((buffer (lem:current-buffer)))
    (mapc #'delete-preview (buffer-previews buffer))
    (release-preview-hook buffer)))

(defun latex-preview-init ()
  "turn previews on in a new organ-mode buffer when `*organ-latex-preview-auto*' says to."
  (when (and *organ-latex-preview-auto*
             (preview-images-drawable-p))
    (organ-latex-preview-mode t)))

;; runs after `organ-mode-init-all', which is what parses the buffer we are about to preview.
(lem:add-hook *organ-mode-hook* 'latex-preview-init -1)