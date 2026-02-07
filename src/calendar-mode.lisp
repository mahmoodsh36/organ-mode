(defpackage :organ/calendar-mode
  (:use :cl :lem)
  (:export
   :calendar
   :calendar-with-callback
   :update-calendar-display
   :move-to-date
   :refresh-calendar-highlights
   :calendar-cleanup
   :calendar-grid-width
   :calendar-grid-height
   :calendar-marked-dates
   :calendar-date
   :popup-calendar-with-callback))

(in-package :organ/calendar-mode)

(define-editor-variable calendar-date nil)
(define-editor-variable today-point nil)
(define-editor-variable source-buffer nil)
(define-editor-variable calendar-callback nil)
(define-editor-variable calendar-marked-dates nil)

(define-editor-variable calendar-grid-width 3)
(define-editor-variable calendar-grid-height 3)

;; note: this isnt really the iso format
(defparameter *iso-date-format* '((:year 4) "-" (:month 2) "-" (:day 2)))
(defparameter *month-header-format* '(:long-month " " :year))
(defparameter *weekday-header* "  Su  Mo  Tu  We  Th  Fr  Sa")
(defparameter *calendar-column-width* 28)
(defparameter *calendar-day-cell-width* 4)
(defparameter *days-in-week* 7)
(defparameter *date-search-range* 10)
(defparameter *cross-month-search-lines* 8)
(defparameter *calendar-date-display-format* '(:long-month " " (:day 2) ", " (:year 4)))
(defparameter *day-line-regex* "^\\s *[0-9]+")

(defvar *calendar-strict-grid-navigation*
  t
  "if true, navigation jumps to next/previous month when no date exists in target column.
if nil, navigation finds nearest date in same month first.")

(define-major-mode calendar-mode nil
  (:name "calendar"
   :keymap *calendar-mode-keymap*))

(define-attribute current-day
    (t :foreground :base00 :background :base0D))

(define-attribute marked-day
    (t :foreground :base00 :background :base0B))

(define-attribute today
    (t :foreground :base00 :background :base0A))

(defstruct calendar-segment
  text
  ts)

(defun encode-date (day month year)
  "encode a timestamp at midnight for the given day, month, and year."
  (local-time:encode-timestamp 0 0 0 0 day month year))

(defun first-of-month (month year)
  "encode a timestamp for the first day of the given month and year."
  (encode-date 1 month year))

(defun same-date-p (date1 date2)
  "check if two timestamps represent the same calendar day."
  (and (= (local-time:timestamp-year date1) (local-time:timestamp-year date2))
       (= (local-time:timestamp-month date1) (local-time:timestamp-month date2))
       (= (local-time:timestamp-day date1) (local-time:timestamp-day date2))))

(defun today-p (date)
  "check if DATE is today."
  (same-date-p date (local-time:now)))

(defun format-calendar-date (date
                              &optional (format *iso-date-format*))
  (local-time:format-timestring nil date :format format))

(defun calendar-current-day-overlay (buffer)
  (buffer-value buffer 'calendar-current-day-overlay))

(defun (setf calendar-current-day-overlay) (overlay buffer)
  (setf (buffer-value buffer 'calendar-current-day-overlay) overlay))

(defun calendar-marked-dates-overlays (buffer)
  (buffer-value buffer 'calendar-marked-dates-overlays))

(defun (setf calendar-marked-dates-overlays) (overlays buffer)
  (setf (buffer-value buffer 'calendar-marked-dates-overlays) overlays))

(defun calendar-today-overlay (buffer)
  (buffer-value buffer 'calendar-today-overlay))

(defun (setf calendar-today-overlay) (overlay buffer)
  (setf (buffer-value buffer 'calendar-today-overlay) overlay))

(defun make-day-overlay-at (point attribute)
  "create a 4-character overlay at POINT with ATTRIBUTE if it has a :calendar-day property."
  (when (text-property-at point :calendar-day)
    (with-point ((end-pos (copy-point point :temporary)))
      (character-offset end-pos *calendar-day-cell-width*)
      (make-overlay point end-pos attribute))))

(defun clear-current-day-overlay (&optional (buffer (current-buffer)))
  (when (calendar-current-day-overlay buffer)
    (delete-overlay (calendar-current-day-overlay buffer))
    (setf (calendar-current-day-overlay buffer) nil)))

(defun clear-marked-dates-overlays (&optional (buffer (current-buffer)))
  (dolist (overlay (calendar-marked-dates-overlays buffer))
    (delete-overlay overlay))
  (setf (calendar-marked-dates-overlays buffer) nil))

(defun clear-today-overlay (&optional (buffer (current-buffer)))
  (when (calendar-today-overlay buffer)
    (delete-overlay (calendar-today-overlay buffer))
    (setf (calendar-today-overlay buffer) nil)))

(defun calendar-cleanup (buffer)
  "clean up calendar-specific state for a buffer."
  (clear-current-day-overlay buffer)
  (clear-marked-dates-overlays buffer)
  (clear-today-overlay buffer))

(defun render-month-to-lines (timestamp)
  (let* ((now (local-time:now))
         (year (local-time:timestamp-year timestamp))
         (month (local-time:timestamp-month timestamp))
         (first-day-of-month (first-of-month month year))
         (days-in-month (local-time:days-in-month month year))
         (padding-count (mod (local-time:timestamp-day-of-week first-day-of-month) 7)))
    (let* ((grid-cells
             (append
              (loop repeat padding-count
                    collect (make-calendar-segment
                             :text (make-string *calendar-day-cell-width*
                                                :initial-element #\space)))
              (loop for day from 1 to days-in-month
                    for ts = (encode-date day month year)
                    collect (make-calendar-segment
                             :text (if (same-date-p ts now)
                                       (format nil "[~2d]" day)
                                       (format nil " ~2d " day))
                             :ts ts))))
           (weeks (loop while grid-cells
                        collect (let ((week (subseq
                                             grid-cells
                                             0
                                             (min *days-in-week* (length grid-cells)))))
                                  (setf grid-cells (nthcdr *days-in-week* grid-cells))
                                  week))))
      (coerce
       (cons
        (list (make-calendar-segment
               :text (local-time:format-timestring
                      nil
                      first-day-of-month
                      :format *month-header-format*)))
        (cons
         (list (make-calendar-segment :text *weekday-header*))
         weeks))
       'list))))

(defun get-date-under-cursor (&optional (point (current-point)))
  "get the full date timestamp at POINT. falls back to constructing from :calendar-day."
  (or (text-property-at point :calendar-date)
      (let ((day (text-property-at point :calendar-day)))
        (when day
          (let ((buffer (current-buffer)))
            (encode-date
             day
             (local-time:timestamp-month
              (variable-value 'calendar-date :buffer buffer))
             (local-time:timestamp-year
              (variable-value 'calendar-date :buffer buffer))))))))

(defun get-month-index-in-grid (month
                                year
                                &optional (calendar-date
                                           (variable-value
                                            'calendar-date
                                            :buffer (current-buffer))))
  "get the index of a month/year in the current grid display."
  (let* ((buffer (current-buffer))
         (start-month (local-time:timestamp-month calendar-date))
         (start-year (local-time:timestamp-year calendar-date))
         (num-months (* (variable-value 'calendar-grid-width :buffer buffer)
                        (variable-value 'calendar-grid-height :buffer buffer))))
    (loop for i from 0 below num-months
          for ts = (local-time:timestamp+ (first-of-month start-month start-year)
                                          i
                                          :month)
          when (and (= (local-time:timestamp-month ts) month)
                    (= (local-time:timestamp-year ts) year))
            return i)))

(defun month-visible-in-grid (month
                              year
                               &optional (calendar-date
                                          (variable-value 'calendar-date
                                                          :buffer (current-buffer))))
  "check if the specified month/year is visible in the current calendar grid."
  (not (null (get-month-index-in-grid month year calendar-date))))

(defun find-day-position (buffer day target-month target-year)
  "find the position of a specific day in the buffer for the specific month/year."
  (with-point ((point (buffer-point buffer)))
    (buffer-start point)
    (loop
      (let ((date-ts (text-property-at point :calendar-date)))
        (when (and date-ts
                   (= (local-time:timestamp-day date-ts) day)
                   (= (local-time:timestamp-month date-ts) target-month)
                   (= (local-time:timestamp-year date-ts) target-year))
          (return-from find-day-position (copy-point point :temporary))))
      (unless (character-offset point 1)
        (return-from find-day-position nil)))))

(defun find-day-position-by-date (buffer date)
  "find the position of DATE in BUFFER."
  (when date
    (find-day-position buffer
                       (local-time:timestamp-day date)
                       (local-time:timestamp-month date)
                       (local-time:timestamp-year date))))

(defun find-date-in-line (line-point target-col)
  "find the date in the given line that is closest to the target column."
  (with-point ((search-point (copy-point line-point :temporary)))
    (line-start search-point)
    (let ((best-point)
          (best-distance most-positive-fixnum))
      (loop while (character-offset search-point 1)
            do (when (text-property-at search-point :calendar-day)
                 (let* ((col (point-column search-point))
                        (distance (abs (- col target-col))))
                   (when (< distance best-distance)
                     (setf best-distance distance)
                     (setf best-point (copy-point search-point :temporary))))))
      best-point)))

(defun find-date-near-column (point)
  "find a date near POINT in the current line."
  (loop for offset from 0 to *date-search-range*
        do (let ((pos (copy-point point :temporary)))
             (when (and (character-offset pos offset)
                        (text-property-at pos :calendar-day))
               (return-from find-date-near-column pos))))
  (loop for offset from 1 to *date-search-range*
        do (let ((pos (copy-point point :temporary)))
             (when (and (character-offset pos (- offset))
                        (text-property-at pos :calendar-day))
               (return-from find-date-near-column pos)))))

(defun highlight-current-day ()
  "highlight the current day under the cursor."
  (let ((buffer (current-buffer)))
    (clear-current-day-overlay buffer)
    (let ((current-date (get-date-under-cursor)))
      (when current-date
        (let ((date-pos (find-day-position-by-date buffer current-date)))
          (when date-pos
            (let ((overlay (make-day-overlay-at date-pos 'current-day)))
              (when overlay
                (setf (calendar-current-day-overlay buffer) overlay)))))))))

(defun highlight-marked-dates ()
  "highlight all marked dates with overlays."
  (let ((buffer (current-buffer)))
    (clear-marked-dates-overlays buffer)
    (dolist (date (variable-value 'calendar-marked-dates :buffer buffer))
      (let ((date-pos (find-day-position-by-date buffer date)))
        (when date-pos
          (let ((overlay (make-day-overlay-at date-pos 'marked-day)))
            (when overlay
              (push overlay (calendar-marked-dates-overlays buffer)))))))))

(defun highlight-today-date ()
  "highlight today's date with overlay."
  (let* ((buffer (current-buffer))
         (today-point (variable-value 'today-point :buffer buffer)))
    (clear-today-overlay buffer)
    (when today-point
      (let ((overlay (make-day-overlay-at today-point 'today)))
        (when overlay
          (setf (calendar-today-overlay buffer) overlay))))))

(defun refresh-calendar-highlights ()
  "refresh all calendar highlighting in the correct order."
  (highlight-current-day)
  (highlight-marked-dates)
  (highlight-today-date))

(defun find-cross-month-target (current-point direction)
  "find cross-month target in DIRECTION (:up or :down), by scanning lines and picking the best candidate."
  (let* ((current-col (point-column current-point))
         (line-step (if (eq direction :up) -1 1))
         (current-date (get-date-under-cursor))
         (best-candidate)
         (best-distance most-positive-fixnum))
    (unless current-date (return-from find-cross-month-target nil))
    (with-point ((search-point (copy-point current-point :temporary)))
      (loop repeat *cross-month-search-lines*
            while (line-offset search-point line-step)
            do (when (looking-at search-point *day-line-regex*)
                 (let ((candidate-point (find-date-in-line search-point current-col)))
                   (when candidate-point
                     (let ((found-date (get-date-under-cursor candidate-point)))
                       (when (and found-date
                                  (not (and (= (local-time:timestamp-month found-date)
                                               (local-time:timestamp-month current-date))
                                            (= (local-time:timestamp-year found-date)
                                               (local-time:timestamp-year current-date)))))
                         (let ((distance (abs (- (point-column candidate-point) current-col))))
                           (when (< distance best-distance)
                             (setf best-distance distance)
                             (setf best-candidate found-date))
                           (when (zerop distance)
                             (return-from find-cross-month-target best-candidate)))))))))
      best-candidate)))

(defun try-intra-month-navigation (direction)
  "try to navigate within the current month, return T if successful."
  (let* ((buffer (current-buffer))
         (current-col (point-charpos (current-point)))
         (line-step (if (eq direction :up) -1 1)))
    (with-point ((p (current-point)))
      (loop
        (unless (line-offset p line-step)
          (return-from try-intra-month-navigation nil))
        (let ((line-len (length (line-string p))))
          (cond
            ((and (> line-len 0) (looking-at p *day-line-regex*))
             (character-offset p (min current-col (1- line-len)))
             (cond
               ((text-property-at p :calendar-day)
                (move-point (current-point) p)
                (highlight-current-day)
                (return-from try-intra-month-navigation t))
               (*calendar-strict-grid-navigation*
                (return-from try-intra-month-navigation nil))
               (t
                (let ((found-point (find-date-near-column p)))
                  (when found-point
                    (move-point (current-point) found-point)
                    (highlight-current-day))
                  (return-from try-intra-month-navigation
                    (not (null found-point)))))))))))))

(defun navigate-vertically (direction)
  "navigate up/down. tries intra-month navigation first, then cross-month."
  (unless (try-intra-month-navigation direction)
    (let ((target-date (find-cross-month-target (current-point) direction)))
      (when target-date
        (move-to-date target-date)
        (highlight-current-day)))))

(defun search-next-date-in-seq (start-point direction boundary-point)
  "search for the next date property starting from START-POINT towards BOUNDARY-POINT.

DIRECTION: :right (forward) or :left (backward).
returns the point if found, nil otherwise."
  (let ((p (copy-point start-point :temporary))
        (step (if (eq direction :right) 1 -1))
        (boundary-fn (if (eq direction :right) #'point< #'point>)))
    (loop while (funcall boundary-fn p boundary-point)
          do (when (text-property-at p :calendar-day)
               (return p))
             (unless (character-offset p step)
               (return nil)))))

(defun navigate-grid-horizontally (direction)
  "navigate horizontally in the calendar grid. DIRECTION can be :left or :right."
  (let* ((buffer (current-buffer))
         (step (if (eq direction :right) 1 -1))
         (line-boundary-fn (if (eq direction :right) #'line-end #'line-start))
         (buffer-boundary (if (eq direction :right) (buffer-end-point buffer) (buffer-start-point buffer))))
    ;; search remaining part of current line
    (with-point ((p (current-point))
                 (line-bound (current-point)))
      (funcall line-boundary-fn line-bound)
      ;; verify we aren't already at line bound
      (when (if (eq direction :right) (point< p line-bound) (point> p line-bound))
        ;; skip current date chars first
        (let ((current-day (text-property-at p :calendar-day)))
          (loop while (and (if (eq direction :right) (point< p line-bound) (point> p line-bound))
                           (equal (text-property-at p :calendar-day) current-day))
                do (character-offset p step)))
        ;; search rest of line
        (let ((found (search-next-date-in-seq p direction line-bound)))
          (when found
            (move-point (current-point) found)
            (highlight-current-day)
            (return-from navigate-grid-horizontally)))))
    ;; search subsequent lines (if not strict mode)
    (unless *calendar-strict-grid-navigation*
      (with-point ((p (current-point)))
        (loop
          (unless (line-offset p step)
            (return))
          ;; for new line, scanning depends on direction
          ;; if right: scan from start of new line to end
          ;; if left: scan from end of new line to start
          (if (eq direction :right)
              (line-start p)
              (line-end p))
          (let ((line-bound (copy-point p :temporary)))
            (if (eq direction :right) (line-end line-bound) (line-start line-bound))
            (let ((found (search-next-date-in-seq p direction line-bound)))
              (when found
                (move-point (current-point) found)
                (highlight-current-day)
                (return-from navigate-grid-horizontally)))))))))

(defun move-to-calendar-day (day target-month target-year)
  "move the buffer's cursor to a specific day in the calendar."
  (let ((day-pos (find-day-position (current-buffer) day target-month target-year)))
    (when day-pos
      (move-point (current-point) day-pos))))

(defun move-to-date (target-date &key (scroll-style :jump))
  "move cursor to the specified date, updating calendar if needed.

SCROLL-STYLE can be :jump (default) or :scroll."
  (let* ((buffer (current-buffer))
         (calendar-date (variable-value 'calendar-date :buffer buffer))
         (target-day (local-time:timestamp-day target-date))
         (target-month (local-time:timestamp-month target-date))
         (target-year (local-time:timestamp-year target-date)))
    (unless (month-visible-in-grid target-month target-year calendar-date)
      (cond
        ((eq scroll-style :scroll)
         (if (local-time:timestamp< target-date calendar-date)
             (setf (variable-value 'calendar-date :buffer buffer)
                   (local-time:timestamp- calendar-date 1 :month))
             (setf (variable-value 'calendar-date :buffer buffer)
                   (local-time:timestamp+ calendar-date 1 :month))))
        (t
         (setf (variable-value 'calendar-date :buffer buffer)
               (first-of-month target-month target-year))))
      (update-calendar-display buffer))
    (move-to-calendar-day target-day target-month target-year)
    (refresh-calendar-highlights)))

(defun update-calendar-display (buffer)
  (let* ((calendar-date (variable-value 'calendar-date :buffer buffer))
         (grid-width (variable-value 'calendar-grid-width :buffer buffer))
         (grid-height (variable-value 'calendar-grid-height :buffer buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (setf (variable-value 'today-point :buffer buffer) nil)
      (let* ((point (buffer-point buffer))
             (timestamp (or calendar-date (local-time:now)))
             (now (local-time:now))
             (num-months (* grid-width grid-height))
             (calendars (loop for i from 0 below num-months
                              collect (render-month-to-lines
                                       (local-time:timestamp+ timestamp
                                                              i
                                                              :month)))))
        (dotimes (row-index grid-height)
          (let ((row-calendars
                  (loop for col-index from 0 below grid-width
                        for cal-index = (+ (* row-index grid-width)
                                           col-index)
                        when (< cal-index num-months)
                          collect (nth cal-index calendars))))
            (when row-calendars
              (let ((max-lines (loop for cal in row-calendars
                                     maximize (length cal))))
                (dotimes (line-num max-lines)
                  (dolist (cal row-calendars)
                    (let ((line-segments (nth line-num cal)))
                      (if line-segments
                          (let ((line-width 0))
                            (dolist (segment line-segments)
                              (let ((start (copy-point point :temporary)))
                                (incf line-width
                                      (lem/common/character:string-width
                                       (calendar-segment-text segment)))
                                (insert-string point
                                               (calendar-segment-text segment))
                                (when (calendar-segment-ts segment)
                                  (let ((date-ts (calendar-segment-ts segment)))
                                    (put-text-property
                                     start point
                                     :calendar-day (local-time:timestamp-day
                                                    date-ts))
                                    (put-text-property
                                     start point
                                     :calendar-date date-ts)
                                    (when (same-date-p date-ts now)
                                      (setf (variable-value 'today-point
                                                            :buffer buffer)
                                            (copy-point start :temporary)))))))
                            (dotimes (i (- *calendar-column-width* line-width))
                              (insert-string point " ")))
                          (insert-string
                           point
                           (make-string *calendar-column-width*
                                        :initial-element #\space))))
                    (insert-string point "  "))
                  (insert-string point (string #\newline)))
                (insert-string point (string #\newline))))))))))

(defun date-marked-p (date)
  "check if a date is marked."
  (when date
    (member date
            (variable-value 'calendar-marked-dates :buffer (current-buffer))
            :test #'same-date-p)))

(defun toggle-date-mark ()
  "toggle mark on the date under cursor."
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor)))
    (when current-date
      (if (date-marked-p current-date)
          (progn
            (setf (variable-value 'calendar-marked-dates :buffer buffer)
                  (remove current-date
                          (variable-value 'calendar-marked-dates
                                          :buffer buffer)
                          :test #'same-date-p))
            (message "unmarked date: ~A" (format-calendar-date current-date)))
          (progn
            (push current-date
                  (variable-value 'calendar-marked-dates
                                  :buffer buffer))
            (message "marked date: ~A" (format-calendar-date current-date))))
      (refresh-calendar-highlights))))

(defun clear-all-marks ()
  "clear all marked dates."
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor)))
    (setf calendar-marked-dates nil)
    (clear-marked-dates-overlays buffer)
    (update-calendar-display buffer)
    (when current-date
      (move-to-date current-date))
    (refresh-calendar-highlights)
    (message "cleared all marks")))

(defun navigate-to-date-or-first-visible (target-date)
  "move to target date, or first visible date if target is not in grid."
  (let ((buffer (current-buffer)))
    (if (month-visible-in-grid (local-time:timestamp-month target-date)
                               (local-time:timestamp-year target-date)
                               (variable-value 'calendar-date :buffer buffer))
        (move-to-date target-date)
        (let ((first-visible-date (variable-value 'calendar-date :buffer buffer)))
          (move-to-calendar-day
           1
           (local-time:timestamp-month first-visible-date)
           (local-time:timestamp-year first-visible-date))))))

(defun navigate-calendar (offset unit)
  "navigate calendar by OFFSET units of UNIT (:month or :year)."
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor)))
    (setf (variable-value 'calendar-date :buffer buffer)
          (local-time:timestamp+ (variable-value 'calendar-date :buffer buffer) offset unit))
    (update-calendar-display buffer)
    (when current-date
      (navigate-to-date-or-first-visible current-date))
    (refresh-calendar-highlights)))

(defun adjust-grid-dimension (var delta &optional (min-value 1))
  "adjust a grid dimension variable by DELTA, clamping at MIN-VALUE."
  (let* ((buffer (current-buffer))
         (new-value (+ (variable-value var :buffer buffer) delta)))
    (when (>= new-value min-value)
      (setf (variable-value var :buffer buffer) new-value)
      (update-calendar-display buffer)
      (refresh-calendar-highlights))))

(define-command calendar () ()
  (calendar-with-callback nil))

(defun calendar-with-callback (callback &optional (buffer-name "*calendar*") initial-date)
  "open a calendar view with a callback function.

CALLBACK is a function that receives the selected date as argument.
INITIAL-DATE can be a timestamp or nil for today."
  (let ((source-buffer (current-buffer)))
    (let ((buffer (make-buffer buffer-name)))
      (switch-to-buffer buffer)
      (calendar-mode)
      ;; initialize buffer-local state
      (setf (calendar-current-day-overlay buffer) nil)
      (setf (calendar-marked-dates-overlays buffer) nil)
      (setf (calendar-today-overlay buffer) nil)
      (setf (variable-value 'calendar-grid-width :buffer buffer) 3)
      (setf (variable-value 'calendar-grid-height :buffer buffer) 3)
      ;; add cleanup hook
      (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
                #'calendar-cleanup)
      ;; set calendar state
      (setf (variable-value 'calendar-date :buffer buffer) (or initial-date (local-time:now)))
      (setf (variable-value 'source-buffer :buffer buffer) source-buffer)
      (setf (variable-value 'calendar-callback :buffer buffer) callback)
      (setf (variable-value 'calendar-marked-dates :buffer buffer) nil)
      (clear-marked-dates-overlays buffer)
      (clear-today-overlay buffer)
      (update-calendar-display buffer)
      (calendar-goto-today)
      (move-to-date (variable-value 'calendar-date :buffer buffer))
      (refresh-calendar-highlights))))

(define-command quit-calendar () ()
  "quit the calendar view."
  (let ((buffer (current-buffer)))
    (calendar-cleanup buffer)
    (kill-buffer buffer)))

(define-command calendar-increase-width () ()
  (adjust-grid-dimension 'calendar-grid-width 1))

(define-command calendar-decrease-width () ()
  (adjust-grid-dimension 'calendar-grid-width -1))

(define-command calendar-increase-height () ()
  (adjust-grid-dimension 'calendar-grid-height 1))

(define-command calendar-decrease-height () ()
  (adjust-grid-dimension 'calendar-grid-height -1))

(defun calendar-at-date (initial-date)
  "open a calendar view at a specific date. INITIAL-DATE should be a local-time timestamp."
  (calendar-with-callback nil "*calendar*" initial-date))

(define-command open-calendar-next-week () ()
  "open a calendar view starting at next week."
  (calendar-at-date (local-time:timestamp+ (local-time:now) *days-in-week* :day)))

(define-command calendar-next-month () ()
  (navigate-calendar 1 :month))

(define-command calendar-previous-month () ()
  (navigate-calendar -1 :month))

(define-command calendar-next-year () ()
  (navigate-calendar 1 :year))

(define-command calendar-previous-year () ()
  (navigate-calendar -1 :year))

(define-command calendar-goto-today () ()
  (move-to-date (local-time:now)))

(define-command calendar-next-day () ()
  "move to the next sequential calendar day."
  (let ((current-date (get-date-under-cursor)))
    (when current-date
      (move-to-date (local-time:timestamp+ current-date 1 :day)
                    :scroll-style :scroll))))

(define-command calendar-previous-day () ()
  "move to the previous sequential calendar day."
  (let ((current-date (get-date-under-cursor)))
    (when current-date
      (move-to-date (local-time:timestamp- current-date 1 :day)
                    :scroll-style :scroll))))

(define-command calendar-goto-date () ()
  "prompt for a specific date and go to that date."
  (let* ((buffer (current-buffer))
         (day (parse-integer
               (prompt-for-string "day: " :initial-value "1")))
         (month (parse-integer
                 (prompt-for-string "month (1-12): " :initial-value "1")))
         (year (parse-integer
                (prompt-for-string
                 "year: "
                 :initial-value (write-to-string
                                 (local-time:timestamp-year
                                  (local-time:now))))))
         (new-date (encode-date day month year)))
    (setf (variable-value 'calendar-date :buffer buffer) new-date)
    (update-calendar-display buffer)
    (move-to-calendar-day day month year)
    (refresh-calendar-highlights)))

(define-command calendar-show-date () ()
  "display the date under the cursor."
  (let ((current-date (get-date-under-cursor)))
    (if current-date
        (message (local-time:format-timestring
                  nil
                  current-date
                  :format *calendar-date-display-format*))
        (message "no date at cursor position"))))

(define-command calendar-select-and-exit () ()
  "select the date under the cursor and call the callback, then close calendar."
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor))
         (marked-dates (variable-value 'calendar-marked-dates :buffer buffer))
         (callback (variable-value 'calendar-callback :buffer buffer))
         (source-buffer (variable-value 'source-buffer :buffer buffer)))
    (if callback
        (progn
          (if marked-dates
              (funcall callback marked-dates)
              (when current-date
                (funcall callback (list current-date))))
          (kill-buffer buffer))
        (progn
          ;; this is a dummy callback that inserts the dates in the source-buffer (where the
          ;; calendar was initially invoked from)
          (switch-to-buffer source-buffer)
          (let ((date-format *iso-date-format*))
            (if marked-dates
                (dolist (date (sort marked-dates #'local-time:timestamp<))
                  (insert-string
                   (current-point)
                   (format nil "~A~%"
                           (local-time:format-timestring
                            nil date :format date-format))))
                (when current-date
                  (insert-string
                   (current-point)
                   (local-time:format-timestring
                    nil current-date :format date-format)))))
          (kill-buffer buffer)))))

(define-command calendar-grid-right () ()
  "move to the next date position in the visual grid."
  (navigate-grid-horizontally :right))

(define-command calendar-grid-left () ()
  "move to the previous date position in the visual grid."
  (navigate-grid-horizontally :left))

(define-command calendar-up-day () ()
  "move up to the same column position in the previous line."
  (navigate-vertically :up))

(define-command calendar-down-day () ()
  "move down to the same column position in the next line."
  (navigate-vertically :down))

(define-command calendar-toggle-mark () ()
  "toggle mark on the date under cursor."
  (toggle-date-mark))

(define-command calendar-clear-marks () ()
  "clear all marked dates."
  (clear-all-marks))

(define-key *calendar-mode-keymap* "q" 'quit-calendar)
(define-key *calendar-mode-keymap* "n" 'calendar-next-month)
(define-key *calendar-mode-keymap* "p" 'calendar-previous-month)
(define-key *calendar-mode-keymap* "M->" 'calendar-next-year)
(define-key *calendar-mode-keymap* "M-<" 'calendar-previous-year)
(define-key *calendar-mode-keymap* "g" 'calendar-goto-today)
(define-key *calendar-mode-keymap* "Return" 'calendar-select-and-exit)
(define-key *calendar-mode-keymap* "d" 'calendar-goto-date)
;; keybindings for visual grid navigation
(define-key *calendar-mode-keymap* "l" 'calendar-grid-right)
(define-key *calendar-mode-keymap* "h" 'calendar-grid-left)
(define-key *calendar-mode-keymap* "j" 'calendar-down-day)
(define-key *calendar-mode-keymap* "k" 'calendar-up-day)
;; sequential date navigation
(define-key *calendar-mode-keymap* "C-n" 'calendar-next-day)
(define-key *calendar-mode-keymap* "C-p" 'calendar-previous-day)
(define-key *calendar-mode-keymap* "m" 'calendar-toggle-mark)
(define-key *calendar-mode-keymap* "M" 'calendar-clear-marks)

;; make it work in vim-mode normal state
(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode calendar-mode))
  (list *calendar-mode-keymap*))