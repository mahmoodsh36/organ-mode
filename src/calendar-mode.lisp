(defpackage :organ/calendar-mode
  (:use :cl :lem)
  (:export :calendar
           :calendar-with-callback))

(in-package :organ/calendar-mode)

(define-editor-variable *calendar-date* nil)
(define-editor-variable *today-point* nil)
(define-editor-variable *source-buffer* nil)
(define-editor-variable *selected-date* nil)
(define-editor-variable *calendar-callback* nil)
(define-editor-variable *calendar-marked-dates* nil)

(define-editor-variable *calendar-grid-width* 3)
(define-editor-variable *calendar-grid-height* 3)
(define-editor-variable *calendar-commands-history* nil)
(define-editor-variable *calendar-mode-debug* nil
  "when non-nil, enables debugging messages for calendar navigation.")

(define-major-mode calendar-mode nil
  (:name "calendar"
   :keymap *calendar-mode-keymap*))

(defstruct calendar-segment
  text
  ts)

(defun render-month-to-lines (timestamp)
  (let* ((now (local-time:now))
         (year (local-time:timestamp-year timestamp))
         (month (local-time:timestamp-month timestamp))
         (first-day-of-month (local-time:encode-timestamp 0 0 0 0 1 month year))
         (days-in-month (local-time:days-in-month month year))
         (padding-count (mod (local-time:timestamp-day-of-week first-day-of-month) 7)))
    ;; create a single, flat list of all calendar cells (padding followed by days).
    (let ((grid-cells
            (append
             (loop repeat padding-count
                   collect (make-calendar-segment :text "    "))
             (loop for day from 1 to days-in-month
                   for ts = (local-time:encode-timestamp 0 0 0 0 day month year)
                   collect (if (and (= (local-time:timestamp-year ts)
                                       (local-time:timestamp-year now))
                                    (= (local-time:timestamp-month ts)
                                       (local-time:timestamp-month now))
                                    (= (local-time:timestamp-day ts)
                                       (local-time:timestamp-day now)))
                               (make-calendar-segment
                                :text (format nil "[~2d]" day)
                                :ts ts)
                               (make-calendar-segment
                                :text (format nil " ~2d " day)
                                :ts ts))))))
      ;; partition the flat list of cells into weeks (sublists of 7).
      (let ((weeks (loop while grid-cells
                         collect (let ((week (subseq
                                              grid-cells
                                              0
                                              (min 7 (length grid-cells)))))
                                   (setf grid-cells (nthcdr 7 grid-cells))
                                   week))))
        ;; combine headers and the partitioned weeks into the final list of lines.
        (coerce
         (cons
          (list (make-calendar-segment
                 :text (local-time:format-timestring
                        nil
                        first-day-of-month
                        :format '(:long-month " " :year))))
          (cons
           (list (make-calendar-segment :text "  Su  Mo  Tu  We  Th  Fr  Sa"))
           weeks))
         'list)))))

(defun normalize-day-of-week (dow)
  "convert local-time's DOW (0=Mon ... 6=Sun) to sunday-first (0=Sun ... 6=Sat)."
  (mod (1+ dow) 7))

(defun get-date-under-cursor (&optional (point (current-point)))
  "get the full date timestamp under the cursor. relies on the :calendar-date text property."
  (text-property-at point :calendar-date))

(defun get-month-index-in-grid (month
                                year
&optional (calendar-date (variable-value '*calendar-date* :buffer (current-buffer))))
  "get the index of a month/year in the current grid display"
  (let* ((buffer (current-buffer))
         (start-month (local-time:timestamp-month calendar-date))
         (start-year (local-time:timestamp-year calendar-date))
         (num-months (* (variable-value '*calendar-grid-width* :buffer buffer)
                        (variable-value '*calendar-grid-height* :buffer buffer))))
    (loop for i from 0 below num-months
          for ts = (local-time:timestamp+
                    (local-time:encode-timestamp 0
                                                 0
                                                 0
                                                 0
                                                 1
                                                 start-month
                                                 start-year)
                    i :month)
          when (and (= (local-time:timestamp-month ts) month)
                    (= (local-time:timestamp-year ts) year))
            return i)))

(defun get-grid-position-for-month-index (index)
  "get the (row, column) position for a month index in the grid."
  (when index
    (let ((buffer (current-buffer)))
      (values (floor index (variable-value '*calendar-grid-width* :buffer buffer)) ;; row
              (mod index (variable-value '*calendar-grid-width* :buffer buffer)))))) ;; column

(defun get-date-grid-position (date-ts)
  "get the (row, column) position of a date in the current grid."
  (when date-ts
    (let ((month (local-time:timestamp-month date-ts))
          (year (local-time:timestamp-year date-ts)))
      (let ((index (get-month-index-in-grid month year)))
        (when index
          (get-grid-position-for-month-index index))))))

(defun get-date-position-within-month (date-ts)
  "get the (week, weekday) position of a date within its month (0-based)."
  (when date-ts
    (let* ((buffer (current-buffer))
           (day (local-time:timestamp-day date-ts))
           (month (local-time:timestamp-month date-ts))
           (year (local-time:timestamp-year date-ts))
           (first-day-of-month (local-time:encode-timestamp 0 0 0 0 1 month year))
           (first-day-weekday
             (normalize-day-of-week
              (local-time:timestamp-day-of-week first-day-of-month)))
           (weekday-index (mod (+ (1- day) first-day-weekday) 7))
           (week-index (floor (+ (1- day) first-day-weekday) 7)))
      (when (variable-value '*calendar-mode-debug* :buffer buffer)
        (multiple-value-bind (grid-row grid-col) (get-date-grid-position date-ts)
          (message "DEBUG pos: ~a/~a -> first-day=~a, weekday=~a, week=~a, grid=~a,~a"
                   month day first-day-weekday weekday-index week-index grid-row grid-col)))
      (values week-index weekday-index))))

(defun get-date-at-month-position (target-month-ts week-index weekday-index)
  "get the date at a specific (week, weekday) position within a target month"
  (when target-month-ts
    (let* ((target-month (local-time:timestamp-month target-month-ts))
           (target-year (local-time:timestamp-year target-month-ts))
           (first-day-of-target-month (local-time:encode-timestamp 0 0 0 0 1 target-month target-year))
           (first-day-weekday
             (normalize-day-of-week
              (local-time:timestamp-day-of-week first-day-of-target-month)))
           (target-day (+ (* week-index 7)
                          weekday-index
                          (- first-day-weekday)
                          1)))
      (loop while (<= target-day 0) do (incf target-day 7))
      (loop while (> target-day
                     (local-time:days-in-month target-month target-year))
            do (decf target-day 7))
      (when (and (> target-day 0)
                 (<= target-day
                     (local-time:days-in-month target-month target-year)))
        (local-time:encode-timestamp 0 0 0 0 target-day target-month target-year)))))

(defun find-date-in-line (line-point target-col)
  "find the date in the given line that is closest to the target column."
  (let ((buffer (current-buffer)))
    (when (variable-value '*calendar-mode-debug* :buffer buffer)
      (message "DEBUG: find-date-in-line: target-col=~a" target-col))
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
        (when (and best-point (variable-value '*calendar-mode-debug* :buffer buffer))
          (message "DEBUG: selected date at col ~a (best distance ~a)"
                   (point-column best-point) best-distance))
        best-point))))

(defun find-cross-month-target (current-point direction)
  "find cross-month target in DIRECTION (:up or :down), by scanning a fixed number of lines and picking the best candidate."
  (let* ((buffer (current-buffer))
         (current-col (point-column current-point))
         (line-step (if (eq direction :up) -1 1))
         (current-date (get-date-under-cursor))
         (best-candidate)
         (best-distance most-positive-fixnum))
    (unless current-date (return-from find-cross-month-target nil))
    (when (variable-value '*calendar-mode-debug* :buffer buffer)
      (message "DEBUG: find-cross-month-target: direction=~a, current-col=~a, current-date=~a/~a"
               direction current-col
               (local-time:timestamp-month current-date)
               (local-time:timestamp-day current-date)))
    (with-point ((search-point (copy-point current-point :temporary)))
      (loop repeat 8
            while (line-offset search-point line-step)
            do (when (looking-at search-point "^\\s *[0-9]+")
                 (let ((candidate-point (find-date-in-line search-point current-col)))
                   (when candidate-point
                     (let ((found-date (get-date-under-cursor-at-point candidate-point)))
                       (when (and found-date
                                  (or (not (= (local-time:timestamp-month found-date)
                                              (local-time:timestamp-month current-date)))
                                      (not (= (local-time:timestamp-year found-date)
                                              (local-time:timestamp-year current-date)))))
                         (let ((distance (abs (- (point-column candidate-point) current-col))))
                           (when (variable-value '*calendar-mode-debug* :buffer buffer)
                             (message "DEBUG: found candidate ~a/~a at col ~a (distance ~a)"
                                      (local-time:timestamp-month found-date)
                                      (local-time:timestamp-day found-date)
                                      (point-column candidate-point)
                                      distance))
                           (when (< distance best-distance)
                             (setf best-distance distance)
                             (setf best-candidate found-date))
                           (when (zerop distance)
                             (return-from find-cross-month-target best-candidate)))))))))
      best-candidate)))

(defun find-date-near-column (point target-col)
  "find a date near the target column in the current line"
  (let ((found-point))
    (loop for offset from 0 to 10
          do (let ((right-pos (copy-point point :temporary)))
               (when (and (character-offset right-pos offset)
                          (text-property-at right-pos :calendar-day))
                 (setf found-point right-pos)
                 (return))))
    (unless found-point
      (loop for offset from 1 to 10
            do (let ((left-pos (copy-point point :temporary)))
                 (when (and (character-offset left-pos (- offset))
                            (text-property-at left-pos :calendar-day))
                   (setf found-point left-pos)
                   (return)))))
    found-point))

;; local (same-month) navigation
(defun try-intra-month-navigation (direction)
  "try to navigate within the current month, return T if successful."
  (let* ((buffer (current-buffer))
         (current-col (point-charpos (current-point)))
         (line-step (if (eq direction :up) -1 1)))
    (when (variable-value '*calendar-mode-debug* :buffer buffer)
      (message "DEBUG: try-intra-month-navigation: direction=~a, strict-mode=~a"
               direction (variable-value '*calendar-strict-grid-navigation* :buffer buffer)))
    (with-point ((p (current-point)))
      (loop
        (unless (line-offset p line-step)
          (when (variable-value '*calendar-mode-debug* :buffer buffer)
            (message "DEBUG: reached buffer edge in intra-month nav"))
          (return-from try-intra-month-navigation nil))
        (let ((line-content (line-string p))
              (line-len (length (line-string p))))
          (when (variable-value '*calendar-mode-debug* :buffer buffer)
            (message "DEBUG: intra-month nav checking line='~a', len=~a" line-content line-len))
          (cond
            ((and (> line-len 0) (looking-at p "^\\s *[0-9]+"))
             (let ((target-pos (min current-col (1- line-len))))
               (character-offset p target-pos)
               (let ((has-date-at-col (text-property-at p :calendar-day)))
                 (when (variable-value '*calendar-mode-debug* :buffer buffer)
                   (message "DEBUG: intra-month nav: target-pos=~a, has-date-at-col=~a"
                            target-pos has-date-at-col))
                 (if has-date-at-col
                     (progn
                       (when (variable-value '*calendar-mode-debug* :buffer buffer)
                         (message "DEBUG: found date at exact target column"))
                       (move-point (current-point) p)
                       (highlight-current-day)
                       (return-from try-intra-month-navigation t))
                     (if (variable-value '*calendar-strict-grid-navigation* :buffer buffer)
                         (progn
                           (when (variable-value '*calendar-mode-debug* :buffer buffer)
                             (message "DEBUG: strict mode: no date at target column, going to cross-month"))
                           (return-from try-intra-month-navigation nil))
                         (let ((found-point (find-date-near-column p current-col)))
                           (if found-point
                               (progn
                                 (when (variable-value '*calendar-mode-debug* :buffer buffer)
                                   (message "DEBUG: found nearby date"))
                                 (move-point (current-point) found-point)
                                 (highlight-current-day)
                                 (return-from try-intra-month-navigation t))
                               (progn
                                 (when (variable-value '*calendar-mode-debug* :buffer buffer)
                                   (message "DEBUG: no nearby date found"))
                                 (return-from try-intra-month-navigation nil)))))))))
            (t
             (when (variable-value '*calendar-mode-debug* :buffer buffer)
               (message "DEBUG: intra-month nav: empty line or header, continuing search"))
             nil)))))))

(defun navigate-vertically (direction)
  "main navigation logic for up/down movement. tries intra-month navigation first. if that fails, performs a cross-month visual search."
  (let ((buffer (current-buffer)))
    (unless (try-intra-month-navigation direction)
      (when (variable-value '*calendar-mode-debug* :buffer buffer)
        (message "DEBUG: try-cross-month-navigation: direction=~a" direction))
      (let ((target-date (find-cross-month-target (current-point) direction)))
        (if target-date
            (progn
              (when (variable-value '*calendar-mode-debug* :buffer buffer)
                (message "DEBUG: cross-month target found: ~a/~a"
                         (local-time:timestamp-month target-date)
                         (local-time:timestamp-day target-date)))
              (move-to-date target-date)
              (highlight-current-day))
            (when (variable-value '*calendar-mode-debug* :buffer buffer)
              (message "DEBUG: no cross-month target found")))))))

(defun get-date-under-cursor-at-point (point)
  "get the date under a specific point."
  (let* ((buffer (current-buffer))
         (date-ts (text-property-at point :calendar-date)))
    (if date-ts
        date-ts
        (let ((day (text-property-at point :calendar-day)))
          (when day
            (local-time:encode-timestamp
             0 0 0 0
             day
             (local-time:timestamp-month (variable-value '*calendar-date* :buffer buffer))
             (local-time:timestamp-year (variable-value '*calendar-date* :buffer buffer))))))))

(defun calendar-current-day-overlay (buffer)
  "get the current day overlay for the buffer"
  (buffer-value buffer 'calendar-current-day-overlay))

(defun (setf calendar-current-day-overlay) (overlay buffer)
  "set the current day overlay for the buffer"
  (setf (buffer-value buffer 'calendar-current-day-overlay) overlay))

(defun calendar-marked-dates-overlays (buffer)
  "get the list of marked date overlays for the buffer"
  (buffer-value buffer 'calendar-marked-dates-overlays))

(defun (setf calendar-marked-dates-overlays) (overlays buffer)
  "set the list of marked date overlays for the buffer"
  (setf (buffer-value buffer 'calendar-marked-dates-overlays) overlays))

(defun calendar-today-overlay (buffer)
  "get the today overlay for the buffer"
  (buffer-value buffer 'calendar-today-overlay))

(defun (setf calendar-today-overlay) (overlay buffer)
  "set the today overlay for the buffer"
  (setf (buffer-value buffer 'calendar-today-overlay) overlay))

(define-editor-variable *calendar-strict-grid-navigation* t
  "if true, navigation jumps to next/previous month when no date exists in target column.
if nil, navigation finds nearest date in same month first.")

(define-attribute current-day
    (t :foreground :base00 :background :base0D))

(define-attribute marked-day
    (t :foreground :base00 :background :base0B))

(define-attribute today
    (t :foreground :base00 :background :base0A))

(defun highlight-current-day ()
  "highlight the current day under the cursor."
  (let ((buffer (current-buffer)))
    (when (calendar-current-day-overlay buffer)
      (delete-overlay (calendar-current-day-overlay buffer))
      (setf (calendar-current-day-overlay buffer) nil))
    (let ((current-date (get-date-under-cursor)))
      (when current-date
        (let ((date-pos (find-day-position
                         buffer
                         (local-time:timestamp-day current-date)
                         (local-time:timestamp-month current-date)
                         (local-time:timestamp-year current-date))))
          (when date-pos
            (with-point ((end-pos (copy-point date-pos :temporary)))
              (let ((day (text-property-at date-pos :calendar-day)))
                (when day
                  (character-offset end-pos 4)
                  (setf (calendar-current-day-overlay buffer)
                        (make-overlay date-pos end-pos 'current-day)))))))))))

(defun clear-marked-dates-overlays ()
  "clear all marked date overlays."
  (let ((buffer (current-buffer)))
    (dolist (overlay (calendar-marked-dates-overlays buffer))
      (delete-overlay overlay))
    (setf (calendar-marked-dates-overlays buffer) '())))

(defun clear-today-overlay ()
  "clear the today date overlay."
  (let ((buffer (current-buffer)))
    (when (calendar-today-overlay buffer)
      (delete-overlay (calendar-today-overlay buffer))
      (setf (calendar-today-overlay buffer) nil))))

(defun same-date-p (date1 date2)
  "check if two timestamps represent the same calendar day"
  (and (= (local-time:timestamp-year date1) (local-time:timestamp-year date2))
       (= (local-time:timestamp-month date1) (local-time:timestamp-month date2))
       (= (local-time:timestamp-day date1) (local-time:timestamp-day date2))))

(defun format-calendar-date (date &optional (format '((:year 4) "-" (:month 2) "-" (:day 2))))
  (local-time:format-timestring nil date :format format))

(defun highlight-marked-dates ()
  "highlight all marked dates with overlays."
  (let ((buffer (current-buffer)))
    (clear-marked-dates-overlays)
    (with-point ((search-point (buffer-point buffer)))
      (dolist (date (variable-value '*calendar-marked-dates* :buffer buffer))
        (let ((date-pos (find-day-position buffer
                                           (local-time:timestamp-day date)
                                           (local-time:timestamp-month date)
                                           (local-time:timestamp-year date))))
          (when date-pos
            (with-point ((end-pos (copy-point date-pos :temporary)))
              (let ((day (text-property-at date-pos :calendar-day)))
                (when day
                  (character-offset end-pos 4)
                  (push (make-overlay date-pos end-pos 'marked-day)
                        (calendar-marked-dates-overlays buffer)))))))))))

(defun highlight-today-date ()
  "highlight today's date with overlay."
  (let* ((buffer (current-buffer))
         (today-point (variable-value '*today-point* :buffer buffer)))
    (clear-today-overlay)
    (when today-point
      (with-point ((end-pos (copy-point today-point :temporary)))
        (let ((day (text-property-at today-point :calendar-day)))
          (when day
            (character-offset end-pos 4)
            (setf (calendar-today-overlay buffer)
                  (make-overlay today-point end-pos 'today))))))))

(defun refresh-calendar-highlights ()
  "refresh all calendar highlighting in the correct order."
  (highlight-current-day)
  (highlight-marked-dates)
  (highlight-today-date))

(defun date-marked-p (date)
  "check if a date is marked."
  (when date
    (let ((buffer (current-buffer)))
      (member date (variable-value '*calendar-marked-dates* :buffer buffer) :test #'same-date-p))))

(defun toggle-date-mark ()
  "toggle mark on the date under cursor"
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor)))
    (when current-date
      (if (date-marked-p current-date)
          (progn
            (setf (variable-value '*calendar-marked-dates* :buffer buffer)
                  (remove current-date
                          (variable-value '*calendar-marked-dates* :buffer buffer)
                          :test #'same-date-p))
            (message "unmarked date: ~A" (format-calendar-date current-date)))
          (progn
            (push current-date (variable-value '*calendar-marked-dates* :buffer buffer))
            (message "marked date: ~A" (format-calendar-date current-date))))
      (refresh-calendar-highlights))))

(defun clear-all-marks ()
  "clear all marked dates"
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor)))
    (setf *calendar-marked-dates* nil)
    (clear-marked-dates-overlays)
    (update-calendar-display buffer)
    (when current-date
      (move-to-date current-date))
    (refresh-calendar-highlights)
    (message "cleared all marks")))

(defun calendar-cleanup (buffer)
  "clean up calendar-specific state for a buffer"
  (when (calendar-current-day-overlay buffer)
    (delete-overlay (calendar-current-day-overlay buffer))
    (setf (calendar-current-day-overlay buffer) nil))
  (dolist (overlay (calendar-marked-dates-overlays buffer))
    (delete-overlay overlay))
  (setf (calendar-marked-dates-overlays buffer) '())
  (when (calendar-today-overlay buffer)
    (delete-overlay (calendar-today-overlay buffer))
    (setf (calendar-today-overlay buffer) nil))
  ;; clear buffer-local variables
  (setf (buffer-value buffer 'calendar-current-day-overlay) nil)
  (setf (buffer-value buffer 'calendar-marked-dates-overlays) nil)
  (setf (buffer-value buffer 'calendar-today-overlay) nil))

(defun month-visible-in-grid (month year &optional (calendar-date (variable-value '*calendar-date* :buffer (current-buffer))))
  "check if the specified month/year is visible in the current calendar grid"
  (let* ((buffer (current-buffer))
         (start-month (local-time:timestamp-month calendar-date))
         (start-year (local-time:timestamp-year calendar-date))
         (num-months (* (variable-value '*calendar-grid-width* :buffer buffer)
                        (variable-value '*calendar-grid-height* :buffer buffer)))
         (visible-months))
    (loop for i from 0 below num-months
          for ts = (local-time:timestamp+
                    (local-time:encode-timestamp 0 0 0 0 1 start-month start-year)
                    i :month)
          do (push (cons (local-time:timestamp-month ts)
                         (local-time:timestamp-year ts))
                   visible-months))
    (member (cons month year)
            visible-months
            :test (lambda (a b)
                    (and (= (car a) (car b))
                         (= (cdr a) (cdr b)))))))

(defun move-to-date (target-date &key (scroll-style :jump) (calendar-date-var '*calendar-date*))
  "move cursor to the specified date, updating calendar if needed.
SCROLL-STYLE can be :jump (the default) or :scroll.
:jump will redraw the grid starting with the target month if it's not visible.
:scroll will shift the grid by one month in the direction of the target date."
  (let* ((buffer (current-buffer))
         (calendar-date (if (eq calendar-date-var '*calendar-date*)
                            (variable-value '*calendar-date* :buffer buffer)
                            (symbol-value calendar-date-var))))
    (when (variable-value '*calendar-mode-debug* :buffer buffer)
      (message "DEBUG move-to-date: called for ~a with style ~a" target-date scroll-style))
    (let ((target-day (local-time:timestamp-day target-date))
          (target-month (local-time:timestamp-month target-date))
          (target-year (local-time:timestamp-year target-date)))
      (unless (month-visible-in-grid target-month target-year calendar-date)
        (when (variable-value '*calendar-mode-debug* :buffer buffer)
          (message "DEBUG move-to-date: target month not visible. Recalculating view."))
        (cond
((eq scroll-style :scroll)
            (when (variable-value '*calendar-mode-debug* :buffer buffer)
              (message "DEBUG move-to-date: scrolling one month."))
            (if (local-time:timestamp< target-date calendar-date)
                (setf (variable-value calendar-date-var :buffer buffer)
                      (local-time:timestamp- calendar-date 1 :month))
                (setf (variable-value calendar-date-var :buffer buffer)
                      (local-time:timestamp+ calendar-date 1 :month))))
          (t ;; :jump style (the default)
           (when (variable-value '*calendar-mode-debug* :buffer buffer)
             (message "DEBUG move-to-date: jumping to new grid start."))
           (setf (variable-value calendar-date-var :buffer buffer)
                 (local-time:encode-timestamp 0 0 0 0 1 target-month target-year))))
        (update-calendar-display buffer)
        (when (variable-value '*calendar-mode-debug* :buffer buffer)
          (message "DEBUG move-to-date: redraw complete.")))
      (when (variable-value '*calendar-mode-debug* :buffer buffer)
        (message "DEBUG move-to-date: calling move-to-calendar-day for ~a/~a/~a" target-day target-month target-year))
      (move-to-calendar-day target-day target-month target-year)
      (when (variable-value '*calendar-mode-debug* :buffer buffer)
        (message "DEBUG move-to-date: returned from move-to-calendar-day. highlighting."))
      (refresh-calendar-highlights))))

(defun find-day-position (buffer day target-month target-year)
  "find the position of a specific day in the buffer for the specific month/year."
  (when (variable-value '*calendar-mode-debug* :buffer buffer)
    (message "DEBUG find-day-position: searching for ~a/~a/~a" day target-month target-year))
  (with-point ((point (buffer-point buffer)))
    (buffer-start point)
    (loop
      (let ((date-ts (text-property-at point :calendar-date)))
        (when date-ts
          (when (and (= (local-time:timestamp-day date-ts) day)
                     (= (local-time:timestamp-month date-ts) target-month)
                     (= (local-time:timestamp-year date-ts) target-year))
            (when (variable-value '*calendar-mode-debug* :buffer buffer)
              (message "DEBUG find-day-position: found a match. returning point."))
            (return-from find-day-position (copy-point point :temporary)))))
      (unless (character-offset point 1)
        (when (variable-value '*calendar-mode-debug* :buffer buffer)
          (message "DEBUG find-day-position: reached end of buffer without a match."))
        (return-from find-day-position nil)))))

(defun move-to-calendar-day (day target-month target-year)
  "move the buffer's cursor to a specific day in the calendar."
  (let ((buffer (current-buffer)))
    (when (variable-value '*calendar-mode-debug* :buffer buffer)
      (message "DEBUG move-to-calendar-day: looking for ~a/~a/~a" day target-month target-year))
    (let ((day-pos (find-day-position buffer day target-month target-year)))
      (if day-pos
          (progn
            (when (variable-value '*calendar-mode-debug* :buffer buffer)
              (message "DEBUG move-to-calendar-day: Found position. Moving point."))
            (move-point (current-point) day-pos))
          (when (variable-value '*calendar-mode-debug* :buffer buffer)
            (message "DEBUG move-to-calendar-day: Position was NIL. Did not move point."))))))

(defun update-calendar-display (buffer &optional (calendar-date-var '*calendar-date*))
  (let* ((calendar-date (if (eq calendar-date-var '*calendar-date*)
                            (variable-value '*calendar-date* :buffer buffer)
                            (symbol-value calendar-date-var)))
         (grid-width (variable-value '*calendar-grid-width* :buffer buffer))
         (grid-height (variable-value '*calendar-grid-height* :buffer buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (setf (variable-value '*today-point* :buffer buffer) nil)
      (let* ((point (buffer-point buffer))
             (timestamp (or calendar-date (local-time:now)))
             (num-months (* grid-width grid-height))
             (calendars (loop for i from 0 below num-months
                              collect (render-month-to-lines
                                       (local-time:timestamp+ timestamp
                                                              i :month)))))
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
                                     start
                                     point
                                     :calendar-day (local-time:timestamp-day
                                                    date-ts))
                                    (put-text-property
                                     start
                                     point
                                     :calendar-date date-ts)
                                    (let ((now (local-time:now)))
                                      (when (and (= (local-time:timestamp-day date-ts)
                                                    (local-time:timestamp-day now))
                                                 (= (local-time:timestamp-month date-ts)
                                                    (local-time:timestamp-month now))
                                                 (= (local-time:timestamp-year date-ts)
                                                    (local-time:timestamp-year now)))
                                        (setf (variable-value '*today-point* :buffer buffer)
                                              (copy-point start
                                                          :temporary))))))))
                            (dotimes (i (- 28 line-width))
                              (insert-string point " ")))
                          (insert-string
                           point
                           (make-string 28
                                        :initial-element #\space))))
                    (insert-string point "  "))
                  (insert-string point (string #\newline)))
                (insert-string point (string #\newline))))))))))

(define-command calendar () ()
  (calendar-with-callback nil "*calendar*" nil))

(defun calendar-with-callback (callback &optional (buffer-name "*calendar*") initial-date)
  "open a calendar view with a callback function.
CALLBACK is a function that receives the selected date as argument.
INITIAL-DATE can be a timestamp or nil for today."
  (let ((source-buffer (current-buffer)))
    (let ((buffer (make-buffer buffer-name)))
      (switch-to-buffer buffer)
      (calendar-mode)
      ;; Initialize buffer-local state
      (setf (calendar-current-day-overlay buffer) nil)
      (setf (calendar-marked-dates-overlays buffer) '())
      (setf (calendar-today-overlay buffer) nil)
      (setf (variable-value '*calendar-grid-width* :buffer buffer) 3)
      (setf (variable-value '*calendar-grid-height* :buffer buffer) 3)
      (setf (variable-value '*calendar-commands-history* :buffer buffer) nil)
      (setf (variable-value '*calendar-mode-debug* :buffer buffer) nil)
      (setf (variable-value '*calendar-strict-grid-navigation* :buffer buffer) t)
      ;; Add cleanup hook
      (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
                #'calendar-cleanup)
      ;; Set calendar state
      (setf (variable-value '*calendar-date* :buffer buffer) (or initial-date (local-time:now)))
      (setf (variable-value '*source-buffer* :buffer buffer) source-buffer)
      (setf (variable-value '*calendar-callback* :buffer buffer) callback)
(setf (variable-value '*calendar-marked-dates* :buffer buffer) nil)
      (clear-marked-dates-overlays)
      (clear-today-overlay)
      (update-calendar-display buffer)
      (calendar-goto-today)
      (move-to-date (variable-value '*calendar-date* :buffer buffer))
      (refresh-calendar-highlights))))

(define-command quit-calendar () ()
  "quit the calendar view."
  (let ((buffer (current-buffer)))
    (when (calendar-current-day-overlay buffer)
      (delete-overlay (calendar-current-day-overlay buffer))
      (setf (calendar-current-day-overlay buffer) nil))
    (clear-marked-dates-overlays)
    (clear-today-overlay)
    (kill-buffer buffer)))

(define-command calendar-increase-width () ()
  (let ((buffer (current-buffer)))
    (setf (variable-value '*calendar-grid-width* :buffer buffer)
          (1+ (variable-value '*calendar-grid-width* :buffer buffer)))
    (update-calendar-display buffer)
    (refresh-calendar-highlights)))

(define-command calendar-decrease-width () ()
  (let ((buffer (current-buffer)))
    (when (> (variable-value '*calendar-grid-width* :buffer buffer) 1)
      (setf (variable-value '*calendar-grid-width* :buffer buffer)
            (1- (variable-value '*calendar-grid-width* :buffer buffer)))
      (update-calendar-display buffer)
      (refresh-calendar-highlights))))

(define-command calendar-increase-height () ()
  (let ((buffer (current-buffer)))
    (setf (variable-value '*calendar-grid-height* :buffer buffer)
          (1+ (variable-value '*calendar-grid-height* :buffer buffer)))
    (update-calendar-display buffer)
    (refresh-calendar-highlights)))

(define-command calendar-decrease-height () ()
  (let ((buffer (current-buffer)))
    (when (> (variable-value '*calendar-grid-height* :buffer buffer) 1)
      (setf (variable-value '*calendar-grid-height* :buffer buffer)
            (1- (variable-value '*calendar-grid-height* :buffer buffer)))
      (update-calendar-display buffer)
      (refresh-calendar-highlights))))

(defun calendar-at-date (initial-date)
  "open a calendar view at a specific date.
INITIAL-DATE should be a local-time timestamp."
  (calendar-with-callback nil "*calendar*" initial-date))

(define-command open-calendar-next-week () ()
  "open a calendar view starting at next week."
  (calendar-at-date (local-time:timestamp+ (local-time:now) 7 :day)))

(defun navigate-to-date-or-first-visible (target-date)
  "move to target date, or first visible date if target is not in grid"
  (let ((buffer (current-buffer)))
    (if (month-visible-in-grid (local-time:timestamp-month target-date)
                               (local-time:timestamp-year target-date)
                               (variable-value '*calendar-date* :buffer buffer))
        (move-to-date target-date)
        ;; Fallback: move to first day of the first visible month
        (let ((first-visible-date (variable-value '*calendar-date* :buffer buffer)))
          (move-to-calendar-day 1
                                (local-time:timestamp-month first-visible-date)
                                (local-time:timestamp-year first-visible-date))))))

(defun navigate-calendar (offset unit)
  "navigate calendar by OFFSET units of UNIT (month or year)"
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor)))
    (setf (variable-value '*calendar-date* :buffer buffer)
          (local-time:timestamp+ (variable-value '*calendar-date* :buffer buffer) offset unit))
    (update-calendar-display buffer)
    (when current-date
      (navigate-to-date-or-first-visible current-date))
    (refresh-calendar-highlights)))

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
         (new-date (local-time:encode-timestamp 0 0 0 0 day month year)))
    (setf (variable-value '*calendar-date* :buffer buffer) new-date)
    (update-calendar-display buffer)
    (move-to-calendar-day day month year)
    (refresh-calendar-highlights)))

(define-command calendar-show-date () ()
  "display the date under the cursor."
  (let ((current-date (get-date-under-cursor)))
    (if current-date
        (let ((date-string (local-time:format-timestring
                            nil current-date
                            :format '(:long-month
                                      " "
                                      (:day 2)
                                      ", "
                                      (:year 4)))))
          (message date-string))
        (message "no date at cursor position"))))

(define-command calendar-select-and-exit () ()
  "select the date under the cursor and call the callback, then close calendar."
  (let* ((buffer (current-buffer))
         (current-date (get-date-under-cursor))
         (marked-dates (variable-value '*calendar-marked-dates* :buffer buffer))
         (calendar-buffer buffer))
    (let ((callback (variable-value '*calendar-callback* :buffer buffer))
          (source-buffer (variable-value '*source-buffer* :buffer buffer)))
      (if callback
          (progn
            (if marked-dates
                (funcall callback marked-dates)
                (when current-date
                  (funcall callback (list current-date))))
            (kill-buffer calendar-buffer))
          (progn
            (switch-to-buffer source-buffer)
            (if marked-dates
                (dolist (date (sort marked-dates #'local-time:timestamp<))
                  (insert-string
                   (current-point)
                   (format nil "~A~%"
                           (local-time:format-timestring
                            nil date
                            :format '((:year 4) "-" (:month 2) "-" (:day 2))))))
                (when current-date
                  (insert-string
                   (current-point)
                   (local-time:format-timestring
                    nil current-date
                    :format '((:year 4) "-" (:month 2) "-" (:day 2))))))
            (kill-buffer calendar-buffer))))))

(defun navigate-grid-horizontally (direction)
  "navigate horizontally in the calendar grid.
DIRECTION can be :left or :right."
  (let* ((buffer (current-buffer))
         (strict-navigation (variable-value '*calendar-strict-grid-navigation* :buffer buffer))
         (found-on-line)
         (step (if (eq direction :right) 1 -1))
         (line-fn (if (eq direction :right) #'line-end #'line-start))
         (boundary-fn (if (eq direction :right) #'point< #'point>))
         (buffer-boundary (if (eq direction :right)
                             (buffer-end-point buffer)
                             (buffer-start-point buffer))))
    ;; first, try to find the next/previous date on the current line
    (with-point ((p (copy-point (current-point) :temporary))
                 (boundary-point (copy-point (current-point) :temporary)))
      (funcall line-fn boundary-point)
      (let ((current-day (text-property-at p :calendar-day)))
        ;; move past the current date cell
        (loop while (and (character-offset p step)
                         (funcall boundary-fn p boundary-point)
                         (equal (text-property-at p :calendar-day) current-day))))
      ;; search for the next/previous date cell
      (loop while (funcall boundary-fn p boundary-point)
            do (when (text-property-at p :calendar-day)
                 (move-point (current-point) p)
                 (highlight-current-day)
                 (setf found-on-line t)
                 (return))
               (unless (character-offset p step)
                 (return))))
    ;; if not found, search other lines if not in strict mode
    (when (and (not found-on-line) (not strict-navigation))
      (with-point ((p (current-point)))
        (line-offset p step)
        (loop
          ;; if we've hit the buffer boundary, stop
          (unless (funcall boundary-fn p buffer-boundary)
            (return))
          ;; scan the new line for dates
          (with-point ((line-scanner p))
            (if (eq direction :right)
                (progn
                  (line-start line-scanner)
                  (with-point ((line-end p))
                    (line-end line-end)
                    (loop while (point< line-scanner line-end)
                          do (when (text-property-at line-scanner :calendar-day)
                               (move-point (current-point) line-scanner)
                               (highlight-current-day)
                               (return-from navigate-grid-horizontally))
                             (character-offset line-scanner 1))))
                (progn
                  (line-end line-scanner)
                  (with-point ((line-start p))
                    (line-start line-start)
                    (loop while (point> line-scanner line-start)
                          do (character-offset line-scanner -1)
                             (when (text-property-at line-scanner :calendar-day)
                               (move-point (current-point) line-scanner)
                               (highlight-current-day)
                               (return-from navigate-grid-horizontally)))))))
          ;; no date found on this line, try the next
          (unless (line-offset p step)
            (return)))))))

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

;; (define-command calendar-debug-position () ()
;;   "debug the grid position of the current date"
;;   (let ((current-date (get-date-under-cursor)))
;;     (when current-date
;;       (multiple-value-bind (grid-row grid-col) (get-date-grid-position current-date)
;;         (multiple-value-bind (week-index weekday-index) (get-date-position-within-month current-date)
;;           (message "current date ~a/~a: grid=~a,~a, month-internal=week=~a,weekday=~a"
;;                    (local-time:timestamp-month current-date)
;;                    (local-time:timestamp-day current-date)
;;                    grid-row grid-col week-index weekday-index))))))

(define-command calendar-toggle-debug () ()
  "toggle verbose debugging messages for calendar navigation."
  (let ((buffer (current-buffer)))
    (setf (variable-value '*calendar-mode-debug* :buffer buffer)
          (not (variable-value '*calendar-mode-debug* :buffer buffer)))
    (message "calendar debug mode ~:[disabled~;enabled~]."
             (variable-value '*calendar-mode-debug* :buffer buffer))))

(define-command calendar-toggle-mark () ()
  "toggle mark on the date under cursor."
  (toggle-date-mark))

(define-command calendar-clear-marks () ()
  "clear all marked dates."
  (clear-all-marks))

;; keys
(define-key *calendar-mode-keymap* "q" 'quit-calendar)
(define-key *calendar-mode-keymap* "n" 'calendar-next-month)
(define-key *calendar-mode-keymap* "p" 'calendar-previous-month)
(define-key *calendar-mode-keymap* "M->" 'calendar-next-year)
(define-key *calendar-mode-keymap* "M-<" 'calendar-previous-year)
(define-key *calendar-mode-keymap* "g" 'calendar-goto-today)
(define-key *calendar-mode-keymap* "Return" 'calendar-select-and-exit)
(define-key *calendar-mode-keymap* "d" 'calendar-goto-date)
(define-key *calendar-mode-keymap* "D" 'calendar-toggle-debug)
;; keybindings for visual grid navigation
(define-key *calendar-mode-keymap* "l" 'calendar-grid-right)
(define-key *calendar-mode-keymap* "h" 'calendar-grid-left)
(define-key *calendar-mode-keymap* "j" 'calendar-down-day)
(define-key *calendar-mode-keymap* "k" 'calendar-up-day)
;; keybindings for sequential date navigation
(define-key *calendar-mode-keymap* "C-n" 'calendar-next-day)
(define-key *calendar-mode-keymap* "C-p" 'calendar-previous-day)
;; marks
(define-key *calendar-mode-keymap* "m" 'calendar-toggle-mark)
(define-key *calendar-mode-keymap* "M" 'calendar-clear-marks)

;; make it work in vim-mode normal state
(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode calendar-mode))
  (list *calendar-mode-keymap*))