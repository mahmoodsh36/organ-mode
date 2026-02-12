(defpackage :organ/popup-calendar
  (:use :cl :lem)
  (:export :popup-calendar :popup-calendar-with-callback :test-popup-calendar))

(in-package :organ/popup-calendar)

(define-editor-variable popup-calendar-date nil)
(define-editor-variable popup-calendar-callback nil)
(define-editor-variable popup-calendar-source-buffer nil)
(define-editor-variable popup-calendar-window nil)
(define-editor-variable popup-calendar-date nil)

;; these variables are not buffer-isolated but the intended workflow
;; doenst require that anyway.
(defvar *popup-calendar-active* nil)
(defvar *popup-calendar-current-buffer* nil)

(defvar *popup-calendar-prompt-keymap*
  (make-keymap :description '*popup-calendar-prompt-keymap*))

(defparameter +popup-calendar-buffer-name+ "*popup-calendar*")
(defvar *calendar-grid-width* 3)
(defvar *calendar-grid-height* 1)
(defvar *popup-window-width* 95)
(defvar *popup-window-height* 8)

(defparameter *weekday-name-map*
  '(("sun" . 0)
    ("mon" . 1)
    ("tue" . 2)
    ("wed" . 3)
    ("thu" . 4)
    ("fri" . 5)
    ("sat" . 6)))

(defun parse-time-part (time-string)
  "parse time string like '10:00'. returns (values hour minute) or nil."
  (when (and time-string (not (string= time-string "")))
    (let ((colon-pos (position #\: time-string)))
      (when colon-pos
        (let* ((hour-str (subseq time-string 0 colon-pos))
               (min-str (subseq time-string (1+ colon-pos)))
               (hour (parse-integer hour-str :junk-allowed t))
               (minute (parse-integer min-str :junk-allowed t)))
          (when (and hour minute (<= 0 hour 23) (<= 0 minute 59))
            (values hour minute)))))))

(defun parse-relative-date (date-part)
  "parse relative date like +2d, +1w. returns timestamp or nil."
  (when (and (> (length date-part) 1)
             (char= (char date-part 0) #\+)
             (digit-char-p (char date-part 1)))
    (let* ((num-str (subseq date-part 1))
           (num (parse-integer num-str :junk-allowed t))
           (unit (if (> (length date-part) 2) (subseq date-part 2) "d")))
      (when num
        (let ((unit-keyword (case (aref unit 0)
                              (#\d :day)
                              (#\w :week)
                              (#\m :month)
                              (#\y :year))))
          (when unit-keyword
            (local-time:timestamp+ (local-time:now) num unit-keyword)))))))

(defun parse-weekday-name (date-part)
  "parse weekday name like 'sat', 'monday'. returns next occurrence timestamp or nil."
  (when (and (>= (length date-part) 3)
             (every #'alpha-char-p date-part))
    (let* ((day-name (string-downcase date-part))
           (prefix (subseq day-name 0 3))
           (entry (assoc prefix *weekday-name-map* :test #'string-equal))
           (target-dow (cdr entry)))
      (when target-dow
        (let* ((now (local-time:now))
               (current-dow (local-time:timestamp-day-of-week now))
               (days-ahead (mod (- target-dow current-dow) 7)))
          (when (zerop days-ahead) (setf days-ahead 7))
          (local-time:timestamp+ now days-ahead :day))))))

(defun parse-day-of-month (date-part)
  "parse day of month like '19'. returns timestamp for current or next month, or nil."
  (when (every #'digit-char-p date-part)
    (let* ((day (parse-integer date-part))
           (now (local-time:now))
           (current-month (local-time:timestamp-month now))
           (current-year (local-time:timestamp-year now))
           (days-in-month (local-time:days-in-month current-month current-year)))
      (when (and day (<= 1 day 31))
        (let ((try-date (ignore-errors
                         (local-time:encode-timestamp 0 0 0 0 day current-month current-year))))
          (cond
            ((and try-date (local-time:timestamp>= try-date now))
             try-date)
            (t
             ;; try next month
             (let* ((next-month (if (= current-month 12) 1 (1+ current-month)))
                    (next-year (if (= current-month 12) (1+ current-year) current-year))
                    (next-days (local-time:days-in-month next-month next-year)))
               (when (<= day next-days)
                 (local-time:encode-timestamp 0 0 0 0 day next-month next-year))))))))))

(defun parse-full-date (date-part)
  "parse date like 15/05 or 15/05/2023. returns timestamp or nil."
  (when (find #\/ date-part)
    (let* ((parts (uiop:split-string date-part :separator "/"))
           (day (parse-integer (first parts) :junk-allowed t))
           (month (parse-integer (second parts) :junk-allowed t))
           (year (when (>= (length parts) 3)
                   (parse-integer (third parts) :junk-allowed t))))
      (when (and day month (<= 1 day 31) (<= 1 month 12))
        (let ((final-year (or year (local-time:timestamp-year (local-time:now)))))
          (when (>= final-year 1900)
            (local-time:encode-timestamp 0 0 0 0 day month final-year)))))))

(defun parse-date-time-input (input-string)
  "parse date-time input. Returns a local-time timestamp or nil.

supported formats:
- relative: +1d, +2w, +1m, +1y.
- weekday: sat, mon, thursday (next occurrence).
- day of month: 19 (current or next month).
- full date: 15/05, 15/05/2023.
- time: [date] 10:00"
  (when (and input-string (not (string= input-string "")))
    (let* ((trimmed (string-trim '(#\space #\tab) input-string))
           (parts (uiop:split-string trimmed :separator " "))
           (date-part (first parts))
           (time-part (second parts))
           (base-date (or (parse-relative-date date-part)
                          (parse-weekday-name date-part)
                          (parse-day-of-month date-part)
                          (parse-full-date date-part))))
      (if (and base-date time-part)
          (multiple-value-bind (hour minute) (parse-time-part time-part)
            (if hour
                (local-time:encode-timestamp 0
                                             0
                                             minute hour
                                             (local-time:timestamp-day base-date)
                                             (local-time:timestamp-month base-date)
                                             (local-time:timestamp-year base-date))
                base-date))
          base-date))))

(defun update-calendar-state (buffer target-date)
  "update calendar display, move to date, and refresh highlights in buffer."
  (with-current-buffer buffer
    (organ/calendar-mode:update-calendar-display buffer)
    (when target-date
      (organ/calendar-mode:move-to-date target-date))
    (organ/calendar-mode:refresh-calendar-highlights)))

(defun navigate-popup-calendar (direction)
  "navigate popup calendar by one day in DIRECTION (:next or :previous)."
  (when *popup-calendar-active*
    (let ((buffer *popup-calendar-current-buffer*))
      (when buffer
        (let* ((current-date (or (variable-value 'popup-calendar-date :buffer buffer)
                                 (local-time:now)))
               (offset (if (eq direction :next) 1 -1))
               (target-date (local-time:timestamp+ current-date offset :day)))
          (setf (variable-value 'popup-calendar-date :buffer buffer)
                target-date)
          (update-calendar-state buffer target-date))))))

(defun cleanup-popup-calendar (&optional (buffer *popup-calendar-current-buffer*))
  "clean up popup calendar resources."
  (when buffer
    (let ((window (variable-value 'popup-calendar-window :buffer buffer)))
      (when window
        (delete-window window))
      (organ/calendar-mode:calendar-cleanup buffer)
      (when (get-buffer "*popup-calendar*")
        (kill-buffer buffer))))
  (setf *popup-calendar-active* nil)
  (setf *popup-calendar-current-buffer* nil)
  ;; also exit the current prompt if active
  (when (lem/prompt-window:current-prompt-window)
    (lem/prompt-window::exit-prompt (current-window))))

(defun update-calendar-highlight-from-input (input)
  "update calendar highlight based on current input."
  (let ((final-date (when (and input (not (string= input "")))
                      (parse-date-time-input input))))
    (let ((buffer *popup-calendar-current-buffer*))
      (when buffer
        (setf (variable-value 'popup-calendar-date :buffer buffer)
              final-date)
        (if final-date
            (progn
              ;; Use move-to-date to handle grid shifting when needed during typing
              (with-current-buffer buffer
                (organ/calendar-mode:move-to-date final-date)))
            ;; reset to today if no valid input
            (let ((today (local-time:now)))
              (with-current-buffer buffer
                (organ/calendar-mode:move-to-date today))))))
    final-date))

(defun create-popup-calendar-buffer ()
  "create the calendar buffer for popup display."
  (let ((buffer (make-buffer +popup-calendar-buffer-name+
                             :enable-undo-p nil
                             :temporary t)))
    (with-buffer-read-only buffer nil
      (setf (variable-value 'organ/calendar-mode:calendar-grid-width :buffer buffer)
            *calendar-grid-width*)
      (setf (variable-value 'organ/calendar-mode:calendar-grid-height :buffer buffer)
            *calendar-grid-height*)
      (setf (variable-value 'organ/calendar-mode:calendar-marked-dates :buffer buffer)
            nil)
      (setf (variable-value 'organ/calendar-mode:calendar-date :buffer buffer)
            (local-time:now))
      (setf (variable-value 'popup-calendar-date :buffer buffer)
            (local-time:now))
      (organ/calendar-mode:update-calendar-display buffer))
    buffer))

(defun create-calendar-window (buffer)
  "create and center the floating window for the calendar."
  (let* ((display-width (display-width))
         (display-height (display-height))
         (x (- (floor display-width 2) (floor *popup-window-width* 2)))
         (y (- (floor display-height 2) (floor *popup-window-height* 2))))
    (make-floating-window
     :buffer buffer
     :x x
     :y y
     :width *popup-window-width*
     :height *popup-window-height*
     :use-border t)))

(defun popup-calendar-input-callback (input)
  "callback for prompt input changes."
  (when *popup-calendar-active*
    (let ((buffer *popup-calendar-current-buffer*))
      (when buffer
        (update-calendar-highlight-from-input input)
        (update-calendar-state
         buffer
         (variable-value 'popup-calendar-date :buffer buffer))))))

(define-command popup-calendar-next-day () ()
  "navigate to next day."
  (navigate-popup-calendar :next))

(define-command popup-calendar-previous-day () ()
  "navigate to previous day and update prompt."
  (navigate-popup-calendar :previous))

(define-command popup-calendar-quit () ()
  "quit popup calendar and clean up."
  (when *popup-calendar-active*
    (cleanup-popup-calendar)))

(defun popup-calendar-with-callback (prompt-msg callback &optional initial-date)
  "open a popup calendar wih CALLBACK. INITIAL-DATE can be a timestamp or nil."
  (let ((source-buffer (current-buffer))
        (calendar-buffer (create-popup-calendar-buffer)))
    (setf (variable-value 'popup-calendar-callback :buffer calendar-buffer)
          callback
          (variable-value 'popup-calendar-source-buffer :buffer calendar-buffer)
          source-buffer
          *popup-calendar-current-buffer*
          calendar-buffer)
    (let ((calendar-window (create-calendar-window calendar-buffer)))
      (setf (variable-value 'popup-calendar-window :buffer calendar-buffer)
            calendar-window
            *popup-calendar-active*
            t)
      (lem-core::add-floating-window (current-frame) calendar-window)
      (unwind-protect
           (progn
             (when initial-date
               (setf (variable-value 'organ/calendar-mode:calendar-date :buffer calendar-buffer)
                     initial-date
                     (variable-value 'popup-calendar-date :buffer calendar-buffer)
                     initial-date)
               (update-calendar-state calendar-buffer initial-date))
             (update-calendar-state
              calendar-buffer
              (variable-value 'popup-calendar-date :buffer calendar-buffer))
             (let* ((cancelled nil)
                    (result (handler-case
                                (prompt-for-string
                                 prompt-msg
                                 :initial-value ""
                                 :edit-callback #'popup-calendar-input-callback
                                 :special-keymap *popup-calendar-prompt-keymap*)
                              (editor-abort ()
                                (setf cancelled t)
                                nil))))
               (setf *popup-calendar-active* nil
                     *popup-calendar-current-buffer* nil)
               (if cancelled
                   (progn
                     (when callback (funcall callback nil))
                     nil)
                   (let* ((navigated-date (when (buffer-name calendar-buffer)
                                            (variable-value 'popup-calendar-date
                                                            :buffer calendar-buffer)))
                          (final-date (or navigated-date
                                          (parse-date-time-input result))))
                     (when callback
                       (funcall callback final-date))
                     final-date))))
        (cleanup-popup-calendar calendar-buffer)))))

(define-command popup-calendar () ()
  "open a popup calendar without callback."
  (popup-calendar-with-callback
   (lambda (date)
     (when date
       (message "selected date: ~A"
                (local-time:format-timestring
                 nil
                 date
                 :format '(:long-month " " (:day 2) ", " (:year 4))))))))

(define-key *popup-calendar-prompt-keymap* "C-n" 'popup-calendar-next-day)
(define-key *popup-calendar-prompt-keymap* "C-p" 'popup-calendar-previous-day)
(define-key *popup-calendar-prompt-keymap* "Escape" 'popup-calendar-quit)