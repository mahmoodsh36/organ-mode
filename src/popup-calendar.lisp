(defpackage :organ/popup-calendar
  (:use :cl :lem)
  (:export :popup-calendar :popup-calendar-with-callback :test-popup-calendar))

(in-package :organ/popup-calendar)

(define-editor-variable *popup-calendar-date* nil)
(define-editor-variable *popup-calendar-callback* nil)
(define-editor-variable *popup-calendar-source-buffer* nil)
(define-editor-variable *popup-calendar-window* nil)
(define-editor-variable *popup-calendar-parsed-date* nil)

;; these variables are not buffer-isolated but the intended workflow
;; doenst require that anyway.
(defvar *popup-calendar-active* nil)
(defvar *popup-calendar-current-buffer* nil)

(defvar *popup-calendar-prompt-keymap*
  (make-keymap :description '*popup-calendar-prompt-keymap*))

(defun parse-date-time-input (input-string)
  "parse date-time input like '+2d', 'sat', '19', 'sat 10:00', '19 20:00' etc.
returns a local-time timestamp or nil if parsing fails."
  (when (and input-string (not (string= input-string "")))
    (let* ((trimmed (string-trim '(#\space #\tab) input-string))
           (parts (uiop:split-string trimmed :separator " "))
           (date-part (first parts))
           (time-part (second parts)))
      (flet ((parse-time (time-string)
               (when (and time-string (not (string= time-string "")))
                 (let ((colon-pos (position #\: time-string)))
                   (when colon-pos
                     (let* ((hour-str (subseq time-string 0 colon-pos))
                            (min-str (subseq time-string (1+ colon-pos)))
                            (hour (parse-integer hour-str :junk-allowed t))
                            (minute (parse-integer min-str :junk-allowed t)))
                       (when (and hour
                                  minute
                                  (>= hour 0)
                                  (<= hour 23)
                                  (>= minute 0)
                                  (<= minute 59))
                         (values hour minute))))))))
        (let ((base-date))
          (cond
            ;; relative dates like +2d, +1w, +3m
            ((and (> (length date-part) 1)
                  (char= (char date-part 0) #\+)
                  (digit-char-p (char date-part 1)))
             (let* ((num-str (subseq date-part 1))
                    (num (parse-integer num-str :junk-allowed t))
                    (unit (if (> (length date-part) 2)
                              (subseq date-part 2)
                              "d")))
               (when num
                 (let ((unit-keyword (case (aref unit 0)
                                       (#\d :day)
                                       (#\w :week)
                                       (#\m :month)
                                       (#\y :year)
                                       (otherwise nil))))
                   (when unit-keyword
                     (setf base-date
                           (local-time:timestamp+ (local-time:now)
                                                  num
                                                  unit-keyword)))))))
            ;; day names like 'sat', 'sun', 'mon', etc.
            ((and (>= (length date-part) 3)
                  (every #'alpha-char-p date-part))
             (let* ((day-name (string-downcase date-part))
                    (target-dow
                      (cond
                        ;; sunday = 6 in local-time (0=Mon)
                        ((and (>= (length day-name) 3)
                              (string= (subseq day-name 0 3) "sun"))
                         6)
                        ((and (>= (length day-name) 3)
                              (string= (subseq day-name 0 3) "mon"))
                         0)
                        ((and (>= (length day-name) 3)
                              (string= (subseq day-name 0 3) "tue"))
                         1)
                        ((and (>= (length day-name) 3)
                              (string= (subseq day-name 0 3) "wed"))
                         2)
                        ((and (>= (length day-name) 3)
                              (string= (subseq day-name 0 3) "thu"))
                         3)
                        ((and (>= (length day-name) 3)
                              (string= (subseq day-name 0 3) "fri"))
                         4)
                        ((and (>= (length day-name) 3)
                              (string= (subseq day-name 0 3) "sat"))
                         5)
                        (t nil))))
               (when target-dow
                 (let* ((now (local-time:now))
                        (current-dow (local-time:timestamp-day-of-week now))
                        (days-ahead (mod (- target-dow current-dow) 7)))
                   (when (zerop days-ahead) ;; if today, use next week
                     (setf days-ahead 7))
                   (setf base-date
                         (local-time:timestamp+ now days-ahead :day))))))
            ;; day numbers like '19'
            ((every #'digit-char-p date-part)
             (let* ((day (parse-integer date-part))
                    (now (local-time:now))
                    (current-month (local-time:timestamp-month now))
                    (current-year (local-time:timestamp-year now))
                    (days-in-month (local-time:days-in-month current-month current-year)))
               (when (and day (>= day 1) (<= day 31))
                 (let ((try-date (local-time:encode-timestamp
                                  0 0 0 0 day current-month current-year)))
                   (if (and (<= day days-in-month)
                            (local-time:timestamp>= try-date now))
                       (setf base-date try-date)
                       ;; try next month if current month's day has passed
                       (let* ((next-month (if (= current-month 12)
                                              1
                                              (1+ current-month)))
                              (next-year (if (= current-month 12)
                                             (1+ current-year)
                                             current-year))
                              (next-days-in-month
                                (local-time:days-in-month next-month next-year)))
                         (when (<= day next-days-in-month)
                           (setf base-date
                                 (local-time:encode-timestamp
                                  0 0 0 0
                                  day
                                  next-month
                                  next-year)))))))))
            ;; full date like day/month/year, day/month, or dd/mm/yyyy
            ((find #\/ date-part)
             (let* ((parts (uiop:split-string date-part :separator "/"))
                    (day (parse-integer (first parts) :junk-allowed t))
                    (month (parse-integer (second parts) :junk-allowed t))
                    (year (when (>= (length parts) 3)
                            (parse-integer (third parts) :junk-allowed t))))
               (when (and day month
                          (>= day 1)
                          (<= day 31)
                          (>= month 1)
                          (<= month 12))
                 (let ((final-year (or year
                                       (local-time:timestamp-year
                                        (local-time:now)))))
                   (when (>= final-year 1900) ;; basic year validation
                     (setf base-date
                           (local-time:encode-timestamp
                            0 0 0 0
                            day
                            month
                            final-year)))))))
            (t nil))
          ;; combine with time if available
          (if (and base-date time-part)
              (multiple-value-bind (hour minute) (parse-time time-part)
                (when hour
                  (local-time:encode-timestamp
                   0 0 minute hour
                   (local-time:timestamp-day base-date)
                   (local-time:timestamp-month base-date)
                   (local-time:timestamp-year base-date))))
              base-date))))))

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
        (let ((current-date (variable-value '*popup-calendar-parsed-date*
                                            :buffer buffer)))
          (let ((target-date
                  (if current-date
                      (if (eq direction :next)
                          (local-time:timestamp+ current-date 1 :day)
                          (local-time:timestamp- current-date 1 :day))
                      (if (eq direction :next)
                          (local-time:timestamp+ (local-time:now) 1 :day)
                          (local-time:timestamp- (local-time:now) 1 :day)))))
            (setf (variable-value '*popup-calendar-parsed-date*
                                  :buffer buffer)
                  target-date)
            (update-calendar-state buffer target-date)))))))

(defun cleanup-popup-calendar (&optional (buffer *popup-calendar-current-buffer*))
  "clean up popup calendar resources."
  (when buffer
    (let ((window (variable-value '*popup-calendar-window* :buffer buffer)))
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
        (setf (variable-value '*popup-calendar-parsed-date* :buffer buffer)
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
  (let ((buffer (make-buffer "*popup-calendar*"
                             :enable-undo-p nil
                             :temporary t)))
    (with-buffer-read-only buffer nil
      (setf (variable-value 'organ/calendar-mode:*calendar-grid-width*
                            :buffer buffer)
            3)
      (setf (variable-value 'organ/calendar-mode:*calendar-grid-height*
                            :buffer buffer)
            1)
      (setf (variable-value 'organ/calendar-mode:*calendar-marked-dates*
                            :buffer buffer)
            nil)
      (setf (variable-value 'organ/calendar-mode:*calendar-date*
                            :buffer buffer)
            (local-time:now))
      (setf (variable-value '*popup-calendar-date* :buffer buffer)
            (local-time:now))
      (organ/calendar-mode:update-calendar-display buffer))
    buffer))

(defun popup-calendar-input-callback (input)
  "callback for prompt input changes."
  (when *popup-calendar-active*
    (let ((buffer *popup-calendar-current-buffer*))
      (when buffer
        (update-calendar-highlight-from-input input)
        ;; force refresh of calendar display
        (update-calendar-state
         buffer
         (variable-value '*popup-calendar-parsed-date* :buffer buffer))))))

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

(defun popup-calendar-with-callback (callback &optional initial-date)
  "open a popup calendar with a callback function.
CALLBACK is a function that receives the selected date as argument.
INITIAL-DATE can be a timestamp or nil for now."
  (let ((source-buffer (current-buffer)))
    (let ((calendar-buffer (create-popup-calendar-buffer)))
      (setf (variable-value '*popup-calendar-callback* :buffer calendar-buffer)
            callback)
      (setf (variable-value '*popup-calendar-source-buffer*
                            :buffer calendar-buffer)
            source-buffer)
      (setf *popup-calendar-current-buffer* calendar-buffer)
      ;; create popup window for calendar (centered)
      (let* ((display-width (display-width))
             (display-height (display-height))
             (window-width 95)
             (window-height 8)
             (x (- (floor display-width 2) (floor window-width 2)))
             (y (- (floor display-height 2) (floor window-height 2)))
             (calendar-window (make-floating-window
                               :buffer calendar-buffer
                               :x x
                               :y y
                               :width window-width
                               :height window-height
                               :use-border t)))
        (setf (variable-value '*popup-calendar-window* :buffer calendar-buffer)
              calendar-window)
        (setf *popup-calendar-active* t)
        ;; show window
        (lem-core::add-floating-window (current-frame) calendar-window)
        ;; set up initial calendar state (don't switch to buffer)
        (when initial-date
          (setf (variable-value 'organ/calendar-mode:*calendar-date*
                                :buffer calendar-buffer)
                initial-date)
          (setf (variable-value '*popup-calendar-date* :buffer calendar-buffer)
                initial-date)
          (update-calendar-state calendar-buffer initial-date))
        ;; refresh highlights for current state
        (update-calendar-state
         calendar-buffer
         (variable-value '*popup-calendar-parsed-date*
                         :buffer calendar-buffer))
        ;; start prompt with edit callback
        (let ((result (handler-case
                          (prompt-for-string
                           "date: "
                           :initial-value ""
                           :edit-callback #'popup-calendar-input-callback
                           :test-function (lambda (input)
                                            ;; handle escape key
                                            (when (eq input 'escape)
                                              (popup-calendar-quit)
                                              nil)
                                            t)
                           :special-keymap *popup-calendar-prompt-keymap*)
                        (error ()
                          ;; clean up
                          (popup-calendar-quit)
                          nil))))
          (setf *popup-calendar-active* nil)
          (setf *popup-calendar-current-buffer* nil)
          ;; use the navigated date if available (from C-n/C-p), otherwise parse prompt
          (let* ((navigated-date (when calendar-buffer
                                   (variable-value '*popup-calendar-parsed-date*
                                                   :buffer calendar-buffer)))
                 (final-date
                   (or navigated-date
                       ;; fallback to parsing prompt result
                       (parse-date-time-input result))))
            ;; clean up
            (cleanup-popup-calendar calendar-buffer)
            ;; call callback
            (when callback
              (if final-date
                  (funcall callback final-date)
                  (funcall callback nil)))
            final-date))))))

(define-command popup-calendar () ()
  "open a popup calendar without callback."
  (popup-calendar-with-callback
   (lambda (date)
     (message "here ~A~%" date)
     (when date
       (message "selected date: ~A"
                (local-time:format-timestring
                 nil
                 date
                 :format '(:long-month " " (:day 2) ", " (:year 4))))))))

;; keybindings - only C-n and C-p for navigation
(define-key *popup-calendar-prompt-keymap* "C-n" 'popup-calendar-next-day)
(define-key *popup-calendar-prompt-keymap* "C-p" 'popup-calendar-previous-day)
(define-key *popup-calendar-prompt-keymap* "Escape" 'popup-calendar-quit)