(defpackage :organ/agenda
  (:use :cl :lem)
  (:export :show-agenda-buffer))
(in-package :organ/agenda)

(defvar *days-to-show*
  14
  "number of days to be in the organ-agenda buffer by default.")

(defun start-of-day (timestamp)
  "returns a new timestamp set to the beginning of the day (00:00:00)."
  (local-time:adjust-timestamp timestamp
    (:set :hour 0) (:set :minute 0) (:set :sec 0) (:set :nsec 0)))

(defun get-item-timestamp (item)
  "returns a timestamp for an agenda item for sorting."
  (etypecase item
    (cltpt/agenda:todo
     (or (cltpt/agenda:todo-scheduled item)
         (cltpt/agenda:todo-deadline item)
         (cltpt/agenda:todo-timed item)))
    (cons (getf item :timestamp))))

(defun same-day-p (ts1 ts2)
  "checks if two timestamps fall on the same calendar day."
  (and ts1 ts2
       (= (local-time:timestamp-year ts1) (local-time:timestamp-year ts2))
       (= (local-time:timestamp-month ts1) (local-time:timestamp-month ts2))
       (= (local-time:timestamp-day ts1) (local-time:timestamp-day ts2))))

(defun collect-items-for-day (todos current-day is-overdue-collection-day-p today)
  "collects all relevant todo items for a single day."
  (loop for todo in todos
        for todo-ts = (get-item-timestamp todo)
        when (and todo-ts
                  (or ;; always include items scheduled for the current day.
                   (same-day-p todo-ts current-day)
                   ;; if this is the designated "overdue collection day",
                   ;; also include all items from before today.
                   (and is-overdue-collection-day-p
                        (local-time:timestamp< todo-ts today))))
          collect todo))

(defun generate-timeline-markers (now current-day-start)
  "generates the 'now' marker and fixed hourly timeline markers."
  (cons (list :type :timeline-marker
              :time-str (local-time:format-timestring
                         nil now :format
                         '((:hour 2) ":" (:min 2)))
              :is-now t :timestamp now)
        (loop for hour in '(8 10 12 14 16 18 20 22)
              collect (list :type :timeline-marker
                            :time-str (format nil "~2,'0d:00" hour)
                            :timestamp (local-time:adjust-timestamp current-day-start
                                         (:set :hour hour))))))

(defun sort-day-items (items today)
  "sorts items for a single day: timeline first, then overdue items."
  (sort items
        (lambda (a b)
          (let* ((a-is-timed (or (consp a)
                                 (and (typep a 'cltpt/agenda:todo)
                                      (cltpt/agenda:todo-scheduled a))))
                 (b-is-timed (or (consp b)
                                 (and (typep b 'cltpt/agenda:todo)
                                      (cltpt/agenda:todo-scheduled b)))))
            (if (eq a-is-timed b-is-timed)
                (if a-is-timed
                    (local-time:timestamp< (get-item-timestamp a)
                                           (get-item-timestamp b))
                    (let ((age-a (if (and (get-item-timestamp a)
                                          (local-time:timestamp<
                                           (get-item-timestamp a)
                                           today))
                                     (local-time:timestamp-difference
                                      today
                                      (get-item-timestamp a))
                                     0))
                          (age-b (if (and
                                      (get-item-timestamp b)
                                      (local-time:timestamp<
                                       (get-item-timestamp b)
                                       today))
                                     (local-time:timestamp-difference
                                      today
                                      (get-item-timestamp b))
                                     0)))
                      (> age-a age-b)))
                a-is-timed)))))

(defun prepare-agenda-display-list (todos begin-date end-date)
  "generates the complete list of items to display for a given date range."
  (let* ((now (local-time:now))
         (today (start-of-day now))
         ;; determine the single day on which to display all overdue items.
         (overdue-collection-day (if (local-time:timestamp< begin-date today)
                                     today
                                     begin-date)))
    (loop
      for current-day = begin-date then (local-time:timestamp+ current-day 1 :day)
      while (local-time:timestamp<= current-day end-date)
      appending
      (let* ((is-today-p (same-day-p current-day today))
             (is-overdue-day-p (same-day-p current-day overdue-collection-day))
             (day-items (collect-items-for-day todos current-day is-overdue-day-p today))
             (timeline-markers
               (if is-today-p
                   (generate-timeline-markers now current-day)
                   nil))
             (all-day-items (append day-items timeline-markers)))
        (cons (list :type :day-header :date current-day)
              (sort-day-items all-day-items today))))))

(defgeneric draw-agenda-item (item point)
  (:documentation "generic function to draw an agenda item at a given point.")
  (:method :after (item point)
    (insert-character point #\newline)))

(defun draw-day-header (item point)
  "draws the main date header for a given day."
  (let* ((timestamp (getf item :date))
         (day-name (local-time:format-timestring
                    nil
                    timestamp
                    :format '(:long-weekday)))
         (date-str (local-time:format-timestring
                    nil
                    timestamp
                    :format '(:day " " :long-month " " :year)))
         (week-num (nth-value 1 (local-time::%timestamp-decode-iso-week timestamp)))
         (week-str (if (= (local-time:timestamp-day-of-week timestamp) 1)
                       (format nil " W~A" week-num)
                       "")))
    (insert-string point
                   (format nil "~A~A ~A~A"
                           day-name
                           (make-string (max 0 (- 11 (length day-name)))
                                        :initial-element #\space)
                           date-str
                           week-str)
                   :attribute 'title-attribute)))

(defun draw-timeline-marker (item point)
  "draws a timeline marker, using a different highlight for the 'now' line."
  (let ((time-str (getf item :time-str))
        (is-now (getf item :is-now)))
    (if is-now
        (insert-string point (format nil "              ~A ┄┄┄┄┄ ← now" time-str)
                       :attribute 'syntax-warning-attribute)
        (insert-string point (format nil "              ~A ┄┄┄┄┄ ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"
                                     time-str)
                       :attribute 'syntax-comment-attribute))))

(defmethod draw-agenda-item ((item cons) point)
  "draws non-todo items like headers and markers."
  (case (getf item :type)
    (:day-header (draw-day-header item point))
    (:timeline-marker (draw-timeline-marker item point))))

(defmethod draw-agenda-item ((item cltpt/agenda:todo) point)
  "draws a single todo item, applying syntax highlighting to each part."
  (let* ((state (cltpt/agenda:todo-state item))
         (title (cltpt/agenda:todo-title item))
         (tags (cltpt/agenda:todo-tags item))
         (scheduled (cltpt/agenda:todo-scheduled item))
         (deadline (cltpt/agenda:todo-deadline item))
         (timed (cltpt/agenda:todo-timed item))
         (start-prop (copy-point point :temporary))
         (todo-ts (or scheduled deadline timed))
         (today-start (start-of-day (local-time:now)))
         (days-ago (when (and todo-ts (local-time:timestamp< todo-ts today-start))
                     (floor (/ (local-time:timestamp-difference today-start todo-ts)
                               86400))))
         (time-str (when scheduled
                     (local-time:format-timestring
                      nil
                      scheduled
                      :format '((:hour 2) ":" (:min 2)))))
         (prefix (cond (days-ago (format nil " ~A d. ago:  " days-ago))
                       (scheduled (format nil "              ~A ┄┄┄┄┄ " time-str))
                       (deadline "  Deadline:   ")
                       (t "              ")))
         (tag-str (if tags (format nil ":~{~A:~}:" tags) "")))
    (insert-string point prefix :attribute 'syntax-comment-attribute)
    (insert-string point
                   (format nil "~A " state)
                   :attribute
                   (cond ((string-equal state "TODO") 'syntax-warning-attribute)
                         ((string-equal state "DONE") 'syntax-string-attribute)
                         (t 'syntax-keyword-attribute)))
    (insert-string point title)
    (when tags
      (insert-string point
                     (format nil " ~A" tag-str)
                     :attribute 'syntax-comment-attribute))
    (put-text-property start-prop point :agenda-item item)))

(defun show-agenda-view (agenda begin-date end-date)
  (let* ((buffer-name "*organ-agenda*")
         (buffer (make-buffer buffer-name :enable-undo-p nil :read-only-p t))
         (todos (cltpt/agenda:agenda-todos agenda)))
    ;; store all necessary state as independent buffer-local variables.
    (setf (buffer-value buffer 'agenda) agenda)
    (setf (buffer-value buffer 'begin-date) (start-of-day begin-date))
    (setf (buffer-value buffer 'end-date) (start-of-day end-date))
    ;; call the drawing function.
    (draw-agenda-to-buffer buffer
                           todos
                           (start-of-day begin-date)
                           (start-of-day end-date))
    ;; set up the buffer's mode and switch to it.
    (change-buffer-mode buffer 'organ-agenda-mode)
    (switch-to-buffer buffer)))

(defvar *organ-agenda-mode-keymap*
  (make-keymap :name '*organ-agenda-mode-keymap* :parent *global-keymap*))

(define-major-mode organ-agenda-mode ()
  (:name "organ-agenda"
   :keymap *organ-agenda-mode-keymap*))

(defun draw-agenda-to-buffer (buffer todos begin-date end-date)
  "wipes and redraws the agenda view for a given date range."
  (let ((point (buffer-point buffer)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (let ((items-to-draw (prepare-agenda-display-list todos begin-date end-date)))
        (dolist (item items-to-draw)
          (draw-agenda-item item point))))
    (buffer-start point)))

(define-command organ-agenda-reload () ()
  "redraws the agenda buffer."
  (let* ((buffer (current-buffer))
         (agenda (buffer-value buffer 'agenda))
         (begin-date (buffer-value buffer 'begin-date))
         (end-date (buffer-value buffer 'end-date)))
    (let* ((todos (cltpt/agenda:agenda-todos agenda)))
      (draw-agenda-to-buffer buffer todos begin-date end-date)
      (message "reloaded agenda."))))

(define-command organ-agenda-quit () ()
  (delete-buffer (current-buffer)))

(define-key *organ-agenda-mode-keymap* "g" 'organ-agenda-reload)
(define-key *organ-agenda-mode-keymap* "q" 'organ-agenda-quit)

(defun show-agenda-buffer (rmr)
  "takes an instance of cltpt/roam:roamer RMR, opens the agenda buffer."
  (let* ((agenda (cltpt/agenda:from-roamer rmr))
         (start-date (local-time:now))
         (end-date (local-time:timestamp+ start-date *days-to-show* :day)))
    (show-agenda-view agenda start-date end-date)))