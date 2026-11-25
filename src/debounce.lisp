(in-package :organ/utils)

(defstruct debounce-state
  timer
  last-call)

(defun ms-since (internal-time)
  (round (* 1000 (- (get-internal-real-time) internal-time))
         internal-time-units-per-second))

(defun start-debounce-timer (buffer state delay fn wait)
  (setf (debounce-state-timer state)
        (lem:start-timer
         (lem:make-timer
          (lambda ()
            (setf (debounce-state-timer state) nil)
            (when (member buffer (lem:buffer-list))
              (let ((remaining (- delay (ms-since (debounce-state-last-call state)))))
                (if (plusp remaining)
                    (start-debounce-timer buffer state delay fn remaining)
                    (funcall fn buffer)))))
          :name "organ-debounce")
         wait
         :repeat nil)))

(defun debounce (buffer key delay fn)
  "call FN with BUFFER once DELAY ms pass without another call for KEY."
  (let ((state (or (lem:buffer-value buffer key)
                   (setf (lem:buffer-value buffer key) (make-debounce-state)))))
    (setf (debounce-state-last-call state) (get-internal-real-time))
    (unless (debounce-state-timer state)
      (start-debounce-timer buffer state delay fn delay))))
