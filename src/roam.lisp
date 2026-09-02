(defpackage :organ/roam
  (:use :cl)
  (:export
   :*organ-files*
   :*roam-cache-enabled* :roam-cache-invalidate
   :*roam-cache-auto-rescan* :*roam-cache-rescan-interval*
   :*roam-titles-include-id* :roam-rescan :roam-cache-auto-rescan-toggle
   :current-roamer))

(in-package :organ/roam)

(defvar *organ-files*
  nil
  "a list of rules according to which to look for and parse files. see `cltpt/roam:find-files'.")

(defvar *roam-cache-enabled*
  t
  "when non-nil, parsed roam nodes are kept in memory instead of rescanned on every command.")

(defvar *roam-cache*
  nil
  "the cached `cltpt/roam:roamer', or nil.")

(defvar *roam-rescan-in-progress*
  nil
  "whether a background rescan is running.")

(defvar *roam-cache-auto-rescan*
  t
  "when non-nil, the roam files are rescanned periodically instead of only on demand.")

(defvar *roam-cache-rescan-interval*
  30
  "seconds between periodic background rescans.")

(defvar *roam-cache-rescan-timer*
  nil
  "the repeating timer for the periodic rescans, or nil if they are off.")

(defvar *roam-titles-include-id*
  nil
  "when non-nil, include a node's id as an extra searchable title in `roam-find'.")

(defun roam-cache-invalidate ()
  "drop the cached roamer so the next command rescans."
  (setf *roam-cache* nil))

(defun start-roam-rescan (&optional callback)
  "scan `*organ-files*' in a thread, then install the result as the cache.
a fresh roamer is built rather than using `cltpt/roam:roamer-rescan' on the cached one.
CALLBACK runs on the editor thread with the new roamer. returns nil if a scan was already running."
  (unless *roam-rescan-in-progress*
    (setf *roam-rescan-in-progress* t)
    (bt2:make-thread
     (lambda ()
       (let ((rmr)
             (failure))
         (handler-case
             (setf rmr (cltpt/roam:roamer-from-files *organ-files*))
           (error (e)
             (setf failure (princ-to-string e))))
         (lem:send-event
          (lambda ()
            (setf *roam-rescan-in-progress* nil)
            (if rmr
                (progn
                  (setf *roam-cache* rmr)
                  (when callback
                    (funcall callback rmr)))
                (lem:message "roam rescan failed: ~A" failure))))))
     :name "organ-roam-rescan")
    t))

(defun stop-roam-cache-rescan ()
  (when *roam-cache-rescan-timer*
    (lem:stop-timer *roam-cache-rescan-timer*)
    (setf *roam-cache-rescan-timer* nil)))

(defun start-roam-cache-rescan ()
  "(re)start the timer that rescans the roam files every `*roam-cache-rescan-interval*' seconds.
the timer function runs on the editor thread, it starts the scan on its own thread."
  (stop-roam-cache-rescan)
  (setf *roam-cache-auto-rescan* t)
  (setf *roam-cache-rescan-timer*
        (lem:start-timer
         (lem:make-timer
          (lambda ()
            (when (and *organ-files* *roam-cache-enabled*)
              (start-roam-rescan)))
          :name "organ-roam-cache-rescan")
         (* 1000 *roam-cache-rescan-interval*)
         :repeat t)))

(defun ensure-roam-cache-rescan ()
  (when (and *roam-cache-auto-rescan* (null *roam-cache-rescan-timer*))
    (start-roam-cache-rescan)))

(lem:define-command roam-cache-auto-rescan-toggle () ()
  "turn the periodic background rescans on or off."
  (if *roam-cache-rescan-timer*
      (progn
        (stop-roam-cache-rescan)
        (setf *roam-cache-auto-rescan* nil)
        (lem:message "roam cache auto rescan off."))
      (progn
        (start-roam-cache-rescan)
        (lem:message "rescanning the roam files every ~A seconds."
                     *roam-cache-rescan-interval*))))

(defun current-roamer ()
  "return a roamer for `*organ-files*'.
the cache is kept up to date by the rescan timer, so this only blocks when there is nothing
usable to hand back yet."
  (ensure-roam-cache-rescan)
  (if (and *roam-cache-enabled*
           *roam-cache*
           (equal (cltpt/roam:roamer-files *roam-cache*) *organ-files*))
      *roam-cache*
      (let ((rmr (cltpt/roam:roamer-from-files *organ-files*)))
        (when *roam-cache-enabled*
          (setf *roam-cache* rmr))
        rmr)))

(lem:define-command roam-rescan () ()
  "rescan the roam files in the background, replacing the cache when done."
  (if *organ-files*
      (if (start-roam-rescan
           (lambda (rmr)
             (lem:message "rescanned ~A roam nodes."
                          (length (cltpt/roam:roamer-nodes rmr)))))
          (lem:message "rescanning roam files...")
          (lem:message "a roam rescan is already running."))
      (lem:message "you must customize *organ-files* first.")))