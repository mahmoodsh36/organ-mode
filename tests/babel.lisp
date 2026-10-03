(defpackage :organ-mode-tests/babel
  (:use :cl :rove :organ-mode-tests)
  (:import-from
   :lem-fake-interface
   :with-fake-interface))

(in-package :organ-mode-tests/babel)

(register-test-suite :organ-mode-tests/babel)

(defun setup-babel-buffer (text)
  "open TEXT in organ-mode with the cursor on its first src block."
  (let ((buffer (lem:make-buffer (make-test-buf-name))))
    (lem:switch-to-buffer buffer)
    (lem:insert-string (lem:buffer-point buffer) text)
    (organ/organ-mode:organ-mode)
    (lem:move-point (lem:current-point)
                    (organ/utils:char-offset-to-point buffer (search "#+begin_src" text)))
    buffer))

(defun wait-for-babel (buffer)
  "process editor events until BUFFER changes or an error buffer shows up."
  (let ((before (lem:buffer-text buffer)))
    (loop repeat 100
          until (or (string/= before (lem:buffer-text buffer))
                    (lem:get-buffer "*organ-babel-errors*"))
          do (lem-core::receive-event 0.2))))

(defun run-babel (text &optional during-run)
  "execute the first block in TEXT, calling DURING-RUN on the buffer before results land."
  (let ((buffer (setup-babel-buffer text)))
    (organ/organ-mode:organ-babel-execute-src-block)
    (when during-run
      (funcall during-run buffer))
    (wait-for-babel buffer)
    buffer))

(deftest babel-execute
  "src block evaluation tests."
  (lem:with-current-buffers ()
    (with-fake-interface ()
      ;; lem:message needs a timer manager, which the fake interface does not set up.
      (lem/common/timer:with-timer-manager (make-instance 'lem/common/timer:timer-manager)
        (testing "value results are inserted after the block"
          (check-buffer
           "insert"
           (run-babel "#+begin_src python :results value
return [1, 2]
#+end_src
after")
           "#+begin_src python :results value
return [1, 2]
#+end_src

#+RESULTS:
[1, 2]
after"))
        (testing "table results"
          (check-buffer
           "table"
           (run-babel "#+begin_src python :results value table
return [1, 2]
#+end_src")
           "#+begin_src python :results value table
return [1, 2]
#+end_src

#+RESULTS:
| 1 | 2 |"))
        (testing "results follow the block when text is inserted at its start mid-run"
          (check-buffer
           "edited"
           (run-babel "top
#+begin_src python :results output
import time
time.sleep(0.5)
print('hi')
#+end_src"
                      (lambda (buffer)
                        (lem:insert-string (organ/utils:char-offset-to-point
                                            buffer
                                            (search "#+begin_src" (lem:buffer-text buffer)))
                                           (format nil "new line~%"))))
           "top
new line
#+begin_src python :results output
import time
time.sleep(0.5)
print('hi')
#+end_src

#+RESULTS:
hi
"))
        (testing "stderr goes to the errors buffer and no results are inserted"
          (let ((text "#+begin_src python
print(undefined)
#+end_src"))
            (check-buffer "error" (run-babel text) text)
            (let ((errors (lem:get-buffer "*organ-babel-errors*")))
              (ok (and errors (search "NameError" (lem:buffer-text errors)))
                  "traceback in errors buffer")
              (when errors
                (lem:delete-buffer errors)))))
        (testing "unsupported language"
          (setup-babel-buffer "#+begin_src ruby
puts 1
#+end_src")
          (ok (signals (organ/organ-mode:organ-babel-execute-src-block) 'lem:editor-error)))))))