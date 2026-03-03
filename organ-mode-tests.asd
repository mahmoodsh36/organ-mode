(defsystem "organ-mode-tests"
  :depends-on ("lem-fake-interface"
               "lem"
               "organ-mode"
               "rove")
  :pathname "tests"
  :components ((:file "tests")))