(defsystem "organ-mode-tests"
  :depends-on ("lem-fake-interface"
               "lem"
               "organ-mode"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "utils")
               (:file "tests")
               (:file "move")
               (:file "org-list")
               (:file "org-table")
               (:file "nav")))
