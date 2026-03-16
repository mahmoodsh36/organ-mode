(asdf:load-system "lem")
(asdf:load-system "lem-fake-interface")
(asdf:load-system "rove")
(asdf:load-system "organ-mode-tests")

(organ-mode-tests:run-all-suites)

(uiop:quit)