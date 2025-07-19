(defsystem "organ-mode"
  :description "organ-mode - an alternative to org-mode for lem."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("cltpt")
  :components ((:module "base"
                :pathname "src/"
                :components ((:file "organ-mode")
                             (:file "syntax")))))