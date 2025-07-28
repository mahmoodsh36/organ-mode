(defsystem "organ-mode"
  :description "organ-mode - an alternative to org-mode for lem."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("cltpt")
  :components ((:module "agenda"
                :pathname "src/"
                :components ((:file "agenda")))
               (:module "base"
                :pathname "src/"
                :depends-on ("agenda")
                :components ((:file "organ-mode")
                             (:file "highlighting")))))