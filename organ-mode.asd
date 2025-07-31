(defsystem "organ-mode"
  :description "organ-mode - an alternative to org-mode for lem."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("cltpt" "local-time")
  :components ((:module "utils"
                :pathname "src/"
                :components ((:file "utils")))
               (:module "agenda"
                :pathname "src/"
                :components ((:file "agenda")))
               (:module "organ-mode"
                :pathname "src/"
                :depends-on ("utils")
                :components ((:file "organ-mode")
                             (:file "highlighting")))
               (:module "organ"
                :pathname "src/"
                :depends-on ("agenda" "organ-mode")
                :components ((:file "organ")))))