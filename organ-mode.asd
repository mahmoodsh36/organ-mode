(defsystem "organ-mode"
  :description "organ-mode - an alternative to org-mode for lem."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("cltpt" "local-time")
  :components ((:module "utils"
                :pathname "src/"
                :components ((:file "utils")))
               (:module "outline-mode"
                :pathname "src/"
                :components ((:file "outline-mode")))
               (:module "calendar-mode"
                :pathname "src/"
                :components ((:file "calendar-mode")))
               (:module "agenda"
                :pathname "src/"
                :components ((:file "agenda-mode")))
               (:module "organ-mode"
                :pathname "src/"
                :depends-on ("utils" "calendar-mode")
                :components ((:file "organ-mode")
                             (:file "highlighting")))
               (:module "organ"
                :pathname "src/"
                :depends-on ("organ-mode")
                :components ((:file "organ")))))