(defsystem "organ-mode"
  :description "organ-mode - an alternative to org-mode for lem."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("cltpt" "local-time" "lem/core" "lem-transient" "lem-vi-mode")
  :components ((:module "utils"
                :pathname "src/"
                :components ((:file "utils")))
               (:module "outline-mode"
                :pathname "src/"
                :components ((:file "outline-mode")))
               (:module "calendar-mode"
                :pathname "src/"
                :components ((:file "calendar-mode")))
               (:module "popup-calendar"
                :pathname "src/"
                :depends-on ("calendar-mode")
                :components ((:file "popup-calendar")))
               (:module "organ-mode"
                :pathname "src/"
                :depends-on ("utils" "calendar-mode")
                :components ((:file "organ-mode")
                             (:file "highlighting")))
               (:module "agenda"
                :pathname "src/"
                :depends-on ("utils" "outline-mode" "organ-mode")
                :components ((:file "agenda-mode")))
               (:module "organ"
                :pathname "src/"
                :depends-on ("organ-mode")
                :components ((:file "organ")))))