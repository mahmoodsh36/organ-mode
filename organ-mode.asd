(defsystem "organ-mode"
  :description "organ-mode - an alternative to org-mode for lem."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("cltpt" "local-time" "lem/core" "lem-transient" "lem-vi-mode" "uiop")
  :components ((:module "utils"
                :pathname "src/"
                :components ((:file "utils")))
               (:module "outline-mode"
                :pathname "src/agenda-mode/"
                :components ((:file "outline-mode")))
               (:module "calendar"
                :pathname "src/calendar/"
                :depends-on ("utils")
                :components ((:file "calendar-mode")
                             (:file "popup-calendar")))
               (:module "organ-mode"
                :pathname "src/organ-mode/"
                :depends-on ("utils" "calendar")
                :components ((:file "organ-mode")
                             (:file "highlighting")))
               (:module "agenda"
                :pathname "src/agenda-mode/"
                :depends-on ("utils" "outline-mode" "organ-mode")
                :components ((:file "utils")
                             (:file "agenda-mode")))
               (:module "organ"
                :pathname "src/"
                :depends-on ("organ-mode" "calendar" "agenda")
                :components ((:file "organ")))
               (:module "capture"
                :pathname "src/"
                :depends-on ("organ")
                :components ((:file "capture")))
               (:module "publish"
                :pathname "src/"
                :depends-on ("organ")
                :components ((:file "publish")))))