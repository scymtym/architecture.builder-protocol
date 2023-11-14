;;;; architecture.builder-protocol.inspection.asd --- architecture.builder-protocol.inspection system.
;;;;
;;;; Copyright (C) 2021, 2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "architecture.builder-protocol.inspection"
  :description "Allows inspecting trees in the graphical inspector Clouseau."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("clouseau"
                (:version "architecture.builder-protocol" (:read-file-form "version.sexp")))

  :components  ((:module     "inspection"
                 :pathname   "code/inspection"
                 :components ((:file       "package")
                              (:file       "inspect")))))
