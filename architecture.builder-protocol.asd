;;;; architecture.builder-protocol.asd --- System definition of architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2012-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "architecture.builder-protocol"
  :description "Protocol and framework for building parse results and other object graphs."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria")

  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "util")
                              (:file       "variables")
                              (:file       "protocol")
                              (:file       "macros")

                              (:file       "mixins")
                              (:file       "list-builder")
                              (:file       "top-down-forcing-builder"))))

  :in-order-to ((test-op (test-op "architecture.builder-protocol/test"))))

(defsystem "architecture.builder-protocol/test"
  :description "Unit tests of the architecture.builder-protocol system."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"

                (:version "fiveam"                        "1.3")

                (:version "architecture.builder-protocol" (:read-file-form "version.sexp")))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "util")
                              (:file       "protocol")
                              (:file       "macros")

                              (:file       "mixins")
                              (:file       "list-builder")
                              (:file       "top-down-forcing-builder"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:architecture.builder-protocol.test '#:run-tests)))
