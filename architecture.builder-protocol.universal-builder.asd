;;;; architecture.builder-protocol.universal-builder.asd --- architecture.builder-protocol.universal-builder system.
;;;;
;;;; Copyright (C) 2015, 2016, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "architecture.builder-protocol.universal-builder"
  :description "Builder and un-builder for standard-object instances."
  :license     "LLGPLv3"                ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"
                "closer-mop"

                (:version "architecture.builder-protocol" (:read-file-form "version.sexp")))
  :components  ((:module     "universal-builder"
                 :pathname   "src/universal-builder"
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "mixins")
                              (:file       "universal-builder"))))
  :in-order-to ((test-op (test-op "architecture.builder-protocol.universal-builder/test"))))

(defsystem :architecture.builder-protocol.universal-builder/test
  :description "Unit tests of the architecture.builder-protocol.universal-builder system."
  :license     "LLGPLv3"                ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"

                (:version "fiveam"                                          "1.3")

                (:version "architecture.builder-protocol.universal-builder" (:read-file-form "version.sexp"))

                (:version "architecture.builder-protocol/test"              (:read-file-form "version.sexp")))

  :components  ((:module     "universal-builder"
                 :pathname   "test/universal-builder"
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "universal-builder"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:architecture.builder-protocol.universal-builder.test '#:run-tests)))
