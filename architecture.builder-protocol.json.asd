;;;; architecture.builder-protocol.json.asd --- System definition of architecture.builder-protocol.json system.
;;;;
;;;; Copyright (C) 2015-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "architecture.builder-protocol.json"
  :description "Serialize node trees into JSON documents."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"
                "architecture.builder-protocol"
                "cl-json")

  :components  ((:module     "json"
                 :pathname   "src/json"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "json"))))

  :in-order-to ((test-op (test-op "architecture.builder-protocol.json/test"))))

(defsystem "architecture.builder-protocol.json/test"
  :description "Unit tests of the architecture.builder-protocol.json system."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"

                (:version "fiveam"                             "1.3")

                (:version "architecture.builder-protocol.json" (:read-file-form "version.sexp"))

                (:version "architecture.builder-protocol/test" (:read-file-form "version.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/json"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "json"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:architecture.builder-protocol.json.test '#:run-tests)))
