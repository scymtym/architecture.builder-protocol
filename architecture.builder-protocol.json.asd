;;;; architecture.builder-protocol.json.asd --- System definition of architecture.builder-protocol.json system.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :architecture.builder-protocol.json
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Serialize node trees into JSON documents."
  :depends-on  (:alexandria
                :architecture.builder-protocol
                :cl-json)
  :components  ((:module     "json"
                 :pathname   "src/json"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "json"))))
  :in-order-to ((test-op (test-op :architecture.builder-protocol.json/test))))

(defsystem :architecture.builder-protocol.json/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests of the architecture.builder-protocol.json system."
  :depends-on  (:alexandria

                (:version :fiveam                             "1.3")

                (:version :architecture.builder-protocol.json (:read-file-form "version.sexp"))

                (:version :architecture.builder-protocol/test (:read-file-form "version.sexp")))
  :components  ((:module     "test"
                 :pathname   "test/json"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "json")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :architecture.builder-protocol.json/test))))
  (uiop:symbol-call '#:architecture.builder-protocol.json.test '#:run-tests))
