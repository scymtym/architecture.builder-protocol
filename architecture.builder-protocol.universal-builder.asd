;;;; architecture.builder-protocol.universal-builder.asd --- architecture.builder-protocol.universal-builder system.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :architecture.builder-protocol.universal-builder
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Builder and un-builder for standard-object instances."
  :depends-on  (:alexandria
                :closer-mop

                (:version :architecture.builder-protocol (:read-file-form "version.sexp")))
  :components  ((:module     "universal-builder"
                 :pathname   "src/universal-builder"
                 :components ((:file       "package")
                              (:file       "util"))))
  :in-order-to ((test-op (test-op :architecture.builder-protocol.universal-builder-test))))

(defsystem :architecture.builder-protocol.universal-builder-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests of the architecture.builder-protocol.universal-builder system."
  :depends-on  (:alexandria

                (:version :fiveam                                          "1.3")

                (:version :architecture.builder-protocol.universal-builder (:read-file-form "version.sexp"))

                (:version :architecture.builder-protocol-test              (:read-file-form "version.sexp")))
  :components  ((:module     "universal-builder"
                 :pathname   "test/universal-builder"
                 :components ((:file       "package")
                              (:file       "util")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :architecture.builder-protocol.universal-builder-test))))
  (uiop:symbol-call '#:architecture.builder-protocol.universal-builder.test '#:run-tests))
