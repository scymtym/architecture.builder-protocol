;;;; architecture.builder-protocol-xpath.asd --- System definition of architecture.builder-protocol-xpath system.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :architecture.builder-protocol.xpath
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Query node trees using XPath expressions."
  :depends-on  (:alexandria
                :architecture.builder-protocol
                :xpath)
  :components  ((:module     "xpath"
                 :pathname   "src/xpath"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "util")
                              (:file       "protocol")
                              (:file       "navigator")
                              (:file       "proxies"))))
  :in-order-to ((test-op (test-op :architecture.builder-protocol.xpath/test))))

(defsystem :architecture.builder-protocol.xpath/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests of the architecture.builder-protocol-xpath system."
  :depends-on  (:alexandria

                (:version :fiveam                              "1.3")

                (:version :architecture.builder-protocol.xpath (:read-file-form "version.sexp")))
  :components  ((:module     "test"
                 :pathname   "test/xpath"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "protocol")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :architecture.builder-protocol.xpath/test))))
  (uiop:symbol-call '#:architecture.builder-protocol.xpath.test '#:run-tests))
