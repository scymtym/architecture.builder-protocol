;;;; architecture.builder-protocol-xpath.asd --- System definition of architecture.builder-protocol-xpath system.
;;;;
;;;; Copyright (C) 2015-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "architecture.builder-protocol.xpath"
  :description "Query node trees using XPath expressions."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"
                "architecture.builder-protocol"
                "xpath")

  :components  ((:module     "xpath"
                 :pathname   "src/xpath"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "util")
                              (:file       "protocol")
                              (:file       "navigator")
                              (:file       "proxies"))))
  :in-order-to ((test-op (test-op "architecture.builder-protocol.xpath/test"))))

(defsystem "architecture.builder-protocol.xpath/test"
  :description "Unit tests of the architecture.builder-protocol-xpath system."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"

                (:version "fiveam"                              "1.3")

                (:version "architecture.builder-protocol.xpath" (:read-file-form "version.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/xpath"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "protocol"))))

  :perform     (test-op (opereation component)
                 (uiop:symbol-call '#:architecture.builder-protocol.xpath.test '#:run-tests)))
