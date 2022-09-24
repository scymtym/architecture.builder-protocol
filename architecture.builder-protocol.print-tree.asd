;;;; architecture.builder-protocol-print-tree.asd --- System definition of architecture.builder-protocol-print-tree system.
;;;;
;;;; Copyright (C) 2015-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "architecture.builder-protocol.print-tree"
  :description "Printing parse results and other object graphs as textual trees."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"
                "architecture.builder-protocol"
                "utilities.print-tree")

  :components  ((:module     "print-tree"
                 :pathname   "src/print-tree"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "print-tree"))))

  :in-order-to ((test-op (test-op "architecture.builder-protocol.print-tree/test"))))

(defsystem "architecture.builder-protocol.print-tree/test"
  :description "Unit tests of the architecture.builder-protocol.print-tree system."
  :license     "LGPLv3" ; see COPYING file for details
  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"

                (:version "fiveam"                                   "1.3")

                (:version "architecture.builder-protocol.print-tree" (:read-file-form "version.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/print-tree"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "print-tree")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system "architecture.builder-protocol.print-tree/test"))))
  (uiop:symbol-call '#:architecture.builder-protocol.print-tree.test '#:run-tests))
