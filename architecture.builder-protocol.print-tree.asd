;;;; architecture.builder-protocol-print-tree.asd --- System definition of architecture.builder-protocol-print-tree system.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.print-tree-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:architecture.builder-protocol.print-tree-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 2
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definitions

(defsystem :architecture.builder-protocol.print-tree
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Protocol and framework for building parse results and other object graphs."
  :depends-on  (:alexandria
                :architecture.builder-protocol
                :utilities.print-tree)
  :components  ((:module     "print-tree"
                 :pathname   "src/print-tree"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "print-tree"))))
  :in-order-to ((test-op (test-op :architecture.builder-protocol.print-tree-test))))

(defsystem :architecture.builder-protocol.print-tree-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests of the architecture.builder-protocol.print-tree system."
  :depends-on  (:alexandria

                (:version :fiveam "1.1"))
  :components  ((:module     "test"
                 :pathname   "test/print-tree"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "print-tree")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :architecture.builder-protocol.print-tree-test))))
  (uiop:symbol-call '#:architecture.builder-protocol.print-tree-test '#:run-tests))
