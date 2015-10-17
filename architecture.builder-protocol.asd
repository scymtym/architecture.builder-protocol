;;;; architecture.builder-protocol.asd --- System definition of architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:architecture.builder-protocol-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 3
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

(defsystem :architecture.builder-protocol
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Protocol and framework for building parse results and other object graphs."
  :depends-on  (:alexandria)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "util")
                              (:file       "variables")
                              (:file       "protocol")
                              (:file       "macros")

                              (:file       "list-builder")
                              (:file       "top-down-forcing-builder"))))
  :in-order-to ((test-op (test-op :architecture.builder-protocol-test))))

(defsystem :architecture.builder-protocol-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests of the architecture.builder-protocol system."
  :depends-on  (:alexandria

                (:version :fiveam "1.1"))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "util")
                              (:file       "protocol")
                              (:file       "macros")

                              (:file       "list-builder")
                              (:file       "top-down-forcing-builder")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :architecture.builder-protocol-test))))
  (funcall (read-from-string "architecture.builder-protocol.test:run-tests")))
