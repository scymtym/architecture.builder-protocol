;;;; package.lisp --- Package definition for tests of the universal-builder module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.universal-builder.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:architecture.builder-protocol
   #:architecture.builder-protocol.universal-builder)

  (:import-from #:architecture.builder-protocol.universal-builder
   #:ensure-finalized

   #:slot-information
   #:slot-type->cardinality
   #:class-scalar-and-relation-slots)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the `universal-builder'
    class."))

(cl:in-package #:architecture.builder-protocol.universal-builder.test)

;;; Test suite and external interface

(def-suite :architecture.builder-protocol.universal-builder
  :description
  "Test suite for `universal-builder' class.")

(defun run-tests ()
  (run! :architecture.builder-protocol.universal-builder))

;;; Test utilities

(defclass mock-object-1 ()
  ())

(defclass mock-object-2 ()
  ((scalar-1)))

(defclass mock-object-3 ()
  ((scalar-1 :initarg :scalar-1 :type integer)))

(defclass mock-object-4 ()
  ((relation-1 :initarg :relation-1 :type (or null mock-object-1))))

(defclass mock-object-5 ()
  ((relation-1 :initarg :relation-1 :type vector)))
