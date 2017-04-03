;;;; package.lisp --- Package definition for tests of the universal-builder module.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
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
   #:class-initarg-and-relation-slots)

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

(defvar *mock-object-1-1*
  (make-instance 'mock-object-1))

(defclass mock-object-2 ()
  ((scalar-1)))

(defvar *mock-object-2-1*
  (make-instance 'mock-object-2))

(defclass mock-object-3 ()
  ((scalar-1 :initarg :scalar-1 :type integer)))

(defvar *mock-object-3-1*
  (make-instance 'mock-object-3))

(defvar *mock-object-3-2*
  (make-instance 'mock-object-3 :scalar-1 1))

(defclass mock-object-4 ()
  ((relation-1 :initarg :relation-1 :type (or null mock-object-1))))

(defvar *mock-object-4-1*
  (make-instance 'mock-object-4 :relation-1 nil))

(defclass mock-object-5 ()
  ((relation-1 :initarg :relation-1 :type vector)))

(defvar *mock-object-5-1*
  (make-instance 'mock-object-5))

(defvar *mock-object-5-2*
  (make-instance 'mock-object-5 :relation-1 #()))

(defclass mock-object-6 ()
  ((relation-1 :initarg :relation-1 :type hash-table)))

(defvar *mock-object-6-1*
  (make-instance 'mock-object-6))

(defvar *mock-object-6-2*
  (let ((object (make-instance 'mock-object-6 :relation-1 (make-hash-table))))
    (setf (gethash :a (slot-value object 'relation-1)) 1)
    object))

(defclass mock-object-7 ()
  ((relation-1 :initarg :relation-1)))

(defvar *mock-object-7-1*
  (make-instance 'mock-object-7))

(defvar *mock-object-7-2*
  (make-instance 'mock-object-7 :relation-1 :foo))

(defclass mock-object-8 ()
  ((scalar-1   :initarg :scalar-1   :type integer)
   (scalar-2   :initarg :scalar-2   :type string)
   (relation-1 :initarg :relation-1 :type vector)
   (relation-2 :initarg :relation-2 :type list)))

(defvar *mock-object-8-1*
  (make-instance 'mock-object-8))

(defvar *mock-object-8-2*
  (make-instance 'mock-object-8
                 :scalar-1   1
                 :scalar-2   "foo"
                 :relation-1 #()
                 :relation-2 '()))

(defclass mock-object-9 ()
  ((relation-1  :initarg :relation-1  :type list)
   (relation-2  :initarg :relation-2  :type list)
   (relation-3  :initarg :relation-3  :type list)
   (relation-4  :initarg :relation-4  :type list)
   (relation-5  :initarg :relation-5  :type list)
   (relation-6  :initarg :relation-6  :type list)
   (relation-7  :initarg :relation-7  :type list)
   (relation-8  :initarg :relation-8  :type list)
   (relation-9  :initarg :relation-9  :type list)
   (relation-10 :initarg :relation-10 :type list)))

(defvar *mock-object-9-1*
  (make-instance 'mock-object-9))

(defvar *mock-object-9-2*
  (make-instance 'mock-object-9
                 :relation-1  '()
                 :relation-2  '()
                 :relation-3  '()
                 :relation-4  '()
                 :relation-5  '()
                 :relation-6  '()
                 :relation-7  '()
                 :relation-8  '()
                 :relation-9  '()
                 :relation-10 '()))
