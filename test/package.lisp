;;;; package.lisp --- Package definition for unit tests of the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:architecture.builder-protocol)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the
    architecture.builder-protocol system"))

(cl:in-package #:architecture.builder-protocol.test)

;;; Root test suite and external interface

(def-suite :architecture.builder-protocol
  :description
  "Root unit test suite for the architecture.builder-protocol
   system.")

(defun run-tests ()
  (let ((results (run :architecture.builder-protocol)))
    (explain! results)
    (results-status results)))

;;; `mock-node' and mock builders

(defstruct (mock-node
             (:constructor mock-node (kind &key slots relations finished?))
             (:conc-name node-))
  (kind      nil :type symbol :read-only t)
  (slots     '() :type list)
  (relations '() :type list)
  (finished? nil :type boolean))

(defclass mock-builder () ())

(defmethod make-node ((builder mock-builder) (kind t)
                      &rest initargs)
  (mock-node kind :slots initargs))

(defmethod finish-node ((builder mock-builder) (kind t) (node mock-node))
  (setf (node-finished? node) t)
  node)

(defmethod relate ((builder  mock-builder)
                   (relation t)
                   (left     mock-node)
                   (right    mock-node)
                   &rest args)
  (push (cons right args)
        (cdr (or (assoc relation (node-relations left))
                 (first (push (cons relation '())
                              (node-relations left))))))
  left)

(defclass preparable-mock-builder (mock-builder)
  ((prepared? :accessor builder-prepared?
              :initform nil)))

(defmethod prepare ((builder preparable-mock-builder))
  (setf (builder-prepared? builder) t)
  builder)

(defmethod make-node ((builder preparable-mock-builder) (kind t)
                      &rest initargs)
  (apply #'call-next-method builder kind
         (list* :prepared? (builder-prepared? builder) initargs)))

(defclass finish-mock-builder (mock-builder) ())

(defmethod finish ((builder finish-mock-builder) (result t))
  (list :finish result))

(defclass preparable-finish-mock-builder (preparable-mock-builder
                                          finish-mock-builder)
  ())

(defclass call-recording-mock-builder (mock-builder)
  ((calls :accessor builder-calls
          :initform '())))

(defmethod prepare :before ((builder call-recording-mock-builder))
  (appendf (builder-calls builder) '((prepare))))

(defmethod finish :before ((builder call-recording-mock-builder) (result t))
  (appendf (builder-calls builder) `((finish ,result))))

(defmethod wrap :before ((builder call-recording-mock-builder) (thunk t))
  (appendf (builder-calls builder) '((wrap))))

(defmethod make-node :before ((builder call-recording-mock-builder)
                              (kind    t)
                              &rest args &key)
  (appendf (builder-calls builder) `((make-node ,kind ,@args))))

(defmethod finish-node :before ((builder call-recording-mock-builder)
                                (kind    t)
                                (node    t))
  (appendf (builder-calls builder) `((finish-node ,kind ,node))))

(defmethod relate :before ((builder  call-recording-mock-builder)
                           (relation t)
                           (left     t)
                           (right    t)
                           &rest args)
  (appendf (builder-calls builder) `((relate ,relation ,left ,right ,@args))))
