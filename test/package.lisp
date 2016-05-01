;;;; package.lisp --- Package definition for unit tests of the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
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

  ;; Test utilities
  (:export
   #:record-un-build-calls
   #:record-un-build-calls/peeking)

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
  (run! :architecture.builder-protocol))

;;; `mock-node' and mock builders

(defstruct (mock-node
             (:constructor mock-node (kind &key slots relations finished?))
             (:conc-name mock-))
  (kind      nil :type symbol :read-only t)
  (slots     '() :type list)
  (relations '() :type list)
  (finished? nil :type boolean))

(defclass mock-builder () ())

(defmethod make-node ((builder mock-builder) (kind t)
                      &rest initargs)
  (mock-node kind :slots initargs))

(defmethod finish-node ((builder mock-builder) (kind t) (node mock-node))
  (setf (mock-finished? node) t)
  node)

(defmethod relate ((builder  mock-builder)
                   (relation t)
                   (left     mock-node)
                   (right    mock-node)
                   &rest args)
  (push (cons right args)
        (cdr (or (assoc relation (mock-relations left))
                 (first (push (cons relation '())
                              (mock-relations left))))))
  left)

(defmethod node-kind ((builder mock-builder)
                      (node    mock-node))
  (mock-kind node))

(defmethod node-initargs ((builder mock-builder)
                          (node    mock-node))
  (mock-slots node))

(defmethod node-relations ((builder mock-builder)
                           (node    mock-node))
  (mapcar #'car (mock-relations node)))

(defmethod node-relation ((builder  mock-builder)
                          (relation t)
                          (node     mock-node))
  (when-let ((cell (assoc relation (mock-relations node)
                          :key (lambda (cell)
                                 (if (consp cell)
                                     (car cell)
                                     cell)))))
    (destructuring-bind (relation . targets) cell
      (etypecase (when (consp relation) (cdr relation))
        ((member ? 1)
         (values (car targets) (cdr targets)))
        ((or null (eql *) (cons (eql :map)))
         (values (mapcar #'car targets) (mapcar #'cdr targets)))))))

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

(defmethod finish ((builder finish-mock-builder) (values cons))
  (apply #'values (list :finish (first values)) (rest values)))

(defclass preparable-finish-mock-builder (preparable-mock-builder
                                          finish-mock-builder)
  ())

(defclass call-recording-mock-builder (mock-builder)
  ((calls :accessor builder-calls
          :initform '())))

(defmethod prepare :before ((builder call-recording-mock-builder))
  (appendf (builder-calls builder) '((prepare))))

(defmethod finish :before ((builder call-recording-mock-builder) (values t))
  (appendf (builder-calls builder) `((finish ,(first values)))))

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

;;; "Un-build"-related test utilities

(defun %record-un-build-calls (walk-nodes builder object make-function)
  (let ((calls '()))
    (values
     (multiple-value-list
      (flet ((record (call) (push call calls)))
        (funcall walk-nodes builder (funcall make-function #'record) object)))
     (nreverse calls))))

(defun record-un-build-calls (walk-nodes builder object)
  (%record-un-build-calls
   walk-nodes builder object
   (lambda (record)
     (lambda (recurse relation relation-args node kind relations
              &rest initargs)
       (funcall record
                `(:visit ,relation ,relation-args ,node ,kind ,relations ,initargs))
       (or (funcall recurse) kind)))))

(defun record-un-build-calls/peeking (walk-nodes builder atom-type object)
  (%record-un-build-calls
   walk-nodes builder object
   (lambda (record)
     (peeking
      (lambda (builder relation relation-args node)
        (declare (ignore builder))
        (funcall record `(:peek ,relation ,relation-args ,node))
        (unless (typep node atom-type)
          t))
      (lambda (recurse relation relation-args node kind relations
               &rest initargs)
        (funcall record
                 `(:visit ,relation ,relation-args ,node ,kind ,relations ,initargs))
        (or (funcall recurse) kind))))))
