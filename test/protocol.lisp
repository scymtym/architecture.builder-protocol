;;;; protocol.lisp --- Unit tests for the protocol of the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:builder-protocol.test)

(in-suite :builder-protocol)

;;; `mock-node' and `mock-builder'

(defstruct (mock-node
             (:constructor mock-node (kind &optional slots relations))
             (:conc-name node-))
  (kind      nil :type symbol :read-only t)
  (slots     '() :type list)
  (relations '() :type list))

(defclass mock-builder () ())

(defmethod make-node ((builder mock-builder) (kind t)
                      &rest initargs)
  (mock-node kind initargs))

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

;;; Tests

(test make-node.smoke
  "Smoke test for the `make-node' generic function."

  (let ((builder (make-instance 'mock-builder)))
    (is (equalp (mock-node :foo)
                (make-node builder :foo)))
    (is (equalp (mock-node :foo '(:bar 1))
                (make-node builder :foo :bar 1)))))

(test relate.smoke
  "Smoke test for `relate' generic function."

  (let ((builder (make-instance 'mock-builder)))
    (is (equalp (mock-node :foo () `((:baz . ((,(mock-node :bar) . nil)))))
                (relate builder :baz (mock-node :foo) (mock-node :bar))))
    (is (equalp (mock-node :foo () `((:baz . ((,(mock-node :bar) . (:fez 2))))))
                (relate builder :baz (mock-node :foo) (mock-node :bar)
                        :fez 2)))))
