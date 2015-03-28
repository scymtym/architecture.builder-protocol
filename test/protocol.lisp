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

;;; Global processing tests

(test prepare.smoke
  "Smoke test for the `prepare' generic function."

  (let ((builder (prepare (make-instance 'preparable-mock-builder))))
    (is (equalp (mock-node :foo '(:prepared? t))
                (make-node builder :foo)))))

(test finish.smoke
  "Smoke test for the `finish' generic function."

  (let ((builder (prepare (make-instance 'finish-mock-builder))))
    (is (equalp `(:finish ,(mock-node :foo))
                (finish builder (make-node builder :foo))))))

(defun a-wrapper (builder)
  (make-node builder :foo))

(test wrap.smoke
  "Smoke test for the `wrap' generic function."

  (macrolet ((test-case (wrapper)
               `(let ((builder (make-instance 'mock-builder)))
                  (is (equalp (mock-node :foo)
                              (wrap builder ,wrapper))))))
    (test-case 'a-wrapper)
    (test-case #'a-wrapper)))

;;; Node construction tests

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

(test with-builder.smoke.1
  "Smoke test for first syntax of the `with-builder' macro."

  (let ((result (with-builder ((make-instance
                                'preparable-finish-mock-builder))
                  (make-node *builder* :foo))))
    (is (equalp `(:finish ,(mock-node :foo '(:prepared? t))) result))))

(test with-builder.smoke.2
  "Smoke test for second syntax of the `with-builder' macro."

  (let ((result (with-builder (builder (make-instance
                                        'preparable-finish-mock-builder))
                  (make-node builder :foo))))
    (is (equalp `(:finish ,(mock-node :foo '(:prepared? t))) result))))
