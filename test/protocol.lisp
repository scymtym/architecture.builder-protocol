;;;; protocol.lisp --- Unit tests for the protocol of the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

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
