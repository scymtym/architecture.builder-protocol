;;;; protocol.lisp --- Unit tests for the protocol of the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

;;; Utilities

(defmacro with-implicit-and-explicit-builder
    ((builder-var builder) explicit-function &body body)
  (let ((implicit-function (symbolicate explicit-function '*)))
    `(progn
       ;; Explicit builder argument.
       (let ((,builder-var ,builder)) ,@body)
       ;; Implicit builder argument
       (let* ((,builder-var ,builder)
              (*builder* ,builder-var))
         (flet ((,explicit-function (&rest args)
                  (apply ',implicit-function (rest args))))
           ,@body)))))

;;; Global processing tests

(test prepare.smoke
  "Smoke test for the `prepare[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'preparable-mock-builder))
      prepare
    (let ((builder (prepare builder)))
      (is (equalp (mock-node :foo :slots '(:prepared? t))
                  (make-node builder :foo))))))

(test finish.smoke
  "Smoke test for the `finish[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (prepare (make-instance 'finish-mock-builder)))
      finish
    (is (equalp `(:finish ,(mock-node :foo))
                (finish builder (make-node builder :foo))))))

(defun a-wrapper (builder)
  (make-node builder :foo))

(test wrap.smoke
  "Smoke test for the `wrap[*]' functions."

  (macrolet ((test-case (wrapper)
               `(with-implicit-and-explicit-builder
                    (builder (make-instance 'mock-builder))
                    wrap
                  (is (equalp (mock-node :foo) (wrap builder ,wrapper))))))
    (test-case 'a-wrapper)
    (test-case #'a-wrapper)))

;;; Node construction tests

(test make-node.smoke
  "Smoke test for the `make-node[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      make-node
    (is (equalp (mock-node :foo)
                (make-node builder :foo)))
    (is (equalp (mock-node :foo :slots '(:bar 1))
                (make-node builder :foo :bar 1)))))

(test finish-node.smoke
  "Smoke test for the `finish-node[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'mock-builder))
      finish-node
    (is (equalp (mock-node :foo :finished? t)
                (finish-node builder :foo (mock-node :foo))))))

(test make+finish-node.smoke
  "Smoke test for the `make+finish-node[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'mock-builder))
      make+finish-node
    (is (equalp (mock-node :foo :finished? t)
                (make+finish-node builder :foo)))
    (is (equalp (mock-node :foo :slots '(:bar 1) :finished? t)
                (make+finish-node builder :foo :bar 1)))))

(test relate.smoke
  "Smoke test for `relate[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      relate
    (is (equalp (mock-node :foo
                           :relations `((:baz . ((,(mock-node :bar) . nil)))))
                (relate builder :baz (mock-node :foo) (mock-node :bar))))
    (is (equalp (mock-node :foo
                           :relations `((:baz . ((,(mock-node :bar) . (:fez 2))))))
                (relate builder :baz (mock-node :foo) (mock-node :bar)
                        :fez 2)))))

(test make+finish-node+relations.smoke
  "Smoke test for the `make+finish-node+relations[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'mock-builder))
      make+finish-node+relations
    (is (equalp (mock-node :foo :finished? t)
                (make+finish-node+relations builder :foo '() '())))
    (is (equalp (mock-node :foo :slots '(:bar 1) :finished? t)
                (make+finish-node+relations builder :foo '(:bar 1) '())))
    (is (equalp (mock-node :foo
                           :relations `((:fez (,(mock-node :baz))) )
                           :finished? t)
                (make+finish-node+relations
                 builder :foo '() `((1 :fez ,(mock-node :baz))))))
    (is (equalp (mock-node :foo
                           :relations `((:fez (,(mock-node :baz))))
                           :finished? t)
                (make+finish-node+relations
                 builder :foo '() `((* :fez (,(mock-node :baz)))))))))
