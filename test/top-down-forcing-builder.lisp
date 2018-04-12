;;;; top-down-forcing-builder.lisp --- Unit tests for the top-down-forcing-builder.
;;;;
;;;; Copyright (C) 2015, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

(test top-down-forcing-builder.smoke
  "Smoke test for the `top-down-forcing-builder'."

  (let* ((builder1 (make-instance 'call-recording-mock-builder))
         (builder2 (prepare (make-instance 'top-down-forcing-builder
                                           :target builder1)))
         (node2    (make-node builder2 :foo))
         (node1    (make-node builder2 :bar)))
    ;; Test return value, i.e. produce tree plus additional return
    ;; values.
    (is (equalp (list #1=(mock-node :bar
                                    :relations `((:child . ((,#2=(mock-node :foo :finished? t) . nil))))
                                    :finished? t)
                      2)
                (multiple-value-list
                 (finish builder2
                         (wrap builder2
                               (lambda (builder)
                                 (values (relate builder :child node1 node2)
                                         2)))))))
    ;; Test builder calls made and their order.
    (is (equalp `((prepare)
                  (wrap)
                  (make-node :bar)
                  (make-node :foo)
                  (finish-node :foo ,#2#)
                  (relate :child ,#1# ,#2#)
                  (finish-node :bar ,#1#)
                  (finish ,#1#))
                (builder-calls builder1)))))
