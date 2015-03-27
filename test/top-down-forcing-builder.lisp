;;;; top-down-forcing-builder.lisp --- Unit tests for the top-down-forcing-builder.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

(test top-down-forcing-builder.make-node
  "Smoke test for the `top-down-forcing-builder'."

  (let* ((builder1 (make-instance 'call-recording-mock-builder))
         (builder2 (make-instance 'top-down-forcing-builder
                                  :target builder1))
         (node2    (make-node builder2 :foo))
         (node1    (make-node builder2 :bar)))
    (is (equalp #1=(mock-node :bar () `((:child . ((,#2=(mock-node :foo) . nil)))))
                (finish builder2 (relate builder2 :child node1 node2))))
    (is (equalp `((prepare)
                  (wrap)
                  (make-node :bar)
                  (make-node :foo)
                  (finish-node :foo ,#2#)
                  (relate :child ,#1# ,#2#)
                  (finish-node :bar ,#1#)
                  (finish ,#1#))
                (builder-calls builder1)))))
