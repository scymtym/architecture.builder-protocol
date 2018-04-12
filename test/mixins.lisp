;;;; mixins.lisp --- Unit tests for builder mixins.
;;;;
;;;; Copyright (C) 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

(test delaying-mixin.smoke
  "Smoke test for the `delaying-mixin' class."

  (let* ((builder   (make-instance 'delaying-mixin))
         (node2     (make-node builder :foo))
         (node1     (make-node builder :bar))
         (expected1 (make-delayed-node :bar '()))
         (expected2 (make-delayed-node :foo '()))
         (relation  (make-delayed-relation :child expected2 '())))
    (push relation (delayed-node-relations expected1))
    ;; Test return value, i.e. produce tree plus additional return
    ;; values.
    (is (equalp (list expected1 2)
                (multiple-value-list
                 (finish builder
                         (wrap builder
                               (lambda (builder)
                                 (values (relate builder :child node1 node2)
                                         2)))))))))
