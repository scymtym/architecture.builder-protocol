;;;; list-builder.lisp --- Unit tests for the list-builder.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

(test list-builder.make-node.smoke
  "Smoke test for the `make-node' method specialized on the `list'
   builder."

  (is (equal '(:foo ())        (make-node 'list :foo)))
  (is (equal '(:foo () :bar 1) (make-node 'list :foo :bar 1))))

(test list-builder.finish-node.smoke
  "Smoke test for the `finish-node' method specialized on the `list'
   builder."

  (flet ((do-it (kind &rest args)
           (let* ((builder 'list)
                  (node    (apply #'make-node builder kind args)))
             (is (equal node (finish-node builder kind node))))))

    (do-it :foo)
    (do-it :foo :bar 1)))

(test list-builder.relate.smoke
  "Smoke test for the `relate' method specialized on the `list'
   builder."

  ;; "Right" is nil.
  (let* ((builder 'list)
         (node (make-node builder :foo)))
    (relate builder :baz node nil)
    (is (equal `(:foo ()) node)))

  ;; Without relation arguments.
  (let* ((builder 'list)
         (node-1 (make-node builder :foo))
         (node-2 (make-node builder :bar)))
    (relate builder :baz node-1 node-2)
    (is (equal `(:foo (:baz ((,node-2 . ())))) node-1)))

  ;; With relation arguments.
  (let* ((builder 'list)
         (node-1 (make-node builder :foo))
         (node-2 (make-node builder :bar)))
    (relate builder :baz node-1 node-2 :arg 1)
    (is (equal `(:foo (:baz ((,node-2 . (:arg 1))))) node-1))))

