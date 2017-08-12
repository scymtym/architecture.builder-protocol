;;;; list-builder.lisp --- Unit tests for the list-builder.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

;;; Build protocol

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

;;; "Un-build" protocol

(test list-builder.node-kind.smoke
  "Smoke test for the `node-kind' method specialized on the `list'
   builder."

  (flet ((do-it (node expected)
           (is (eq expected (node-kind 'list node)))))
    (do-it '(:foo ())                       :foo)
    (do-it '(:foo () :bar 1)                :foo)
    (do-it '(:foo (:baz (((:fez ()). ())))) :foo)))

(test list-builder.node-initargs.smoke
  "Smoke test for the `node-initargs' method specialized on the `list'
   builder."

  (flet ((do-it (node expected)
           (is (equal expected (node-initargs 'list node)))))
    (do-it '(:foo ())                        '())
    (do-it '(:foo () :bar 1)                 '(:bar 1))
    (do-it '(:foo (:baz (((:fez ()) . ())))) '())))

(test list-builder.node-relations.smoke
  "Smoke test for the `node-relations' method specialized on the
   `list' builder."

  (flet ((do-it (node expected)
           (is (equal expected (node-relations 'list node)))))
    (do-it '(:foo ())                        '())
    (do-it '(:foo () :bar 1)                 '())
    (do-it '(:foo (:baz (((:fez ()) . ())))) '(:baz))))

(test list-builder.node-relation.smoke
  "Smoke test for the `node-relation' method specialized on the `list'
   builder."

  (flet ((do-it (node relation expected)
           (is (equal expected (multiple-value-list
                                (node-relation 'list relation node))))))
    ;; No relations.
    (do-it '(:foo ())        :baz '(()))
    (do-it '(:foo () :bar 1) :baz '(()))
    ;; One relation.
    (let* ((node-3 '(:fez ()))
           (node-2 `(:foo (:baz       ((,node-3 . (:who 2))))))
           (node-1 `(:foo ((:baz . 1) ((,node-3 . (:who 2)))))))
      (do-it node-2 :baz        `((,node-3) ((:who 2))))
      (do-it node-2 '(:baz . *) `((,node-3) ((:who 2))))
      (do-it node-2 '(:fez . 1) '(()))
      (do-it node-1 :baz        `(,node-3 (:who 2)))
      (do-it node-1 '(:baz . 1) `(,node-3 (:who 2)))
      (do-it node-1 '(:fez . 1) '(())))
    ;; Multiple relations
    (let* ((node-3 '(:arp ()))
           (node-2 '(:fez ()))
           (node-1 `(:foo (:baz ((,node-2 . (:who 2)))
                           :dot ((,node-3 . (:dat 3)))))))
      (do-it node-1 :baz `((,node-2) ((:who 2))))
      (do-it node-1 :dot `((,node-3) ((:dat 3))))
      (do-it node-1 :fez '(())))))
