;;;; universal-builder.lisp --- Tests for universal builder.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.universal-builder.test)

(in-suite :architecture.builder-protocol.universal-builder)

;;; Build support

(test universal-builder.make-node.smoke
  "Test `make-node' method for `universal-builder' class."

  (with-builder ((make-instance 'universal-builder))
    (let ((node (make-node* 'mock-object-1)))
      (is (typep node 'mock-object-1)))
    (let ((node (make-node* 'mock-object-3 :scalar-1 1)))
      (is (typep node 'mock-object-3))
      (is (eql 1 (slot-value node 'scalar-1))))))

(test universal-builder.relate.smoke
  "Test `relate' method for `universal-builder' class."

  (with-builder ((make-instance 'universal-builder))
    (let ((left  (make-node* 'mock-object-4))
          (right (make-node* 'mock-object-1)))
      (setf left (relate* :relation-1 left right))
      (is (eq right (slot-value left 'relation-1))))

    (let ((left  (make-node* 'mock-object-5 :relation-1 (make-array 0 :adjustable t :fill-pointer 0)))
          (right (make-node* 'mock-object-1)))
      (setf left (relate* :relation-1 left right))
      (is (eq right (aref (slot-value left 'relation-1) 0))))

    (let ((left  (make-node* 'mock-object-6 :relation-1 (make-hash-table)))
          (right (make-node* 'mock-object-1)))
      (setf left (relate* :relation-1 left right :key :a))
      (is (eq right (gethash :a (slot-value left 'relation-1)))))))

;;; Un-build support

(test universal-builder.node-kind.smoke
  "Smoke test for `node-kind' implementation of `universal-builder'
   class."

  (flet ((do-it (node expected)
           (let ((builder (make-instance 'universal-builder)))
             (is (eq expected (node-kind builder node))))))
    (do-it *mock-object-1-1* 'mock-object-1)
    (do-it *mock-object-2-1* 'mock-object-2)
    (do-it *mock-object-3-1* 'mock-object-3)
    (do-it *mock-object-3-2* 'mock-object-3)

    (do-it *mock-object-4-1* 'mock-object-4)
    (do-it *mock-object-5-1* 'mock-object-5)
    (do-it *mock-object-5-2* 'mock-object-5)
    (do-it *mock-object-6-1* 'mock-object-6)
    (do-it *mock-object-6-2* 'mock-object-6)
    (do-it *mock-object-7-1* 'mock-object-7)
    (do-it *mock-object-7-2* 'mock-object-7)

    (do-it *mock-object-8-1* 'mock-object-8)
    (do-it *mock-object-8-2* 'mock-object-8)
    (do-it *mock-object-9-1* 'mock-object-9)
    (do-it *mock-object-9-2* 'mock-object-9)))

(test universal-builder.node-initargs.smoke
  "Smoke test for `node-initargs' implementation of
   `universal-builder' class."

  (flet ((do-it (node expected)
           (let ((builder (make-instance 'universal-builder)))
             (is (equal expected (node-initargs builder node))))))
    (do-it *mock-object-1-1* '())
    (do-it *mock-object-2-1* '())
    (do-it *mock-object-3-1* '())
    (do-it *mock-object-3-2* '(:scalar-1 1))

    (do-it *mock-object-4-1* '())
    (do-it *mock-object-5-1* '())
    (do-it *mock-object-5-2* '())
    (do-it *mock-object-6-1* '())
    (do-it *mock-object-6-2* '())
    (do-it *mock-object-7-1* '())
    (do-it *mock-object-7-2* '())

    (do-it *mock-object-8-1* '())
    (do-it *mock-object-8-2* '(:scalar-1 1 :scalar-2 "foo"))
    (do-it *mock-object-9-1* '())
    (do-it *mock-object-9-2* '())))

(test universal-builder.node-relations.smoke
  "Smoke test for `node-relations' implementation of
   `universal-builder' class."

  (flet ((do-it (node expected)
           (let ((builder (make-instance 'universal-builder)))
             (is (equal expected (node-relations builder node))))))
    (do-it *mock-object-1-1* '())
    (do-it *mock-object-2-1* '())
    (do-it *mock-object-3-1* '())
    (do-it *mock-object-3-2* '())

    (do-it *mock-object-4-1* '((:relation-1 . ?)))
    (do-it *mock-object-5-1* '((:relation-1 . *)))
    (do-it *mock-object-5-2* '((:relation-1 . *)))
    (do-it *mock-object-6-1* '((:relation-1 . (:map . :key))))
    (do-it *mock-object-6-2* '((:relation-1 . (:map . :key))))
    (do-it *mock-object-7-1* '((:relation-1 . 1)))
    (do-it *mock-object-7-2* '((:relation-1 . 1)))

    (do-it *mock-object-8-1* '((:relation-1 . *) (:relation-2 . *)))
    (do-it *mock-object-8-2* '((:relation-1 . *) (:relation-2 . *)))
    (do-it *mock-object-9-1* '((:relation-1  . *)
                               (:relation-2  . *)
                               (:relation-3  . *)
                               (:relation-4  . *)
                               (:relation-5  . *)
                               (:relation-6  . *)
                               (:relation-7  . *)
                               (:relation-8  . *)
                               (:relation-9  . *)
                               (:relation-10 . *)))
    (do-it *mock-object-9-2* '((:relation-1  . *)
                               (:relation-2  . *)
                               (:relation-3  . *)
                               (:relation-4  . *)
                               (:relation-5  . *)
                               (:relation-6  . *)
                               (:relation-7  . *)
                               (:relation-8  . *)
                               (:relation-9  . *)
                               (:relation-10 . *)))))

(test universal-builder.node-relation.smoke
  "Smoke test for `node-relation' implementation of
   `universal-builder' class."

  (flet ((do-it (node relation expected)
           (let ((builder (make-instance 'universal-builder)))
             (is (equalp expected (multiple-value-list
                                   (node-relation builder relation node)))))))
    ;; No relations.
    (do-it *mock-object-1-1* :no-relations     '(()))
    (do-it *mock-object-2-1* :no-relations     '(()))
    (do-it *mock-object-3-1* :no-relations     '(()))
    (do-it *mock-object-3-2* :no-relations     '(()))
    ;; One relation.
    (do-it *mock-object-4-1* :no-such-relation '(()))
    (do-it *mock-object-4-1* :relation-1       '(()))
    (do-it *mock-object-5-1* :no-such-relation '(()))
    (do-it *mock-object-5-2* :relation-1       '(#()))
    (do-it *mock-object-6-1* :no-such-relation '(()))
    (do-it *mock-object-6-2* :relation-1       '((1) ((:key :a))))
    (do-it *mock-object-7-1* :no-such-relation '(()))
    (do-it *mock-object-7-2* :relation-1       '(:foo))
    ;; Multiple relations
    (do-it *mock-object-8-1* :no-such-relation '(()))
    (do-it *mock-object-8-2* :relation-1       '(#()))
    (do-it *mock-object-9-1* :no-such-relation '(()))
    (do-it *mock-object-9-2* :relation-1       '(()))))

(test universal-builder.class-redefinition
  "Test behavior of `universal-builder' class in case of class
   redefinitions."

  (flet ((do-it (node relation expected)
           (let ((builder (make-instance 'universal-builder)))
             (is (equal expected (multiple-value-list
                                  (node-relation builder relation node)))))))

    (defclass mock-object-7 ()
      ((relation-1 :initarg :relation-1)
       (relation-2 :initarg :relation-2 :initform :foo)))

    (do-it *mock-object-7-1* :no-such-relation '(()))
    (do-it *mock-object-7-1* :relation-1       '(()))
    (do-it *mock-object-7-1* :relation-2       '(:foo))
    (do-it *mock-object-7-2* :no-such-relation '(()))
    (do-it *mock-object-7-2* :relation-1       '(:foo))
    (do-it *mock-object-7-2* :relation-2       '(:foo))

    (defclass mock-object-7 ()
      ((relation-1 :initarg :relation-1)))

    (do-it *mock-object-7-1* :no-such-relation '(()))
    (do-it *mock-object-7-1* :relation-1       '(()))
    (do-it *mock-object-7-1* :relation-2       '(()))
    (do-it *mock-object-7-2* :no-such-relation '(()))
    (do-it *mock-object-7-2* :relation-1       '(:foo))
    (do-it *mock-object-7-2* :relation-2       '(()))))
