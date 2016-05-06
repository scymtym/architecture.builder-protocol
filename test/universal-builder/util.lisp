;;;; universal-builder.lisp --- Tests for universal builder.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.universal-builder.test)

(in-suite :architecture.builder-protocol.universal-builder)

(test slot-information.smoke
  "Smoke test for the `slot-information' function."

  (mapc (lambda (spec)
          (destructuring-bind (class-name slot-name expected) spec
            (let* ((class (ensure-finalized (find-class class-name)))
                   (slot  (find slot-name (c2mop:class-slots class)
                                :key #'c2mop:slot-definition-name)))
              (is (equal expected (slot-information slot))))))
        '((mock-object-4 relation-1 (relation-1 :relation-1 (or null mock-object-1) ?))
          (mock-object-5 relation-1 (relation-1 :relation-1 vector                  *)))))

(test slot-type->cardinality.smoke
  "Smoke test for the `slot-type->cardinality' function."

  (mapc (lambda (spec)
          (destructuring-bind (type expected) spec
            (is (equal expected (slot-type->cardinality type)))))
        '((nil                                  nil)

          (t                                    1)

          (hash-table                           (:map . :key))

          (standard-object                      1)
          ((or null standard-object)            ?)

          (structure-object                     1)
          ((or null structure-object)           ?)

          (string                               nil)
          ((or null string)                     nil)

          ((vector (unsigned-byte 8))           nil)
          ((or null (vector (unsigned-byte 8))) nil)

          (list                                 *)
          (vector                               *))))

(test class-scalar-and-relation-slots.smoke
  "Smoke test for the `class-scalar-and-relation-slots' function."

  (labels ((initarg (slot)
             (first (c2mop:slot-definition-initargs slot)))
           (do-it (class-name expected-scalar expected-relation)
             (multiple-value-bind (scalar-slots relation-slots)
                 (class-scalar-and-relation-slots (find-class class-name))
               (is (equal expected-scalar   (mapcar #'initarg scalar-slots)))
               (is (equal expected-relation (mapcar #'initarg relation-slots))))))
    (do-it 'mock-object-1 '()          '())
    (do-it 'mock-object-2 '()          '())
    (do-it 'mock-object-3 '(:scalar-1) '())
    (do-it 'mock-object-4 '()          '(:relation-1))
    (do-it 'mock-object-5 '()          '(:relation-1))))
