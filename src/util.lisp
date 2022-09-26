;;;; util.lisp --- Utilities provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol)

(defun normalize-relation (relation)
  "Return two values: 1) the relation kind of RELATION 2) the
   cardinality of RELATION.

   RELATION can be of the form RELATION-KIND or (RELATION-KIND
   . CARDINALITY) where CARDINALITY is of type
   `relation-cardinality'."
  (if (consp relation)
      (values (car relation) (cdr relation))
      (values relation       '*)))

;;; `cardinality-[e]case'

(deftype %map-pattern ()
  '(cons (eql :map) symbol))

(defun expand-cardinality-case (cardinality clauses exhaustive)
  (let ((remaining '(? 1 * :map)))
    (once-only (cardinality)
      (labels ((make-type-specifier (spec)
                 (etypecase spec
                   ((or %map-pattern (eql :map))
                    (removef remaining :map)
                    '(cons (eql :map) (not cons)))
                   ((member 1 ? *)
                    (removef remaining spec)
                    `(eql ,spec))))
               (make-bindings (spec)
                 (typecase spec
                   (%map-pattern
                    `((,(cdr spec) (cdr ,cardinality))))))
               (make-clause (clause)
                 (destructuring-bind (cardinalities &body body) clause
                   (let ((cardinalities (if (typep cardinalities '%map-pattern)
                                            (list cardinalities)
                                            (ensure-list cardinalities))))
                     `((or ,@(mapcar #'make-type-specifier cardinalities))
                       (let ,(mappend #'make-bindings cardinalities)
                         ,@body))))))
        (prog1
            `(,(if exhaustive 'etypecase 'typecase) ,cardinality
               ,@(mapcar #'make-clause clauses))
          (when (and exhaustive remaining)
            (error "~@<The following ~[~;case is~:;cases are~] not ~
                    handled: ~{~A~^, ~}.~@:>"
                   (length remaining) remaining)))))))

(defmacro cardinality-case (cardinality &body clauses)
  "Execute the clause in CLAUSES corresponding to the value of
   CARDINALITY.

   Elements of CLAUSES are of the form

     (CARDINALITY-DESIGNATOR &body BODY)

   where CARDINALITY-DESIGNATOR is one of the cardinality designators
   1 ? *, :map or a list of some of these. Alternatively,
   CARDINALITY-DESIGNATOR can be (:map KEY-VAR) in which case KEY-VAR
   will be bound to KEY in a (:map KEY) cardinality designator and
   accessible in the corresponding BODY."
  (expand-cardinality-case cardinality clauses nil))

(defmacro cardinality-ecase (cardinality &body clauses)
  "Like `cardinality-case' but signal a compile-time error if CLAUSES
   does not handle all cardinalities."
  (expand-cardinality-case cardinality clauses t))
