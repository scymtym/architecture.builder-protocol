;;;; util.lisp --- Utilities provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2015-2022 Jan Moringen
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
  "Execute the clause in CLAUSES which corresponds to the value of
   CARDINALITY.

   CARDINALITY is evaluated and the resulting value is used to select
   a clause.

   Elements of CLAUSES are of the form

     (CARDINALITY-DESIGNATOR-OR-LIST &body BODY)

   where CARDINALITY-DESIGNATOR-OR-LIST is either

   1. One of the cardinality designators 1, ?, *, `:map'

      A clause of this form matches if CARDINALITY evaluates to the
      specified cardinality designator or if CARDINALITY evaluates
      to (:map SOME-KEY) and CARDINALITY-DESIGNATOR-OR-LIST is `:map'.

   2. A list of some of the cardinality designators mentioned above

      A clause of this form matches if CARDINALITY evaluates to any of
      the cardinality designators mentioned in
      CARDINALITY-DESIGNATOR-OR-LIST or if CARDINALITY evaluates
      to (:map SOME-KEY) and `:map' is an element of
      CARDINALITY-DESIGNATOR-OR-LIST.

   3. An expression of the form (:map . KEY-VAR)

      A clause of this form matches if the value of CARDINALITY is of
      the form (:map . SOME-KEY). During the evaluation of the BODY of
      the clause, KEY-VAR will be bound to SOME-KEY."
  (expand-cardinality-case cardinality clauses nil))

(defmacro cardinality-ecase (cardinality &body clauses)
  "Like `cardinality-case' but signal a compile-time error if CLAUSES
   does not handle all cardinalities."
  (expand-cardinality-case cardinality clauses t))
