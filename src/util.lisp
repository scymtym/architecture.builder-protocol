;;;; util.lisp --- Utilities provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
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
