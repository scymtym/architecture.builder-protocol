;;;; util.lisp --- Utilities used in the universal-builder module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.universal-builder)

(declaim (inline ensure-finalized))

(defun ensure-finalized (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  class)

(defun slot-information (slot-definition)
  (let* ((name        (c2mop:slot-definition-name slot-definition))
         (initarg     (first (c2mop:slot-definition-initargs
                              slot-definition)))
         (type        (c2mop:slot-definition-type slot-definition))
         (cardinality (slot-type->cardinality type)))
    (list name initarg type cardinality)))

(defun slot-type->cardinality (type)
  (cond
    ;; Helper for recursive calls with things like
    ;; `(and (not null) ,type).
    ((type= type nil)
     nil)
    ((type= type t)
     1)
    ((subtypep type 'hash-table)
     '(:map . :key))
    ((subtypep type '(or standard-object structure-object))
     1)
    ((and (type= `(and ,type null) 'null)
          (eql (slot-type->cardinality `(and (not null) ,type)) 1))
     '?)
    ((and (subtypep type '(or list vector))
          (not (subtypep type '(or null string (array (unsigned-byte 8) (*))))))
     '*)))

(declaim (inline initarg-slot? relation-slot?))

(defun initarg-slot? (slot)
  (c2mop:slot-definition-initargs slot))

(defun relation-slot? (slot)
  (and (c2mop:slot-definition-initargs slot)
       (slot-type->cardinality (c2mop:slot-definition-type slot))))

(defun class-scalar-and-relation-slots (class)
  (loop :for slot :in (c2mop:class-slots (ensure-finalized class))
     :if (relation-slot? slot)
     :collect slot :into relation-slots
     :else :if (initarg-slot? slot)
     :collect slot :into scalar-slots
     :finally (return (values scalar-slots relation-slots))))
