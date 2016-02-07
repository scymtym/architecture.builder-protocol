;;;; mixins.lisp --- Mixin classes for builder classes.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol)

;;; Delayed nodes represent calls to `make-node', `relate',
;;; etc. Delayed nodes can be constructed in any order (parents before
;;; children, children before parents or some combination). The nodes
;;; form a tree that can be walked in an arbitrary order.

(defstruct (argumented (:constructor nil) (:predicate nil) (:copier nil))
  (arguments () :type list :read-only t))

(defstruct (delayed-node
             (:include argumented)
             (:constructor make-delayed-node (kind arguments))
             (:copier nil))
  (kind      nil            :read-only t)
  (relations ()  :type list))

(defstruct (delayed-relation
             (:include argumented)
             (:constructor make-delayed-relation (kind node arguments))
             (:copier nil))
  (kind nil :read-only t)
  (node nil :read-only t))

;;; `delaying-mixin'
;;;
;;; Builds a complete tree of delayed nodes.

(defclass delaying-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into builder classes that have
    to construct a tree of delayed nodes before doing their respective
    actual processing."))

(defmethod make-node ((builder delaying-mixin) (kind t)
                      &rest initargs &key)
  (make-delayed-node kind initargs))

(defmethod relate ((builder delaying-mixin) (relation t) (left t) (right t)
                   &rest args &key)
  (push (make-delayed-relation relation right args)
        (delayed-node-relations left))
  left)

;;; `order-forcing-mixin'

(defclass order-forcing-mixin ()
  ((target :initarg  :target
           :reader   builder-target
           :documentation
           "Stores the builder that should be fed `make-node',
            `relate', etc. calls in top-down order."))
  (:default-initargs
   :target (error "~@<Missing required initarg for class ~S: ~S.~@:>"
                  'order-forcing-mixin :target))
  (:documentation
   "This class is intended to be mixed into builder classes that have
    to process nodes in a particular order.

    In combination with `delaying-mixin', this makes the builder
    independent of the order of the original `make-node' and `relate'
    calls."))

(defmethod finish ((builder order-forcing-mixin) (result cons))
  (let ((target (builder-target builder))
        (visit  (builder-visit-function builder)))
    (with-builder (target)
      (apply #'values (funcall visit (first result)) (rest result)))))
