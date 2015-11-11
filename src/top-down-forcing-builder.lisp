;;;; top-down-forcing-builder.lisp --- Represents constructed results as nested lists.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol)

;;;; The top-down-forcing builder can act as a proxy for other
;;;; builders which require nodes to be created from the root downward
;;;; (as opposed to creating children before their parents). This is
;;;; achieved by creating a tree of "delayed" nodes (can be done
;;;; without top-down or bottom-up ordering constraints) which just
;;;; record `make-node', `relate', etc. calls for later "replay" in
;;;; the desired top-down order.

;;; A delayed node represents calls to `make-node', `relate',
;;; etc. Delayed nodes can be constructed in any order (parents before
;;; children, children before parent or a mix of the two). The nodes
;;; form a tree that can walked in a top-down fashion.

(defstruct (delayed-node (:constructor make-delayed-node (kind initargs))
                         (:copier nil))
  (kind     (error "missing initarg ~S" :kind) :read-only t)
  (initargs '()                                :read-only t)
  (children '()))

(defmethod make-node ((builder (eql 'delayed)) (kind t) &rest initargs &key)
  (make-delayed-node kind initargs))

(defmethod relate ((builder (eql 'delayed)) relation left right
                   &rest args &key)
  (push (list relation right args) (delayed-node-children left))
  left)

;;; The actual builder class.
;;;
;;; Builds a tree complete tree of delayed nodes, then walks it in
;;; top-down order, performing `make-node', `relate', etc. calls on
;;; the target builder.

(defclass top-down-forcing-builder ()
  ((target :initarg  :target
           :reader   builder-target
           :documentation
           "Stores the builder that should be fed `make-node',
            `relate', etc. calls in top-down order."))
  (:default-initargs
   :target (error "~@<Missing required initarg for class ~S: ~S.~@:>"
                  'top-down-forcing-builder :target))
  (:documentation
   "This builder can act as a proxy for other builders which require
    nodes to be created from the root downward."))

(defmethod make-node ((builder top-down-forcing-builder) (kind t)
                      &rest initargs &key)
  (apply #'make-node 'delayed kind initargs))

(defmethod relate ((builder top-down-forcing-builder)
                   (relation t) (left t) (right t)
                   &rest args &key)
  (apply #'relate 'delayed relation left right args))

(defmethod finish ((builder top-down-forcing-builder) (result cons))
  (labels ((relate-child (left relation)
             (destructuring-bind (relation right args) relation
               (apply #'relate *builder* relation left (visit right) args)))
           (visit (node)
             (let* ((kind     (delayed-node-kind node))
                    (initargs (delayed-node-initargs node))
                    (children (delayed-node-children node))
                    (node     (apply #'make-node *builder* kind initargs))
                    (node     (reduce #'relate-child (reverse children)
                                      :initial-value node)))
               (finish-node *builder* kind node))))
    (declare (dynamic-extent #'relate-child #'visit))
    (with-builder ((builder-target builder))
      (apply #'values (visit (first result)) (rest result)))))
