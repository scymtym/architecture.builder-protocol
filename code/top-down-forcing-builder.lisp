;;;; top-down-forcing-builder.lisp --- Forces top-down processing of nodes.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
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

;;; `top-down-forcing-builder'

(defclass top-down-forcing-builder (delaying-mixin
                                    order-forcing-mixin)
  ()
  (:documentation
   "This builder can act as a proxy for other builders which require
    nodes to be created from the root downward."))

(defmethod builder-visit-function ((builder top-down-forcing-builder))
  (labels ((relate-child (relation left)
             (let ((relation  (delayed-relation-kind relation))
                   (right     (delayed-relation-node relation))
                   (arguments (delayed-relation-arguments relation)))
               (apply #'relate* relation left (visit right) arguments)))
           (visit (node)
             (let* ((kind      (delayed-node-kind node))
                    (initargs  (delayed-node-arguments node))
                    (relations (delayed-node-relations node))
                    (node      (apply #'make-node* kind initargs))
                    (node      (reduce #'relate-child relations
                                       :initial-value node :from-end t)))
               (finish-node* kind node))))
    #'visit))
