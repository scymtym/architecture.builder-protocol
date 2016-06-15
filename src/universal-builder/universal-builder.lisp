;;;; universal-builder.lisp --- (Un)builder for arbitrary standard-objects.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.universal-builder)

;;; `universal-builder'

(defclass universal-builder (opaque-fallback-mixin)
  ()
  (:documentation
   "A builder for standard objects."))

;;; Build protocol

(defmethod make-node ((builder universal-builder) (kind t)
                      &rest initargs &key)
  (apply #'make-instance kind initargs))

(defmethod relate ((builder  universal-builder)
                   (relation t)
                   (left     t)
                   (right    t)
                   &rest args &key)
  (let ((relation-slots (nth-value 1 (class-scalar-and-relation-slots
                                      (class-of left)))))
    (destructuring-bind (name initarg type cardinality)
        (slot-information (or (find-slot-by-initarg relation relation-slots)
                              (error "~@<No slot for relation ~S in ~A.~@:>"
                                     relation left)))
      (declare (ignore initarg))
      (cardinality-case cardinality
        ((? 1)
         (setf (slot-value left name) right))
        (*
         (ecase type
           (list   (appendf (slot-value left name) (list right)))
           (vector (vector-push-extend right (slot-value left name)))))
        (:map
         (let ((key (getf args :key)))
           (setf (gethash key (slot-value left name)) right))))))
  left)

;;; "un-build" protocol

(defmethod node-kind ((builder universal-builder) (node standard-object))
  (class-name (class-of node)))

(defmethod node-initargs ((builder universal-builder) (node standard-object))
  (loop :for slot :in (class-scalar-and-relation-slots (class-of node))
     :for (name initarg) = (slot-information slot)
     :when (slot-boundp node name)
     :collect initarg :and :collect (slot-value node name)))

(defmethod node-relations ((builder universal-builder) (node standard-object))
  (loop :for slot :in (nth-value 1 (class-scalar-and-relation-slots
                                    (class-of node)))
     :for (nil initarg nil cardinality) = (slot-information slot)
     :collect (cons initarg cardinality)))

(defmethod node-relation ((builder  universal-builder)
                          (relation symbol)
                          (node     standard-object))
  (let ((relation-slots (nth-value 1 (class-scalar-and-relation-slots
                                      (class-of node)))))
    (when-let ((slot (find-slot-by-initarg relation relation-slots)))
      (destructuring-bind (name initarg type cardinality)
          (slot-information slot)
        (declare (ignore initarg type))
        (when (slot-boundp node name)
          (let ((value (slot-value node name)))
            (cardinality-case cardinality
              ((? 1 *)
               value)
              (:map
               (loop :for key :being :the :hash-key :in value
                  :using (:hash-value value)
                  :collect (list :key key) :into keys
                  :collect value :into values
                  :finally (return (values values keys)))))))))))

;;; Utilities

(defun find-slot-by-initarg (initarg slots)
  (find initarg slots
        :test #'eq
        :key  (compose #'first #'c2mop:slot-definition-initargs)))
