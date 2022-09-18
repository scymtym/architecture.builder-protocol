;;;; list-builder.lisp --- Represents constructed results as nested lists.
;;;;
;;;; Copyright (C) 2014-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This builder produces a universally applicable list-based
;;;; representation mainly suited to interactive development,
;;;; debugging and unit testing.
;;;;
;;;; Nodes are represented as lists of the following form:
;;;;
;;;;   (KIND RELATIONS &rest INITARGS)
;;;;
;;;; where
;;;;
;;;;   KIND is a keyword designating the kind of the node. The valid
;;;;   kinds and respective semantics depend on the context.
;;;;
;;;;   RELATIONS is a plist of elements of the form
;;;;
;;;;     RELATION-KIND TARGETS
;;;;
;;;;   where
;;;;
;;;;     RELATION-KIND is a keyword designating the kind of the
;;;;     relation. The valid kinds and respective semantics depend on
;;;;     the context.
;;;;
;;;;     TARGETS is a list of nodes that form elements of the
;;;;     designated relation with the node in question. Each entry is
;;;;     of the form
;;;;
;;;;       (TARGET-NODE . RELATION-ARGUMENTS)
;;;;
;;;;   INITARGS is a plist of arbitrary properties associated with the
;;;;   node in question.
;;;;
;;;; For example, a node of a hypothetical kind :my-node with a
;;;; hypothetical :child relation could look like this:
;;;;
;;;;   (:my-node (:child (((:my-child () :bounds (3 . 4)) . ())))
;;;;             :name   "a-my-node-instance"
;;;;             :bounds (1 . 5))

(cl:in-package #:architecture.builder-protocol)

;;; Implementation of the build protocol

(defmethod make-node ((builder (eql 'list)) (kind t) &rest initargs &key)
  (list* kind '() initargs))

(defmethod finish-node ((builder (eql 'list)) (kind t) (node t))
  node)

(defmethod relate ((builder (eql 'list)) (relation t) (left t) (right t)
                   &rest args &key)
  (let* ((relations (second left))
         (cell      (if relations
                        (loop :for previous-cell = nil :then cell
                              :for cell :on relations :by #'cddr
                              :when (equal (first cell) relation)
                              :do (return cell)
                              :finally (let ((new-cell (list relation '())))
                                         (setf (cddr previous-cell) new-cell)
                                         (return new-cell)))
                        (let ((new-cell (list relation '())))
                          (setf (second left) new-cell)
                          new-cell))))
    (appendf (second cell) (list (cons right args))))
  left)

(defmethod relate ((builder (eql 'list)) (relation t) (left t) (right null) &key)
  left)

;;; Implementation of the "un-build" protocol

(defmethod node-kind ((builder (eql 'list)) (node cons))
  (first node))

(defmethod node-initargs ((builder (eql 'list)) (node cons))
  (nthcdr 2 node))

(defmethod node-relations ((builder (eql 'list)) (node cons))
  (loop :for (key) :on (second node) :by #'cddr :collect key))

(defmethod node-relation ((builder (eql 'list)) (relation t) (node cons))
  (multiple-value-bind (cardinality entries)
      (loop :for (key entries) :on (second node) :by #'cddr
            :do (cond ((and (consp key) (consp relation))
                       (when (equal key relation)
                         (return (values (cdr key) entries))))
                      ((consp key)
                       (when (eq (car key) relation)
                         (return (values (cdr key) entries))))
                      ((consp relation)
                       (when (eq key (car relation))
                         (return (values '* entries))))
                      (t
                       (when (eq key relation)
                         (return (values '* entries))))))
    (when entries
      (cardinality-ecase cardinality
        ((1 ?)
         (values (car (first entries)) (cdr (first entries))))
        ((* :map)
         (values (mapcar #'car entries) (mapcar #'cdr entries)))))))
