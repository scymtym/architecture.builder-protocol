;;;; list-builder.lisp --- Represents constructed results as nested lists.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
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
;;;;     designated relation with the node in question.
;;;;
;;;;   INITARGS is a plist of arbitrary properties associated to the
;;;;   node in question.
;;;;
;;;; For example, a node of a hypothetical kind :my-node with a
;;;; hypothetical :child relation could look like this:
;;;;
;;;;   (:my-node (:child ((:my-child () :bounds (3 . 4))))
;;;;             :name   "a-my-node-instance"
;;;;             :bounds (1 . 5))

(cl:in-package #:builder-protocol)

(defmethod make-node ((builder (eql 'list)) (kind t) &rest initargs &key)
  (list* kind '() initargs))

(defmethod finish-node ((builder (eql 'list)) (kind t) (node t))
  node)

(defmethod relate ((builder (eql 'list)) (relation t) (left t) (right t) &key)
  (appendf (getf (second left) relation) (list right))
  left)

(defmethod relate ((builder (eql 'list)) (relation t) (left t) (right null) &key)
  left)
