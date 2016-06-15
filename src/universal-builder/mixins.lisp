;;;; mixins.lisp --- Mixins for universal builder classes.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.universal-builder)

(defclass opaque-fallback-mixin ()
  ()
  (:documentation
   "Adds to builder classes a fallback behavior for unknown node kinds.

    Nodes of unknown kinds are presented with a kind based on their
    class but without attributes or relations."))

(defmethod node-kind ((builder opaque-fallback-mixin) (node t))
  (class-name (class-of node)))

(defmethod node-initargs ((builder opaque-fallback-mixin) (node t))
  '())

(defmethod node-relations ((builder opaque-fallback-mixin) (node t))
  '())
