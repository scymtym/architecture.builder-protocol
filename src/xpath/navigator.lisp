;;;; navigator.lisp --- A navigator for un-build-based documents.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.xpath)

;;; `navigator' class
;;;
;;; Provides default behaviors for all kinds of nodes.

(defclass navigator ()
  ((builder  :initarg  :builder
             :reader   navigator-builder
             :documentation
             "Stores the builder that should be used for determining
              node kinds, initargs and relations."))
  (:default-initargs
   :builder (required-argument :builder))
  (:documentation
   "An XPath \"navigator\" class for evaluating XPath expressions on
    tree structures for which an builder with a corresponding
    implementation of the un-build protocol is available."))

;; Default behavior

(defmethod node-p-using-navigator ((navigator navigator)
                                   (node      t))
  nil)

(defmethod node-type-p-using-navigator ((navigator navigator)
                                        (node      t)
                                        (type      t))
  nil)

(defmethod namespace-uri-using-navigator ((navigator navigator)
                                          (node      t))
  "")

(defmethod namespace-pipe-using-navigator ((navigator navigator)
                                           (node      t))
  empty-pipe)

(defmethod namespace-prefix-using-navigator ((navigator navigator)
                                             (node      t))
  "")

(defmethod qualified-name-using-navigator ((navigator navigator)
                                           (node      t))
  (local-name-using-navigator navigator node))

(defmethod hash-key-using-navigator ((navigator navigator)
                                     (node      t))
  node)

(defmethod node-equal-using-navigator ((navigator navigator)
                                       (node      t)
                                       (other     t))
  (eq node other))

(defmethod node-text-using-navigator ((navigator navigator)
                                      (node      t))
  "")
