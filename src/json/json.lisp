;;;; json.lisp --- JSON export of builder-based trees.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.json)

(declaim (inline %maybe-escape-boolean %maybe-unescape-boolean))

(defun %maybe-escape-boolean (value)
  (case value
    ((t)   '%t)
    ((nil) '%nil)
    (t     value)))

(defun %maybe-unescape-boolean (value)
  (case value
    ((%t)   t)
    ((%nil) nil)
    (t      value)))

(declaim (ftype (function (t) (values (or null function) &optional))
                %maybe-ensure-function))
(defun %maybe-ensure-function (thing)
  (when thing (ensure-function thing)))

(declaim (ftype (function (t &key (:cache-size non-negative-integer))
                          (values function &optional))
                make-symbol-transform)
         (ftype (function (&key (:cache-size non-negative-integer))
                          (values function &optional))
                default-symbol-transform default-key-and-value-transform))

(defun make-symbol-transform (function &key (cache-size 1024))
  (let ((function (ensure-function function))
        (cache    (make-hash-table :test #'eq)))
    (lambda (symbol)
      (when (>= (hash-table-count cache) cache-size)
        (clrhash cache))
      (ensure-gethash symbol cache (funcall function symbol)))))

(defun default-symbol-transform (&key (cache-size 1024))
  (make-symbol-transform
   (lambda (symbol)
     (cl-json:lisp-to-camel-case (string symbol)))
   :cache-size cache-size))

(defun default-key-and-value-transform (&key (cache-size 1024))
  (let ((symbol-transform (default-symbol-transform
                              :cache-size cache-size)))
    (lambda (key value)
      (values (funcall symbol-transform key) value))))

(defun default-kind-key-and-value-transform ()
  (lambda (key value)
    (declare (ignore key))
    (values "__kind" value)))

(defun default-member-key-transform ()
  (lambda (key)
    (if (stringp key)
        key
        (princ-to-string key))))

(defun make-walk-function
    (&key
     (kind-transform     (default-kind-key-and-value-transform))
     (initarg-transform  (default-key-and-value-transform))
     (relation-transform (default-symbol-transform))
     (member-transform   (default-member-key-transform)))
  (let ((kind-transform     (%maybe-ensure-function kind-transform))
        (initarg-transform  (%maybe-ensure-function initarg-transform))
        (relation-transform (%maybe-ensure-function relation-transform))
        (member-transform   (ensure-function member-transform)))
    (labels ((encode-property (key value function)
               (multiple-value-bind (key value) (funcall function key value)
                 (when key
                   (json:encode-object-member key value))))
             (encode-initargs (initargs)
               (when initarg-transform
                 (loop :for (key value) :on initargs :by #'cddr :do
                    (encode-property key value initarg-transform))))
             (encode-as-singleton (&rest args)
               (apply #'encode-node args))
             (encode-as-array-member (&rest args)
               (json:as-array-member ()
                 (apply #'encode-node args)))
             (encode-as-object-member (key-key recurse relation relation-args
                                       &rest args)
               (let ((key (getf relation-args key-key)))
                 (json:as-object-member ((funcall member-transform key))
                   (apply #'encode-node recurse relation relation-args args))))
             (encode-relation (recurse relation-and-cardinality function)
               (let ((relations (list relation-and-cardinality)))
                 (declare (dynamic-extent relations))
                 (funcall recurse :relations relations :function function)))
             (encode-relations (recurse relations)
               (declare (type function recurse))
               (loop :for relation-and-cardinality :in relations :do
                  (multiple-value-bind (relation cardinality)
                      (normalize-relation relation-and-cardinality)
                    (when-let ((relation-key (funcall relation-transform relation)))
                      (cardinality-ecase cardinality
                        ((1 ?)
                         (encode-relation
                          recurse relation-and-cardinality
                          (lambda (&rest args)
                            (json:as-object-member (relation-key)
                              (apply #'encode-as-singleton args)))))
                        (*
                         (json:as-object-member (relation-key)
                           (json::with-array ()
                             (encode-relation recurse relation-and-cardinality
                                              #'encode-as-array-member))))
                        ((:map key-key)
                         (json:as-object-member (relation-key)
                           (json:with-object ()
                             (encode-relation recurse relation-and-cardinality
                                              (curry #'encode-as-object-member
                                                     key-key))))))))))
             (encode-node (recurse relation relation-args node kind relations
                           &rest initargs &key &allow-other-keys)
               (declare (ignore relation relation-args))
               (if (eq kind 'raw)
                   (json:encode-json (%maybe-unescape-boolean node))
                   (json:with-object ()
                     (when kind-transform
                       (encode-property "kind" kind kind-transform))
                     (when initarg-transform
                       (encode-initargs initargs))
                     (when relation-transform
                       (encode-relations recurse relations))))))
      #'encode-node)))

(defun make-serializer (&rest args &key
                        kind-transform initarg-transform
                        relation-transform member-transform
                        peek-function)
  "Return a function that can be used as the fourth argument to
   `serialize-using-serializer'.

   The following keyword parameters control some details of the
   mapping:

   If non-nil, KIND-TRANSFORM is function with lambda-list

     (key value)

   where KEY is \"kind\" and VALUE is the kind of the node being
   serialized. The function returns two values: 1) the property name
   that should be used to encode the kind 2) the desired value of the
   property. If nil, no properties describing node kinds are emitted.

   If non-nil, INITARG-TRANSFORM is a function with lambda-list

     (key value)

   where KEY is the name of the initarg and VALUE is its value. The
   function returns two values: 1) the property name that should be
   used to encode the initarg 2) the desired value of the property. If
   nil, no properties are emitted for initargs.

   If non-nil, RELATION-TRANSFORM is a function with lambda-list

     (relation)

   where RELATION is the name of the relation. The function returns
   the name of the property that should be used to encode the
   relation. If nil, no properties are emitted for relations.

   If supplied, MEMBER-TRANSFORM is a function with lambda-list

     (key)

   where KEY is the value of the KEY-KEY relation argument in a (:map
   KEY-KEY) relation. The function returns the name of the property
   that should be used to encode the mapping entry.

   PEEK-FUNCTION behaves like the corresponding keyword parameter of
   `architecture.builder-protocol:walk-nodes' with the following
   addition: for return values of the form

     INSTEAD 'architecture.builder-protocol.json:raw * * *

   INSTEAD is directly inserted into the output instead of the node."
  (declare (ignore kind-transform initarg-transform
                   relation-transform member-transform))
  (let ((peek-function  (%maybe-ensure-function peek-function))
        (visit-function (apply #'make-walk-function
                               (remove-from-plist args :peek-function))))
    (if peek-function
        (locally (declare (type function peek-function))
          (flet ((peek-wrapper (builder relation relation-args node)
                   (multiple-value-bind (instead kind initargs relations builder*)
                       (funcall peek-function builder relation relation-args node)
                     (case kind
                       (raw
                        (values (%maybe-escape-boolean instead) 'raw))
                       (t
                        (values instead kind initargs relations builder*))))))
            (peeking #'peek-wrapper visit-function)))
        visit-function)))

(defun serialize-using-serializer (builder tree stream serializer)
  "Write a JSON rendering of TREE to STREAM using BUILDER and
   SERIALIZER.

   SERIALIZER can be constructed via `make-serializer'."
  (let ((json:*json-output* stream))
    (walk-nodes builder serializer tree)))

(defun serialize (builder tree stream
                  &rest args &key
                  kind-transform initarg-transform
                  relation-transform member-transform
                  peek-function)
  "Write a JSON rendering of TREE to STREAM using BUILDER.

   See package documentation for a general description of the mapping
   to JSON.

   See `make-serializer' for a description of the keyword parameters."
  (declare (ignore kind-transform initarg-transform
                   relation-transform member-transform
                   peek-function))
  (let ((serializer (apply #'make-serializer args)))
    (serialize-using-serializer builder tree stream serializer)))
