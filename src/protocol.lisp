;;;; protocol.lisp --- Protocol provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol)

;;; Types

(deftype relation-cardinality ()
  "Cardinality of a relation between nodes.

   ? Zero or one \"right\" nodes can be related to the \"left\" node.

   1 Exactly one \"right\" node is related to the \"left\" node.

   * Zero or more \"right\" nodes can be related to the \"left\" node.

   See `relate' for \"left\" and \"right\" node roles."
  '(member ? 1 *))

;;; Build protocol
;;;
;;; This protocol allows producers such as parsers to construct object
;;; trees or graphs in an abstract fashion, i.e. independent of the
;;; concrete objects being constructing.
;;;
;;; This is achieved through three parts of the protocol:
;;;
;;; * Global processing (default methods suffice in many cases):
;;;
;;;   prepare builder                                   [generic function]
;;;
;;;   finish builder result                             [generic function]
;;;
;;;   wrap builder thunk                                [generic function]
;;;
;;; * Construction of nodes:
;;;
;;;   make-node builder kind &rest initargs             [generic function]
;;;
;;;   finish-node builder kind node                     [generic function]
;;;
;;; * Establishing relations between nodes
;;;
;;;   relate builder relation left right &rest args     [generic function]

(defgeneric prepare (builder)
  (:documentation
   "Prepare BUILDER for result construction, return a builder.

    The default method just returns BUILDER."))

(defgeneric finish (builder result)
  (:documentation
   "Finalize and return RESULT produced by BUILDER.

    The default method just returns RESULT."))

(defgeneric wrap (builder thunk)
  (:documentation
   "Call THUNK with an appropriate dynamic environment for BUILDER.

    For example, could bind special variables around the call.

    Default methods for `cl:function' and `cl:symbol' just call
    THUNK."))

(defgeneric make-node (builder kind &rest initargs ; TODO rename initargs?
                       &key bounds &allow-other-keys)
  (:documentation
   "Use BUILDER to make a result tree node of kind KIND and return it.

    When supplied, BOUNDS is of the form (START . END) and can be used
    to indicate the input range for which the tree is constructed."))

(defgeneric finish-node (builder kind node)
  (:documentation
   "Use BUILDER to perform finalization for NODE and return NODE."))

(defgeneric relate (builder relation left right
                    &rest args &key &allow-other-keys)
  (:documentation
   "Establish RELATION between nodes LEFT and RIGHT and return the
    resulting modified LEFT node (or an appropriate newly created
    object).

    ARGS can be used to supply additional information about the
    relation that is available from neither LEFT nor RIGHT.

    In a typical case, RELATION could be :child, LEFT being the parent
    node and RIGHT being the child node."))

;; Default behavior

;; No preparation.
(defmethod prepare ((builder t))
  builder)

;; No action, just return the result.
(defmethod finish ((builder t) (result t))
  result)

;; No action, just call the function.
(defmethod wrap ((builder t) (thunk function))
  (let ((*builder* builder))
    (funcall thunk builder)))

(defmethod wrap ((builder t) (thunk symbol))
  (let ((*builder* builder))
    (funcall thunk builder)))

;; This allows consumers to omit defining a method on `finish-node'.
(defmethod finish-node ((builder t) (kind t) (node t))
  node)

;; This allows using nil to indicate that no relation should be in
;; situations in which the call to `relate' would be inconvenient to
;; avoid.
(defmethod relate ((builder t) (relation t) (left t) (right null) &key)
  left)

;;; Convenience functions

(defun make+finish-node (builder kind &rest initargs
                         &key bounds &allow-other-keys)
  "Convenience function for constructing and immediately finishing a
   node."
  (declare (ignore bounds))
  (finish-node builder kind (apply #'make-node builder kind initargs)))

(defun make+finish-node+relations (builder kind initargs relations)
  "Use BUILDER to create a KIND, INITARGS node, relate it via RELATIONS.

   RELATIONS is a list of relation specifications of the form

     (CARDINALITY RELATION-KIND RIGHT &rest ARGS)

   which are translated into `relate' calls in which the created node
   is the \"left\" argument to `relate'. CARDINALITY has to be of type
   `relation-cardinality' and is interpreted as follows:

     ? -> RIGHT is a single node or nil.

     1 -> RIGHT is a single node.

     * -> RIGHT is a (possibly empty) sequence of nodes.

   RELATION-KIND does not have to be unique across the elements of
   RELATIONS. This allows multiple \"right\" nodes to be related to
   LEFT via a given RELATION-KIND with CARDINALITY * in multiple
   RELATIONS entries, potentially with different ARGS.

   `finish-node' is called on the created node. The created node is
   returned."
  (labels ((add-relation/one (left relation right args)
             (apply #'relate builder relation left right args))
           (add-relation/sequence (left relation right args)
             (reduce (lambda (left right)
                       (apply #'relate builder relation left right args))
                     right :initial-value left))
           (add-relations (left relations)
             (reduce (lambda (left spec)
                       (destructuring-bind (arity relation right &rest args) spec
                         (ecase arity
                           ((1 ?) (add-relation/one left relation right args))
                           (*     (add-relation/sequence left relation right args)))))
                     relations :initial-value left)))
    (finish-node
     builder kind
     (add-relations (apply #'make-node builder kind initargs) relations))))

;;; Abbreviated versions

(macrolet ((define-abbreviation (name (&rest args))
             (let* ((name* (symbolicate name '#:*))
                    (&rest (position '&rest args))
                    (args1 (subseq args 0 &rest))
                    (rest  (when &rest (nth (1+ &rest) args))))
               `(defun ,name* ,args
                  ,(format nil "Like `~(~A~)' but uses `*builder*' ~
                                instead of accepting a builder ~
                                parameter."
                           name)
                  ,(if rest
                       `(apply #',name *builder* ,@args1 ,rest)
                       `(,name *builder* ,@args1))))))
  (define-abbreviation prepare ())
  (define-abbreviation finish (result))
  (define-abbreviation wrap (thunk))

  (define-abbreviation make-node (kind &rest initargs
                                  &key &allow-other-keys))
  (define-abbreviation finish-node (kind node))
  (define-abbreviation relate (relation left right
                               &rest args &key &allow-other-keys))

  (define-abbreviation make+finish-node (node &rest initargs
                                         &key &allow-other-keys))
  (define-abbreviation make+finish-node+relations (kind initargs relations)))

;;; "Un-build" protocol
;;;
;;; This protocol allows nodes made by a given builder to be
;;; decomposed back into the individual components from which they
;;; were originally constructed. These components are:
;;;
;;; * node kind
;;; * node initargs
;;; * relations to other nodes
;;;
;;; The functions
;;;
;;;   node-kind builder node                            [generic function]
;;;
;;;   node-initargs builder node                        [generic function]
;;;
;;;   node-relations builder node                       [generic function]
;;;
;;;   node-relation builder relation node               [generic function]
;;;
;;; retrieve these components for NODE which must have been
;;; constructed by BUILDER.

(defgeneric node-kind (builder node)
  (:documentation
   "Return the kind of NODE w.r.t. BUILDER.

    The return value is EQ to the KIND argument used to create NODE
    with BUILDER."))

(defgeneric node-initargs (builder node)
  (:documentation
   "Return a plist of initargs for NODE w.r.t. BUILDER.

    The returned list is EQUAL to the list of keyword arguments pass
    to the MAKE-NODE call that, using BUILDER, constructed NODE."))

(defgeneric node-relations (builder node)
  (:documentation
   "Return a list of relations of NODE w.r.t. BUILDER.

    Each relation is of one of the forms

      RELATION-NAME
      (RELATION-NAME . CARDINALITY)

    where RELATION-NAME names the relation and CARDINALITY is of type
    `relation-cardinality'. When the first form is used,
    i.e. CARDINALITY is not present, it is assumed to be
    `*'. CARDINALITY values are interpreted as follows:

      ? -> The relation designated by RELATION-NAME with NODE as the
           \"left\" node has zero or one \"right\" nodes.

      1 -> The relation designated by RELATION-NAME with NODE as the
           \"left\" node has exactly one \"right\" node.

      * -> The relation designated by RELATION-NAME with NODE as the
           \"left\" node has exactly zero or ore \"right\" nodes.

    . This cardinality information is reflected by the return values
    of (node-relation BUILDER RELATION-NAME NODE)."))

(defgeneric node-relation (builder relation node)
  (:documentation
   "Return two values: 1) a list nodes related to NODE via RELATION
    w.r.t. BUILDER 2) a same-length list of arguments of the relations.

    Each element in the list of relation arguments is EQUAL to the
    list of arguments passed to the RELATE call that, using BUILDER,
    established the relation between NODE and the related node."))

;; Default behavior

(defmethod node-initargs ((builder t) (node t))
  '())

(defmethod node-relations ((builder t) (node t))
  '())
