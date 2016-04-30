;;;; protocol.lisp --- Protocol provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol)

;;; Types

(deftype relation-cardinality ()
  "Cardinality of a relation between nodes.

   ?            Zero or one \"right\" nodes can be related to the
                \"left\" node.

   1            Exactly one \"right\" node is related to the \"left\"
                node.

   *            Zero or more \"right\" nodes can be related to the
                \"left\" node.

   (:map . KEY) Zero or more \"right\" nodes can be related to the
                left node with the additional constraint that the
                relation parameters for each such node must contain a
                unique value for the key KEY.

   See `relate' for \"left\" and \"right\" node roles."
  '(or (member ? 1 *) (cons (eql :map))))

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
;;;   finish builder values                             [generic function]
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

(defgeneric finish (builder values)
  (:documentation
   "Finalize and return VALUES produced by BUILDER.

    The default method just returns VALUES."))

(defgeneric wrap (builder thunk)
  (:documentation
   "Call THUNK with an appropriate dynamic environment for BUILDER.

    For example, could bind special variables around the call.

    Default methods for `cl:function' and `cl:symbol' just call
    THUNK."))

(defgeneric make-node (builder kind &rest initargs ; TODO rename initargs?
                       &key &allow-other-keys)
  (:documentation
   "Use BUILDER to make a result tree node of kind KIND and return it.

    As a convention, when supplied, the value of the :bounds keyword
    argument is of the form (START . END) and can be used to indicate
    the input range for which the tree is constructed."))

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
(defmethod finish ((builder t) (result cons))
  (values-list result))

;; No action, just call the function.
(defmethod wrap ((builder t) (thunk function))
  (let ((*builder* builder))
    (multiple-value-list (funcall thunk builder))))

(defmethod wrap ((builder t) (thunk symbol))
  (let ((*builder* builder))
    (multiple-value-list (funcall thunk builder))))

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
                         &key &allow-other-keys)
  "Convenience function for constructing and immediately finishing a
   node."
  (finish-node builder kind (apply #'make-node builder kind initargs)))

(defun make+finish-node+relations (builder kind initargs relations)
  "Use BUILDER to create a KIND, INITARGS node, relate it via RELATIONS.

   RELATIONS is a list of relation specifications of the form

     (CARDINALITY RELATION-KIND RIGHT &rest ARGS)

   which are translated into `relate' calls in which the created node
   is the \"left\" argument to `relate'. CARDINALITY has to be of type
   `relation-cardinality' and is interpreted as follows:

   ?            RIGHT is a single node or nil.

   1            RIGHT is a single node.

   *            RIGHT is a (possibly empty) sequence of nodes.

   (:map . KEY) RIGHT is a single node that should be associated to
                the mapping key that is the value of KEY in the ARGS
                plist for RIGHT.

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
                       (destructuring-bind (cardinality relation right &rest args) spec
                         (cardinality-ecase cardinality
                           ((1 ?)
                            (add-relation/one left relation right args))
                           (*
                            (add-relation/sequence left relation right args))
                           ((:map key)
                            (unless (getf args key)
                              (error "~@<~S key ~S is missing in ~
                                      relation arguments ~S.~@:>"
                                     :map key args))
                            (add-relation/one left relation right args)))))
                     relations :initial-value left)))
    (finish-node
     builder kind
     (add-relations (apply #'make-node builder kind initargs) relations))))

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
  (:argument-precedence-order node builder)
  (:documentation
   "Return the kind of NODE w.r.t. BUILDER.

    The return value is EQ to the KIND argument used to create NODE
    with BUILDER."))

(defgeneric node-initargs (builder node)
  (:argument-precedence-order node builder)
  (:documentation
   "Return a plist of initargs for NODE w.r.t. BUILDER.

    The returned list is EQUAL to the list of keyword arguments pass
    to the MAKE-NODE call that, using BUILDER, constructed NODE."))

(defgeneric node-relations (builder node)
  (:argument-precedence-order node builder)
  (:documentation
   "Return a list of relations of NODE w.r.t. BUILDER.

    Each relation is of one of the forms

      RELATION-NAME
      (RELATION-NAME . CARDINALITY)

    where RELATION-NAME names the relation and CARDINALITY is of type
    `relation-cardinality'. When the first form is used,
    i.e. CARDINALITY is not present, it is assumed to be
    `*'. CARDINALITY values are interpreted as follows:

      ?            The relation designated by RELATION-NAME with NODE
                   as the \"left\" node has zero or one \"right\"
                   nodes.

      1            The relation designated by RELATION-NAME with NODE
                   as the \"left\" node has exactly one \"right\"
                   node.

      *            The relation designated by RELATION-NAME with NODE
                   as the \"left\" node has zero or more \"right\"
                   nodes.

      (:map . KEY) The relation designated by RELATION-NAME with NODE
                   as the \"left\" node has zero or more \"right\"
                   nodes with the additional constraint that the
                   relation parameters for each such node must contain
                   a unique value for the key KEY.

    . This cardinality information is reflected by the return values
    of (node-relation BUILDER RELATION-NAME NODE)."))

(defgeneric node-relation (builder relation node)
  (:argument-precedence-order node relation builder)
  (:documentation
   "Return two values: 1) a list of nodes related to NODE via RELATION
    w.r.t. BUILDER 2) a same-length list of arguments of the
    relations.

    Each element in the list of relation arguments is EQUAL to the
    list of arguments passed to the RELATE call that, using BUILDER,
    established the relation between NODE and the related node."))

;; Default behavior

(defmethod node-initargs ((builder t) (node t))
  '())

(defmethod node-relations ((builder t) (node t))
  '())

;;; Node walking protocol
;;;
;;; Built on top of the un-build protocol, this protocol allows trees
;;; of nodes constructed by a given builder to be walked in generic
;;; fashion.
;;;
;;; Concretely, the higher-order function `walk-nodes' accepts a
;;; function which it calls on (potentially) each node of the
;;; tree. Node kinds, initargs and relations are determined using the
;;; known builder and the un-build protocol and made available to the
;;; function.

(defgeneric walk-nodes (builder function root)
  (:documentation
   "Call FUNCTION on nodes of the tree ROOT constructed by BUILDER.

    Return whatever FUNCTION returns when called for ROOT.

    The lambda-list of FUNCTION must be compatible to

      (recurse relation relation-args node kind relations
       &rest initargs)

    where RELATION and RELATION-ARGS are the relation and its
    arguments connecting NODE to the previously visited node,

    NODE is the node currently being visited,

    KIND is the kind returned by `node-kind' for BUILDER and NODE.

    RELATIONS are the relations returned by `node-relations' for
    BUILDER and NODE.

    INITARGS are the initargs returned by `node-initargs' for BUILDER
    and NODE.

    RECURSE is a function with the lambda-list

      (&key relations function)

    that can be called, optionally with a list of relations, to
    traverse the nodes related to NODE by that relation. If a list of
    relations is not supplied via the :relations keyword parameter,
    all relations are traversed. The :function keyword parameter
    allows performing the traversal with a different function instead
    of FUNCTION. Calls of this function return a list of elements each
    of which is the result for the corresponding element of
    RELATIONS. The result for a relation is either the return value of
    FUNCTION if the cardinality of the relation is 1 or ? or a list of
    such return values if the cardinality is * or :map.

    If FUNCTION is an instance of `peeking', call the \"peeking\"
    function stored in FUNCTION before the ordinary walk
    function (also stored in FUNCTION) is called. The lambda-list of
    the \"peeking\" function must be compatible to

      (builder relation relation-args node)

    (i.e. it does not receive kind, initargs or relations). This
    function can control whether NODE should be processed normally,
    replaced with something else, processed with different builder or
    ignored: Its return values are interpreted as follows:

    NIL

      Store processing of NODE, in particular do not call `node-kind',
      `node-relations', `node-initargs' or the walk function for NODE.

    T [* * * BUILDER]

      Continue processing as if there was no \"peeking\" function.

      If non-NIL, BUILDER specifies a builder that should be used
      instead of the current builder to process the current node and
      its ancestors.

    INSTEAD KIND INITARGS RELATIONS [BUILDER]

      Continue processing as if NODE had been replaced by INSTEAD and
      builder had returned KIND, INITARGS and RELATIONS. In particular
      do not call `node-kind', `node-relations', `node-initargs' for
      NODE.

      If non-NIL, BUILDER specifies a builder that should be used
      instead of the current builder to process INSTEAD and its
      ancestors.

    Depending on FUNCTION, potentially return a list-of-lists of the
    same shape as the traversed tree containing return values of
    FUNCTION."))

;; Default methods

(declaim (ftype (function ((or null function) function t t)
                          (values t &optional))
                %walk-nodes))
(defun %walk-nodes (peeking-function walk-function builder root)
  (labels
      ((walk-node (walk-function builder relation relation-args node)
         (declare (type function walk-function))
         (multiple-value-bind (instead kind initargs relations new-builder)
             (if peeking-function
                 (funcall peeking-function builder relation relation-args node)
                 t)
           (when new-builder
             (setf builder new-builder))
           (case instead
             ((nil)
              (return-from walk-node))
             ((t)
              (setf kind      (node-kind builder node)
                    initargs  (node-initargs builder node)
                    relations (node-relations builder node)))
             (t
              (setf node instead)))
           (flet ((recurse (&key (relations relations) (function walk-function))
                    (loop :for relation-and-cardinality :in relations :collect
                       (multiple-value-bind (relation cardinality)
                           (normalize-relation relation-and-cardinality)
                         (multiple-value-bind (targets args)
                             (node-relation builder relation node)
                           (cardinality-ecase cardinality
                             (?
                              (when targets
                                (walk-node function builder
                                           relation args targets)))
                             (1
                              (walk-node function builder
                                         relation args targets))
                             ((* :map)
                              (when targets
                                (mapcar (curry #'walk-node
                                               function builder relation)
                                        (or args (load-time-value
                                                  (circular-list '()) t))
                                        targets)))))))))
             (declare (dynamic-extent #'recurse))
             (apply walk-function #'recurse
                    relation relation-args node kind relations initargs)))))
    (declare (dynamic-extent #'walk-node))
    (walk-node walk-function builder nil '() root)))

(defmethod walk-nodes ((builder t) (function t) (root t))
  (walk-nodes (ensure-function function) builder root))

(defmethod walk-nodes ((builder t) (function function) (root t))
  (%walk-nodes nil function builder root))

(defstruct (peeking (:constructor peeking (peeking-function walk-function))
                    (:predicate nil)
                    (:copier nil))
  "Allows specifying a peeking function in conjunction with the walk function."
  (peeking-function nil :type function :read-only t)
  (walk-function    nil :type function :read-only t))

(defmethod walk-nodes ((builder t) (function peeking) (root t))
  (%walk-nodes (peeking-peeking-function function)
               (peeking-walk-function function)
               builder root))

;;; Abbreviated versions of build, "un-build" and `walk-nodes' methods

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
  (define-abbreviation make+finish-node+relations (kind initargs relations))

  (define-abbreviation node-kind (node))
  (define-abbreviation node-initargs (node))
  (define-abbreviation node-relations (node))
  (define-abbreviation node-relation (relation node))

  (define-abbreviation walk-nodes (function root)))
