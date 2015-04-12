;;;; protocol.lisp --- Protocol provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol)

;;; Builder protocol
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

     ((1 | *) RELATION-KIND RIGHT &rest ARGS)

   which are translated into `relate' calls in which the created node
   is the \"left\" argument to `relate'.

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
                           (1 (add-relation/one left relation right args))
                           (* (add-relation/sequence left relation right args)))))
                     relations :initial-value left)))
    (finish-node
     builder kind
     (add-relations (apply #'make-node builder kind initargs) relations))))

(defmacro node ((builder kind &rest initargs &key &allow-other-keys)
                &body relations)
  "Use BUILDER to create a KIND, INITARGS node, relate it via RELATIONS.

   BUILDER, KIND and INITARGS are evaluated and passed to `make-node'.

   RELATIONS is a list of relation specifications of the form accepted
   by `make+finish-node+relations'.

   `finish-node' is called on the created node. The created node is
   returned.

   Example:

     (node (:operator :which '+)
       (* :operand (list left right)))"
  (flet ((wrap-in-list (spec)
           (destructuring-bind (arity relation right &rest args) spec
             `(list ',arity ,relation ,right ,@args))))
    `(make+finish-node+relations
      ,builder ,kind (list ,@initargs)
      (list ,@(mapcar #'wrap-in-list relations)))))

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

(defmacro node* ((kind &rest initargs &key &allow-other-keys) &body relations)
  "Like `node' but uses `*builder*' instead of accepting a builder
   parameter."
  `(node (*builder* ,kind ,@initargs)
     ,@relations))

;;; `with-builder'

(defun call-with-builder (builder thunk)
  (finish builder (wrap (prepare builder) thunk)))

(defmacro with-builder (spec &body body)
  "Execute BODY with a builder binding according to SPEC.

   SPEC is either

     (BUILDER-VAR BUILDER-FORM)

       During the evaluation of BODY, BUILDER-VAR is bound to the
       result of evaluating BUILDER-FORM.

     (BUILDER-FORM)

       Before the evaluation of BODY, BUILDER-FORM is evaluated, but
       the constructed builder is only accessible through the
       `*builder*' special variable.

   Note that during the evaluation of BODY the special variable
   `*builder*' is usually bound to the builder constructed by
   BUILDER-FORM (depending on methods on `prepare' and `wrap')."
  (destructuring-bind (builder-var-or-form
                       &optional
                       (builder-form nil builder-form-p))
      (ensure-list spec)
    (multiple-value-bind (builder-form builder-var var-p)
        (if builder-form-p
            (values builder-form builder-var-or-form t)
            (values builder-var-or-form (gensym)))
      (check-type builder-var symbol)
      `(flet ((with-builder-thunk (,builder-var)
                ,@(unless var-p `((declare (ignore ,builder-var))))
                ,@body))
         (declare (dynamic-extent #'with-builder-thunk))
         (call-with-builder ,builder-form #'with-builder-thunk)))))
