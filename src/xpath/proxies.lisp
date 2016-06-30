;;;; proxies.lisp --- Proxy objects for using nodes in XPath evaluation contexts.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.xpath)

;;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro list->instance-pipe ((list element-lambda-list) &body body)
    "Return a pipe that contains elements of LIST, transformed by BODY.

     For the transformation, one element of LIST is destructured
     according to ELEMENT-LAMBDA-LIST (which has to have a `&rest'
     parameter) and processed by BODY. The value returned by BODY
     becomes the pipe element. The value of the `&rest' parameter
     corresponds to the unprocessed rest of LIST."
    (multiple-value-bind (required optional rest)
        (parse-ordinary-lambda-list element-lambda-list)
      (declare (ignore required optional))
      (assert rest)
      (with-unique-names (function-name remainder)
        `(labels ((,function-name (,remainder)
                    (if (not ,remainder)
                        empty-pipe
                        (destructuring-bind ,element-lambda-list ,remainder
                          (make-pipe (progn ,@body)
                                     (,function-name ,rest))))))
           (,function-name ,list))))))

;;; `proxy' class
;;;
;;; Superclass of all proxy node classes. Proxy nodes wrap actual
;;; nodes of a tree structure or represent things as nodes that are
;;; not actually in the tree structure while an XPath query is
;;; evaluated on it. This is necessary because an implementation of
;;; the (un)builder for a particular kind of tree structure may not
;;; support certain required operations such as finding the parent of
;;; a node.

(defstruct (proxy (:constructor nil)
                  (:predicate nil)
                  (:copier nil))
  (parent   nil #|:type proxy|#      :read-only t)
  ;; Memoizes child pipe.
  (children nil :type (or null cons)))

;;; Since proxies can only be constructed by enumerating theirs
;;; parents' children and child pipes are memoized, there should be no
;;; way to obtain different proxy for any value or relation that
;;; already has an associated proxy.
(defmethod node-equal-using-navigator ((navigator navigator)
                                       (node      proxy)
                                       (other     proxy))
  (eq node other))

(defmethod hash-key-using-navigator ((navigator navigator)
                                     (node      proxy))
  node)

(defmethod node-p-using-navigator ((navigator navigator)
                                   (proxy     proxy))
  t)

(defmethod parent-node-using-navigator ((navigator navigator)
                                        (proxy     proxy))
  (proxy-parent proxy))

(defmethod child-pipe-using-navigator :around ((navigator navigator)
                                               (node      proxy))
  (or (proxy-children node)
      (setf (proxy-children node) (call-next-method))))

;;; `document-proxy' class

(defstruct (document-proxy (:include     proxy)
                           (:constructor make-document-proxy (root))
                           (:predicate   nil)
                           (:copier      nil))
  (root nil :read-only t))

(defmethod node-type-p-using-navigator ((navigator navigator)
                                        (proxy     document-proxy)
                                        (type      (eql :document)))
  t)

(defmethod local-name-using-navigator ((navigator navigator)
                                       (proxy     document-proxy))
  "")

(defmethod attribute-pipe-using-navigator ((navigator navigator)
                                           (proxy     document-proxy))
  empty-pipe)

(defmethod child-pipe-using-navigator ((navigator navigator)
                                       (node      document-proxy))
  (make-pipe (make-node-proxy (document-proxy-root node) node)
             empty-pipe))

;;; `valued-proxy' class

(defstruct (valued-proxy (:include     proxy)
                         (:constructor nil)
                         (:predicate   nil)
                         (:copier      nil))
  (value nil :read-only t))

(defmethod node-text-using-navigator ((navigator navigator)
                                      (proxy     valued-proxy))
  (let ((value (valued-proxy-value proxy)))
    (cond
      ((when-let ((printer (find-printer proxy navigator)))
         (locally (declare (type function printer))
           (funcall printer (navigator-builder navigator) value))))
      ((stringp value)
       value)
      (t
       (princ-to-string value)))))

(defmethod find-printer ((thing valued-proxy) (navigator navigator))
  (find-printer (valued-proxy-value thing) navigator))

;;; `node-poxy' class
;;;
;;; Instances wrap nodes of the tree structure mainly adding the
;;; ability to find the parent of a node.

(defstruct (node-proxy (:include valued-proxy)
                       (:constructor make-node-proxy (value &optional parent))
                       (:predicate nil)
                       (:copier nil)))

(defmacro with-node-proxy-access ((name-and-options (navigator proxy))
                                  &body body)
  (destructuring-bind ((name &optional (accessor (symbolicate '#:node- name)))
                       &key (builder (gensym)) (value (gensym)))
      (typecase name-and-options
        ((not cons) (list (list name-and-options)))
        ((cons symbol (cons keyword t))
         (list* (list (first name-and-options)) (rest name-and-options)))
        (t
         name-and-options))
    `(let* ((,builder (navigator-builder ,navigator))
            (,value   (node-proxy-value ,proxy))
            (,name    (,accessor ,builder ,value)))
       ,@body)))

(defmethod node-type-p-using-navigator ((navigator navigator)
                                        (proxy     node-proxy)
                                        (type      (eql :element)))
  t)

(defmethod local-name-using-navigator ((navigator navigator)
                                       (proxy     node-proxy))
  (with-node-proxy-access (kind (navigator proxy))
    (symbol->name kind)))

(defmethod namespace-uri-using-navigator ((navigator navigator)
                                          (proxy     node-proxy))
  (with-node-proxy-access (kind (navigator proxy))
    (symbol->namespace kind)))

(defmethod attribute-pipe-using-navigator ((navigator navigator)
                                           (proxy     node-proxy))
  (with-node-proxy-access (initargs (navigator proxy))
    (list->instance-pipe (initargs (name value &rest rest))
      (make-attribute-proxy name value proxy))))

(defmethod child-pipe-using-navigator ((navigator navigator)
                                       (proxy     node-proxy))
  (with-node-proxy-access
      ((relations :builder builder :value value) (navigator proxy))
    (labels ((pipe-step (relation current-targets current-args remainder)
               (multiple-value-bind (relation* cardinality)
                   (normalize-relation relation)
                 (cond
                   ;; CURRENT-TARGETS and CURRENT-ARGS contain related
                   ;; nodes and corresponding arguments for the current
                   ;; relation RELATION => put the first into the pipe.
                   (current-targets
                    (cardinality-ecase cardinality
                      ((1 ?)
                       (make-pipe
                        (make-relation-proxy
                         relation* current-targets current-args proxy)
                        (pipe-step relation '() '() remainder)))
                      ((* :map)
                       (destructuring-bind (target &rest target-rest)
                           current-targets
                         (destructuring-bind (&optional args &rest args-rest)
                             current-args
                           (make-pipe
                            (make-relation-proxy relation* target args proxy)
                            (pipe-step relation target-rest args-rest remainder)))))))
                   ;; CURRENT-TARGETS and CURRENT-ARGS and thus the
                   ;; current relation RELATION, are exhausted, and
                   ;; there are more relations in REMAINDER => continue
                   ;; with next relation.
                   (remainder
                    (destructuring-bind (next &rest rest) remainder
                      (multiple-value-bind (relation cardinality)
                          (normalize-relation next)
                        (multiple-value-bind (targets args)
                            (node-relation builder relation value)
                          (cardinality-case cardinality
                            ((1 ?)
                             (pipe-step next targets args rest))
                            ((* :map)
                             ;; TODO non-consing sequence iteration
                             (pipe-step next (coerce targets 'list) (coerce args 'list) rest)))))))
                   ;; Current relation is exhausted and there are no
                   ;; more relations in REMAINDER => done.
                   (t
                    empty-pipe)))))
      (pipe-step nil '() '() relations))))

;;; `attribute-proxy'

(defstruct (attribute-proxy
            (:include valued-proxy
             (parent nil #|:type (or node-proxy relation-proxy)|# :read-only t))
            (:constructor make-attribute-proxy (name value parent))
            (:predicate nil)
            (:copier nil))
  (name  nil :read-only t))

(defmethod node-type-p-using-navigator ((navigator navigator)
                                        (proxy     attribute-proxy)
                                        (type      (eql :attribute)))
  t)

(defmethod local-name-using-navigator ((navigator navigator)
                                       (proxy     attribute-proxy))
  (symbol->name (attribute-proxy-name proxy))) ; TODO allow customization

(defmethod namespace-uri-using-navigator ((navigator navigator)
                                          (proxy     attribute-proxy))
  (symbol->namespace (attribute-proxy-name proxy)))

(defmethod child-pipe-using-navigator ((navigator navigator)
                                       (node      attribute-proxy))
  ;; This method is necessary for expressions likes @foo/bar.
  empty-pipe)

;;; `relation-proxy'

(defstruct (relation-proxy
            (:include proxy
             (parent nil :type node-proxy :read-only t))
            (:constructor make-relation-proxy (relation target args parent))
            (:predicate nil)
            (:copier nil))
  (relation nil            :read-only t)
  ;; The value to be stored in the `node-proxy' child of this node.
  (target   nil            :read-only t)
  (args     nil :type list :read-only t))

(defmethod node-type-p-using-navigator ((navigator navigator)
                                        (proxy     relation-proxy)
                                        (type      (eql :element)))
  t)

(defmethod local-name-using-navigator ((navigator navigator)
                                       (proxy     relation-proxy))
  (symbol->name (relation-proxy-relation proxy))) ; TODO allow customization

(defmethod namespace-uri-using-navigator ((navigator navigator)
                                          (proxy     relation-proxy))
  (symbol->namespace (relation-proxy-relation proxy)))

(defmethod attribute-pipe-using-navigator ((navigator navigator)
                                           (proxy     relation-proxy))
  (let ((args (relation-proxy-args proxy)))
    (list->instance-pipe (args (name value &rest rest))
      (make-attribute-proxy name value proxy))))

(defmethod child-pipe-using-navigator ((navigator navigator)
                                       (proxy     relation-proxy))
  (make-pipe (make-node-proxy (relation-proxy-target proxy) proxy) empty-pipe))

;;; Unwrap

(defmethod unwrap ((navigator t) (proxy xpath:node-set))
  (let ((xpath:*navigator* navigator))
    (xpath:map-node-set->list (curry #'unwrap navigator) proxy)))

(defmethod unwrap ((navigator t) (proxy document-proxy))
  (document-proxy-root proxy))

(defmethod unwrap ((navigator t) (proxy node-proxy))
  (node-proxy-value proxy))

(defmethod unwrap ((navigator t) (proxy relation-proxy))
  (relation-proxy-relation proxy))

(defmethod unwrap ((navigator t) (proxy attribute-proxy))
  (cons (attribute-proxy-name proxy)
        (attribute-proxy-value proxy)))

(defmethod unwrap ((navigator t) (proxy string))
  proxy)

(defmethod unwrap ((navigator t) (proxy null))
  proxy)
