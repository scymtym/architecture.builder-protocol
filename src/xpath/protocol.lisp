;;;; protocol.lisp --- Protocol provided by the xpath module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.xpath)

;;;; We represent nodes and relations for which the "un-build"
;;;; protocol is available as XML nodes and evaluate XPath expressions
;;;; on them.
;;;;
;;;; Technically, the XML nodes are proxies representing the actual
;;;; nodes and relations (see proxies.lisp):
;;;;
;;;; * nodes are mapped to XML element nodes with namespace URI and
;;;;   local-name determined by the value of `node-kind'.
;;;;
;;;; * node initargs are mapped to XML attribute nodes with namespace
;;;;   URI and local-name determined by the symbol naming the initarg.
;;;;
;;;; * relations are mapped to (entirely synthetic) XML element nodes
;;;;   with namespace URI and local-name determined by the symbol
;;;;   naming the relation.
;;;;
;;;; * relation arguments are mapped to XML attribute nodes with
;;;;   namespace URI and local-name determined by the symbol naming
;;;;   the initarg.
;;;;
;;;; In summary, a node tree of the form
;;;;
;;;;   (node* (+ :foo 1 'cl-user::bar 2)
;;;;     (1 :baz (node* (:fez))))
;;;;
;;;; roughly corresponds to an XML node tree of the form
;;;;
;;;;   <cl:+ xmlns:cl="common-lisp"
;;;;         xmlns:cu="common-lisp-user"
;;;;         foo="1" cu:bar="2">
;;;;     <baz>
;;;;       <fez/>
;;;;     </baz>
;;;;   </cl:+>

(defgeneric evaluate (xpath builder tree &key printers node-order)
  (:documentation
   "Evaluate XPATH on TREE using BUILDER in the XPath adapter.

    XPATH can be any XPath object accepted by `xpath:evaluate',
    i.e. an XPath expression string or a compiled representation.

    BUILDER has to provide the \"un-build\" protocol for all nodes in
    TREE.

    PRINTERS, when supplied, has to be a list of elements of the form

      (PREDICATE . PRINTER)

    where PREDICATE is a function that, when called with a value as
    the sole argument, returns a Boolean indicating whether PRINTER
    should be used to render the value as a string. The lambda-list of
    PRINTER must be compatible to

      (BUILDER VALUE)

    where BUILDER is the builder stored in NAVIGATOR and VALUE is a
    value extracted from THING such as the value of a valued proxy
    node. The function must return a string representation of
    VALUE.

    NODE-ORDER only has an effect if XPATH evaluates to a node set. In
    that case, NODE-ORDER controls whether/how the returned node set
    is ordered. Possible values are:

    NIL

      The returned node set can be arranged in any order. This can be
      significantly faster than establishing a particular order.

    :document-order

      Nodes appear in the returned node set in document order."))

(defgeneric find-printer (thing navigator)
  (:documentation
   "Find and return a print function for THING in NAVIGATOR.

    The lambda-list of the returned function must be compatible to

      (BUILDER VALUE)

    where BUILDER is the builder stored in NAVIGATOR and VALUE is a
    value extracted from THING such as the value of a valued proxy
    node. The function must return a string representation of
    VALUE."))

;; Default behavior

(defmethod evaluate ((xpath t) (builder t) (tree t)
                     &key
                     printers
                     (node-order :document-order))
  (let ((navigator (make-instance 'navigator
                                  :builder  builder
                                  :printers printers)))
    (evaluate-using-navigator xpath navigator tree :node-order node-order)))

(defun evaluate-using-navigator (xpath navigator tree
                                 &key
                                 (node-order :document-order))
  (let* ((document          (make-document-proxy tree))
         (xpath:*navigator* navigator)
         (root              (xpath-sys:pipe-head
                             (xpath-protocol:child-pipe document))))
    (xpath:evaluate xpath root (not (eq node-order :document-order)))))

;;; Unwrap protocol

(defgeneric unwrap (navigator proxy)
  (:documentation
   "Use NAVIGATOR to extract a useful result from PROXY.

    NAVIGATOR has to match the navigator used in the `evaluate' call
    that returned PROXY.

    Unwrapping has the following effect on different kinds of things:

    node set        -> a list of the unwrapped nodes contained in the
                       node set

    document-proxy  -> the root element of the document

    node proxy      -> the node

    relation proxy  -> the relation

    attribute proxy -> (ATTRIBUTE-NAME . ATTRIBUTE-VALUE)

    For everything else, unwrapping is the identity function."))
