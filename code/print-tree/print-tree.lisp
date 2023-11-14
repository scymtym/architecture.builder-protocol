;;;; print-tree.lisp --- Printing of builder-based trees.
;;;;
;;;; Copyright (C) 2015-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.print-tree)

;;; This code uses the utilities.print-tree library for printing
;;; trees. Since that library takes care of indentation and prints the
;;; tree structure indicators, the basic operations of this code are
;;; printing a single node and enumerating the children of a node.
;;;
;;; In such a list of children, each child is represented together
;;; with its relation to the previously visited node (the parent) as a
;;; cons of the form
;;;
;;;   (RELATION-AND-MAYBE-KEY . NODE)
;;;
;;; where RELATION-AND-MAYBE-KEY is of one of the forms
;;;
;;;   (RELATION)        if the cardinality of RELATION is 1, ? or *
;;;   (RELATION . KEY)  if the cardinality of RELATION is :map
;;;
;;; where KEY is the value of the relation argument that is the key of
;;; the :map relation.
;;;
;;; Nodes which are represented in the way described above are passed
;;; to the node printer which outputs a description of the relation,
;;; the node kind and, optionally initargs of the node on separate
;;; lines.
;;;
;;; It is possible to supply custom functions for printing nodes. In
;;; that case, a predicate for deciding whether to use a given custom
;;; print function has to be supplied together with the print function
;;; itself.

(defun find-printer (node printers)
  (cdr (find-if (lambda (printer)
                  (funcall (car printer) node))
                printers)))

(defun make-first-line-printer (builder &key printers)
  (named-lambda print-first-line (stream depth node)
    (declare (ignore depth))
    (destructuring-bind (relation . node) node
      (when relation
        (format stream "~A~@[[~S]~]: " (car relation) (cdr relation)))
      (if-let ((printer (find-printer node printers)))
        (progn
          (funcall printer builder node stream)
          ;; Return false to indicate that NODE is printed on a single
          ;; line.
          nil)
        (let ((kind     (node-kind builder node))
              (initargs (node-initargs builder node)))
          (format stream "~A~@[ @~A~]" kind (getf initargs :bounds))
          ;; Return true if there are other INITARGS to indicate that
          ;; more lines should be printed for NODE.
          (and (consp initargs) (or (not (eq (first initargs) :bounds))
                                    (cddr initargs))))))))

(defun make-rest-printer (builder)
  ;; Print additional node initargs.
  (named-lambda print-rest (stream depth node)
    (declare (ignore depth))
    (let* ((node     (cdr node))
           (initargs (node-initargs builder node)))
      (format stream "~{~A: ~S~^~@:_~}"
              (remove-from-plist initargs :bounds)))))

(defun make-node-children (builder)
  (named-lambda node-children (node)
    (destructuring-bind (relation . node) node
      (declare (ignore relation))
      (loop :for relation-and-cardinality :in (node-relations builder node)
            :for (relation cardinality)   =   (multiple-value-list
                                               (normalize-relation
                                                relation-and-cardinality))
            :for (targets args)           =   (multiple-value-list
                                               (node-relation
                                                builder relation node))
            :appending (cardinality-ecase cardinality
                         (?
                          (when targets
                            (list (cons (list relation) targets))))
                         (1
                          (list (cons (list relation) targets)))
                         (*
                          (map 'list #'cons (circular-list (list relation)) targets))
                         ((:map . key-key)
                          (map 'list (lambda (target args)
                                       (let ((key (getf args key-key)))
                                         (cons (cons relation key) target)))
                               targets args)))))))

(defun make-node-printer (builder &key printers)
  (utilities.print-tree:make-node-printer
   (make-first-line-printer builder :printers printers)
   (make-rest-printer builder)
   (make-node-children builder)))

(defun print-tree (builder tree stream &key printers)
  (utilities.print-tree:print-tree
   stream (cons nil tree) (make-node-printer builder :printers printers)))
