;;;; print-tree.lisp --- .
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.print-tree)

(defun make-first-line-printer (builder &key printers)
  (named-lambda print-first-line (stream depth node)
    (declare (ignore depth))
    (destructuring-bind (relation . node) node
      (when relation
        (format stream "~A~@[[~S]~]: " (car relation) (cdr relation)))
      (if-let ((printer (find-if (lambda (printer)
                                   (funcall (car printer) node))
                                 printers)))
        (progn
          (funcall (cdr printer) node stream)
          nil)
        (let ((kind     (node-kind builder node))
              (initargs (node-initargs builder node)))
          (format stream "~A~@[ @~A~]" kind (getf initargs :bounds))
          (remove-from-plist initargs :bounds))))))

(defun make-rest-printer (builder)
  (named-lambda print-rest (stream depth node)
    (declare (ignore depth))
    (let* ((node     (cdr node))
           (initargs (node-initargs builder node)))
      (format stream "~{~A: ~A~^~@:_~}"
              (remove-from-plist initargs :bounds)))))

(defun make-node-children (builder)
  (named-lambda node-children (node)
    (let ((node (cdr node)))
      (loop :for relation :in (node-relations builder node) :appending
         (multiple-value-bind (relation cardinality)
             (normalize-relation relation)
           (multiple-value-bind (targets args)
               (node-relation builder relation node)
             (etypecase cardinality
               ((member 1 ?)
                (list (cons (list relation) targets)))
               ((eql *)
                (map 'list #'cons (circular-list (list relation)) targets))
               ((cons (eql :map))
                (map 'list (lambda (target args)
                             (let* ((key-key (cdr cardinality))
                                    (key     (getf args key-key)))
                               (cons (cons relation key) target)))
                     targets args)))))))))

(defun make-node-printer (builder &key printers)
  (utilities.print-tree:make-node-printer
   (make-first-line-printer builder :printers printers)
   (make-rest-printer builder)
   (make-node-children builder)))

(defun unbuild/print-tree (builder tree
                           &key
                           (stream *standard-output*)
                           printers)
  (utilities.print-tree:print-tree
   stream (cons nil tree) (make-node-printer builder :printers printers)))
