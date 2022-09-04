;;;; inspect.lisp --- Inspector support for trees.
;;;;
;;;; Copyright (C) 2020, 2021, 2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.inspection)

;;; Utilities

(defun format-relation (stream relation cardinality &key container)
  (let ((*print-case* :downcase))
    (if container
        (clouseau:with-preserved-cursor-y (stream)
          (clouseau:formatting-place
              (container 'clouseau:pseudo-place relation nil present-object)
            (present-object stream)))
        (princ relation stream)))
  (clouseau:with-style (stream :note)
    (princ cardinality stream)))

(defun format-initargs (stream object initargs)
  (clim:formatting-table (stream)
    (loop :for (initarg value) :on initargs :by #'cddr
          :do (clim:formatting-row (stream)
                (clim:formatting-cell (stream)
                  (clouseau:formatting-place (object 'clouseau:pseudo-place initarg
                                              nil present-object)
                    (let ((*print-case* :downcase))
                      (present-object stream))))
                (clouseau:formatting-place (object 'clouseau:pseudo-place value
                                            present-place present-object)
                  (clim:formatting-cell (stream)
                    (clouseau:with-style (stream :slot-like)
                      (present-place stream)))
                  (clim:formatting-cell (stream)
                    (let ((*print-level*  3)
                          (*print-circle* t))
                      (present-object stream))))))))

;;; `inspected-node'
;;;
;;; An object that should be inspected as a tree node using a given
;;; (un)builder.

(defvar *builder*) ; TODO hack

(defclass inspected-node (clouseau:inspected-object)
  ((%builder :initarg :builder
             :reader  builder)))

(defmethod clouseau:inspect-object-using-state ((object t)
                                                (state  inspected-node)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (let* ((builder   (builder state))
         (kind      (bp:node-kind builder object))
         (initargs  (bp:node-initargs builder object))
         (relations (bp:node-relations builder object)))
    (clouseau:with-style (stream :header)
      (princ kind stream))
    (loop :for relation :in relations
          :for (relation-name cardinality) = (multiple-value-list
                                              (bp:normalize-relation relation))
          :do (write-char #\Space stream)
              (format-relation stream relation-name cardinality))
    (when initargs
      (write-char #\Space stream)
      (let ((*print-right-margin* most-positive-fixnum))
        (loop :for (initarg value . rest) :on initargs :by #'cddr
              :do (format stream "~(~S~) " initarg)
                  (typecase value
                    (string (clouseau::print-string-compactly
                             value stream :delimitersp t))
                    (t (let ((*print-level*  3)
                             (*print-circle* t)
                             (*print-length* 3))
                         (prin1 value stream))))
                  (when rest
                    (write-char #\Space stream)))))))

(defmethod clouseau:inspect-object-using-state ((object t)
                                                (state  inspected-node)
                                                (style  (eql :expanded-header))
                                                (stream clim:extended-output-stream))
  (let* ((builder (builder state))
         (kind    (bp:node-kind builder object)))
    (clouseau:formatting-place (object 'clouseau:pseudo-place kind
                                       nil present-object)
      (present-object stream))))

(defmethod clouseau:inspect-object-using-state ((object t)
                                                (state  inspected-node)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (let* ((builder   (builder state))
         (initargs  (bp:node-initargs builder object))
         (relations (bp:node-relations builder object)))
    (when initargs
      (clouseau:with-section (stream) "Initargs"
        (format-initargs stream object initargs)))

    (when relations ; TODO hide if empty or empty marker?
      (clouseau:with-section (stream) "Relations"
        (clim:formatting-table (stream)
          (loop :with limit = 100
                :for i :from 0 :below limit
                :for relation :in relations
                :for (relation-name cardinality)
                   = (multiple-value-list
                      (bp:normalize-relation relation))
                :for (rights all-args)
                   = (multiple-value-list
                      (bp:node-relation builder relation object))
                :do (loop :for first? = t :then nil
                          :for right :in (case cardinality
                                           ((1 bp:?) (list rights))
                                           (t rights))
                          :for args :in (case cardinality
                                          ((1 bp:?) (list all-args))
                                          (t all-args))
                          :do (clim:formatting-row (stream)
                                (clim:formatting-cell (stream)
                                  (when first?
                                    (format-relation stream relation-name cardinality
                                                     :container object))
                                  (when args
                                    (format stream "~%  ")
                                    (clim:with-drawing-options (stream :text-size :smaller)
                                      (format-initargs stream object args))))
                                (let ((*builder* builder))
                                  (clouseau:formatting-place
                                      (object 'as-node-place right present-place present-object)
                                    (clim:formatting-cell (stream)
                                      (clouseau:with-style (stream :slot-like)
                                        (present-place stream)))
                                    (clim:formatting-cell (stream)
                                      (present-object stream))))))))))))

;;; `as-node-place'
;;;
;;; A place that forces the inspection of an arbitrary object as a
;;; tree node.

(defclass as-node-place (clouseau:pseudo-place)
  ((%builder :initarg  :builder
             :reader   builder
             :initform *builder*)))

(defmethod clouseau:object-state-class :around ((object t) (place as-node-place))
  'inspected-node)

(defmethod clouseau:make-object-state :around ((object t) (place as-node-place))
  (let ((class (clouseau:object-state-class object place)))
    (make-instance class :place   place
                         :builder (builder place))))

;;; `as-tree'
;;;
;;; A place that allows treating an object as the root of a tree.

(defclass as-tree ()
  ((%object  :initarg :object
             :reader  object)
   (%builder :initarg :builder
             :reader  builder)))

(defun as-tree (object builder)
  (unless (compute-applicable-methods #'bp:node-kind (list builder object))
    (error "~@<~S is not a suitable builder for tree ~S.~@:>"
           builder object))
  (make-instance 'as-tree :object object :builder builder))

(defmethod clouseau:object-state-class ((object as-tree) (place t))
  'inspected-node)

(defmethod clouseau:make-object-state ((object as-tree) (place t))
  (let ((class (clouseau:object-state-class object place)))
    (make-instance class :place   place
                         :builder (builder object))))

;;; HACK this method may sidestep parts of the protocol
(defmethod clouseau:inspect-object-using-state :around ((object as-tree)
                                                        (state  inspected-node)
                                                        (style  t)
                                                        (stream clim:extended-output-stream))
  (clouseau:inspect-object-using-state (object object) state style stream))

;;; `query'

(defclass query ()
  ((%root    :initarg  :root ; TODO same as as-tree
             :reader   root)
   (%builder :initarg  :builder
             :reader   builder)
   (%query   :initarg  :query
             :accessor query)
   (%limit   :initarg  :limit
             :accessor limit
             :initform nil)))

(defun as-query (root builder query)
  (make-instance 'query :root root :builder builder :query query))

(defmethod clouseau:inspect-object-using-state ((object query)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (let ((root    (root object))
        (builder (builder object))
        (query   (query object))
        (limit   (limit object))
        (matches (make-array 32 :adjustable t :fill-pointer 0)))
    ;; Collect matching nodes
    (block nil
      (bp:walk-nodes
       builder
       (lambda (recurse relation relation-args node kind relations
                &rest initargs)
         (declare (ignore relation relation-args relations initargs))
         (when (eq kind query)
           (vector-push-extend node matches)
           (when (and limit (>= (fill-pointer matches) limit))
             (return)))
         (funcall recurse))
       root))
    ;; Header
    (clim:formatting-table (stream)
      (clouseau:format-place-row
       stream object 'clouseau:accessor-place 'query
       :label "Query")
      (clouseau:format-place-row
       stream object 'clouseau:accessor-place 'limit
       :label "Limit"))
    (terpri stream)
    ;; List matches
    (clouseau:with-section (stream) (format stream "Matches (~D)"
                                            (length matches))
      (let ((*builder* builder))
        (clim:formatting-item-list (stream :n-columns 1)
          (map nil (lambda (match)
                     (clim:formatting-cell (stream)
                       (clouseau:formatting-place
                           (object 'as-node-place match nil present-object)
                         (present-object stream))))
               matches))))))
