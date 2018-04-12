(require :architecture.builder-protocol)
(require :esrap)

(cl:defpackage #:builder-protocol.example.esrap
  (:use
   #:cl
   #:alexandria

   #:architecture.builder-protocol
   #:esrap)

  (:shadowing-import-from #:esrap
   #:?))

(cl:in-package #:builder-protocol.example.esrap)

(defrule identifier
    (+ (alpha-char-p character))
  (:text t))

(defrule type-reference
    identifier
  (:lambda (name &bounds start end)
    (make+finish-node *builder* :type-reference
                      :name   name
                      :bounds (cons start end))))

(defrule value
    (+ (digit-char-p character))
  (:lambda (digits &bounds start end)
    (make+finish-node *builder* :value
                      :value  (parse-integer (text digits))
                      :bounds (cons start end))))

(defrule variable-declaration
    (and type-reference " " identifier (? (and " = " value)) (? " "))
  (:destructure (type space1 name (&optional space2 value) &optional space3
                 &bounds start end)
    (declare (ignore space1 space2 space3))
    (let* ((variable (make-node *builder* :variable-declaration
                                :name   name
                                :bounds (cons start end)))
           (variable (relate *builder* :variable-type variable type))
           (variable (if value
                         (relate *builder* :variable-value variable value)
                         variable)))
      (finish-node *builder* :variable-declaration variable))))

(defrule block
    (* variable-declaration)
  (:lambda (declarations &bounds start end)
    (finish-node
     *builder* :block
     (reduce (curry #'relate *builder* :block-element) declarations
             :initial-value (make-node *builder* :block
                                       :bounds (cons start end))))))

(let ((*builder* 'list))
  (parse 'block "foo bar = 1 baz fez"))
