;;;; protocol.lisp --- Unit tests for protocol functions of the xpath module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.xpath.test)

(in-suite :architecture.builder-protocol.xpath)

(test evaluate.smoke
  "Smoke test for the `evaluate' generic function."

  ;; Just check that the basic interface works. Since `evaluate' is a
  ;; thin wrapper around `evaluate-using-navigator', all in-depth
  ;; testing is done for that function.
  (is (string= "foo" (evaluate "name()" 'list '(:foo ())
                               :node-order nil)))
  (is (string= "foo" (evaluate "name()" 'list '(:foo ())
                               :node-order :document-order))))

(defun evaluate-test (cases)
  (labels ((evaluate/unwrap (xpath document &rest args)
             (let ((navigator (make-instance 'navigator :builder 'list)))
               (unwrap navigator (apply #'evaluate-using-navigator
                                        xpath document navigator args))))
           (equal/ (left right)
             (cond
               ((and (emptyp left) (emptyp right))
                t)
               (t
                (map-permutations (lambda (permutation)
                                    (when (equal permutation right)
                                      (return-from equal/ t)))
                                  left)
                nil)))
           (test-case (xpath document expected)
             (is (equal  expected (evaluate/unwrap xpath document)))
             (is (equal/ expected (evaluate/unwrap xpath document
                                                   :node-order nil)))))
    (mapcar (curry #'apply #'test-case) cases)))

(test evaluate.document
  "Smoke test for the `evaluate-using-navigator' function on document
   nodes."

  (evaluate-test
   `(,@(let ((node '(:foo ())))
         `(("/" ,node (,node))))
     ("name(/)"          (:foo ()) "")
     ("local-name(/)"    (:foo ()) "")
     ("namespace-uri(/)" (:foo ()) "")
     ("/@*"              (:foo ()) ())
     ,@(let ((node '(:foo ())))
         `(("/*" ,node (,node)))))))

(test evaluate.element
  "Smoke test for the `evaluate-using-navigator' function on element
   nodes."

  (evaluate-test
   `(,@(let ((node '(:foo ())))
         `(("/foo" ,node (,node))
           ("."    ,node (,node))))
     ("name()"          (:foo ()) "foo")
     ("name()"          (+    ()) "+")
     ("local-name()"    (:foo ()) "foo")
     ("local-name()"    (+    ()) "+")
     ("namespace-uri()" (:foo ()) "")
     ("namespace-uri()" (+    ()) "common-lisp"))))

(test evaluate.attribute
  "Smoke test for the `evaluate-using-navigator' function attribute
   nodes."

  (evaluate-test
   `(("@bar"                   (:foo () :bar 1) ((:bar . 1)))
     ("name(@*)"               (:foo () :bar 1) "bar")
     ("name(@*)"               (:foo () +    1) "+")
     ("local-name(@*)"         (:foo () :bar 1) "bar")
     ("local-name(@*)"         (:foo () +    1) "+")
     ("namespace-uri(@*)"      (:foo () :bar 1) "")
     ("namespace-uri(@*)"      (:foo () +    1) "common-lisp")

     ("@bar/foo"               (:foo () :bar 1) nil)
     ("@no-such-attribute/foo" (:foo () :bar 1) nil))))

(test evaluate.relation
  "Smoke test for the `evaluate-using-navigator' function on relation
   nodes."

  (evaluate-test
   (let* ((child    '(:fez ()))
          (parent-1 `(:foo (:bar       ((,child)))))
          (parent-2 `(:foo ((:bar . 1) (,child))))
          (parent-3 `(:foo (+          ((,child))))))
     `(("bar/fez"          ,parent-1 (,child))
       ("bar/fez"          ,parent-2 (,child))
       ("bar/fez"          ,parent-3 ())
       ("*/fez"            ,parent-1 (,child))
       ("*/fez"            ,parent-2 (,child))
       ("*/fez"            ,parent-3 (,child))
       ("name(*)"          ,parent-1 "bar")
       ("name(*)"          ,parent-2 "bar")
       ("name(*)"          ,parent-3 "+")
       ("local-name(*)"    ,parent-1 "bar")
       ("local-name(*)"    ,parent-2 "bar")
       ("local-name(*)"    ,parent-3 "+")
       ("namespace-uri(*)" ,parent-1 "")
       ("namespace-uri(*)" ,parent-2 "")
       ("namespace-uri(*)" ,parent-3 "common-lisp")))))

(test evaluate.relation.attribute
  "Smoke test for the `evaluate-using-navigator' function on attribute
   nodes of relations."

  (evaluate-test
   (let* ((child  '(:fez ()))
          (parent `(:foo (:bar ((,child :whoop 1 :dat "2"))))))
     `(("bar/@whoop" ,parent ((:whoop . 1)))
       ("bar/@dat"   ,parent ((:dat   . "2")))))))

(test evaluate.axis
  "Smoke test for the `evaluate-using-navigator' function on
   expressions with multiple axis steps."

  (evaluate-test
   (let* ((child-1    '(:fez ()))
          (child-2    '(:fez ()))
          (relation   :bar)
          (document-1 `(:foo (,relation ((,child-1)))))
          (document-2 `(:foo (,relation ((,child-1) (,child-2))))))
     `(;; One child
       ("bar/fez" ,document-1 ,(list child-1))
       ("*/fez"   ,document-1 ,(list child-1))
       ("bar/*"   ,document-1 ,(list child-1))
       ("*/*"     ,document-1 ,(list child-1))
       ("//fez"   ,document-1 ,(list child-1))
       (".//*"    ,document-1 ,(list relation child-1))
       ("//*"     ,document-1 ,(list document-1 relation child-1))
       ;; Two children
       ("bar/fez" ,document-2 ,(list child-1 child-2))
       ("*/fez"   ,document-2 ,(list child-1 child-2))
       ("bar/*"   ,document-2 ,(list child-1 child-2))
       ("*/*"     ,document-2 ,(list child-1 child-2))
       ("//fez"   ,document-2 ,(list child-1 child-2))
       (".//*"    ,document-2 ,(list relation child-1
                                     relation child-2))
       ("//*"     ,document-2 ,(list document-2
                                     relation child-1
                                     relation child-2))))))

(test evaluate.sorting
  "Smoke test for the `evaluate-using-navigator' function on
   expressions the results of which depend on whether document-order
   is requested."

  (evaluate-test
   (let* ((child-1  '(:fez-1 ()))
          (child-2  '(:fez-2 ()))
          (document `(:foo (,:bar ((,child-1) (,child-2))))))
     `(("union(bar/fez-2, bar/fez-1)" ,document ,(list child-1 child-2))
       ("union(bar/*, bar/*)"         ,document ,(list child-1 child-2))))))

(test evaluate.string
  "Smoke test for the `evaluate-using-navigator' function on
   expressions calling the string function."

  (evaluate-test
   `(("string()"     (:foo () :bar 1)                "(FOO NIL BAR 1)")
     ("string(@bar)" (:foo () :bar 1)                "1")
     ("string(@bar)" (:foo () :bar "a")              "a")
     ("string(*)"    (:foo ((:bar . 1) ((:baz ())))) ""))))

(test evaluate.printers
  "Test specifying print functions for certain nodes."

  (labels ((thing-of-type (type)
             (lambda (builder thing)
               (declare (ignore builder))
               (typep thing type)))
           (print-thing-as (type format-control)
             (cons (thing-of-type type)
                   (lambda (builder thing)
                     (declare (ignore builder))
                     (format nil format-control thing))))
           (evaluate/unwrap (xpath document &rest args)
             (let ((navigator (apply #'make-instance 'navigator
                                     :builder 'list args)))
               (unwrap navigator (evaluate-using-navigator
                                  xpath document navigator))))
           (test-case (xpath document args expected)
             (is (equal expected (apply #'evaluate/unwrap xpath document args)))))
    (let ((document-2 '(:foo (:bar (((:fez () :baz :who)))))))
      (mapcar
       (curry #'apply #'test-case)
       `(,(let ((document '(:foo ())))
            `("." ,document () ,(list document)))

          ("bar/fez"
           (:foo (:bar (((:fez ())))))
           ()
           ((:fez nil)))

          ("string(bar/fez)"
           ,document-2
           (:printers ,(list (print-thing-as '(eql :who) "<~A>")
                             (print-thing-as '(cons (eql :fez)) "<~A>")))
           "<(FEZ NIL BAZ WHO)>")

          ("string(bar/fez/@baz)"
           ,document-2
           (:printers ,(list (print-thing-as '(eql :who) "[~A]")
                             (print-thing-as '(cons (eql :fez)) "<~A>")))
           "[WHO]"))))))
