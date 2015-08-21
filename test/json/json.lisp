;;;; json.lisp --- Tests for the JSON serialization functionality.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.json.test)

(in-suite :architecture.builder-protocol.json)

(defun serialize-test-cases (cases)
  (flet ((test-case (spec)
           (destructuring-bind (tree args expected) spec
             (with-unbuilder (builder 'list)
               (is (string= expected
                            (with-output-to-string (stream)
                              (apply #'serialize builder tree stream
                                     args))))))))
    (mapcar #'test-case cases)))

(test (serialize.smoke)
  "Smoke test for the `serialize' function."

  (serialize-test-cases
   '(((:foo ())
      (:kind-transform nil)
      "{}")

     ((:foo () :bar 1)
      (:kind-transform nil)
      "{\"bar\":1}")

     ;; Generic relation.
     ((:foo (:bar (((:baz ())))))
      (:kind-transform nil)
      "{\"bar\":[{}]}")

     ((:foo (:bar (((:baz ()))
                   ((:fez ())))))
      (:kind-transform nil)
      "{\"bar\":[{},{}]}")

     ;; Relation with explicit cardinality * is same as generic.
     ((:foo ((:bar . *) ()))
      (:kind-transform nil)
      "{\"bar\":[]}")

     ((:foo ((:bar . *) (((:baz ())))))
      (:kind-transform nil)
      "{\"bar\":[{}]}")

     ((:foo ((:bar . *) (((:baz ()))
                         ((:fez ())))))
      (:kind-transform nil)
      "{\"bar\":[{},{}]}")

     ;; Relation with cardinality 1.
     ((:foo ((:bar . 1) (((:baz ())))))
      (:kind-transform nil)
      "{\"bar\":{}}")

     ;; Relation with cardinality ?.
     ((:foo ((:bar . ?) ()))
      (:kind-transform nil)
      "{}")

     ((:foo ((:bar . ?) (((:baz ())))))
      (:kind-transform nil)
      "{\"bar\":{}}")

     ;; Relation with cardinality :map.
     ((:foo ((:bar . (:map . :fez)) ()))
      (:kind-transform nil)
      "{\"bar\":{}}")

     ((:foo ((:bar . (:map . :fez))
             (((:baz ()) . (:fez 1)))))
      (:kind-transform nil)
      "{\"bar\":{\"1\":{}}}")

     ((:foo ((:bar . (:map . :fez))
             (((:baz ()) . (:fez 1))
              ((:who ()) . (:fez "a")))))
      (:kind-transform nil)
      "{\"bar\":{\"1\":{},\"a\":{}}}"))))

(test serialize.kind-transform
  "Test supplying a kind key function to the `serialize' function."

  (serialize-test-cases
   '(((:foo () :bar 1)
      ()
      "{\"__kind\":\"foo\",\"bar\":1}"))))

(test serialize.initarg-transform
  "Test supplying a kind key function to the `serialize' function."

  (serialize-test-cases
   `(((:foo () :bar 1)
      (:kind-transform    nil
       :initarg-transform ,(lambda (key value)
                             (values (string-upcase key)
                                     (princ-to-string value))))
      "{\"BAR\":\"1\"}"))))

(test serialize.relation-transform
  "Test supplying a relation key function to the `serialize'
   function."

  (serialize-test-cases
   `(;; Generic relation.
     ((:foo (:bar (((:baz ())))))
      (:kind-transform nil :relation-transform ,#'string-upcase)
      "{\"BAR\":[{}]}")

     ((:foo (:bar (((:baz ()))
                   ((:fez ())))))
      (:kind-transform nil :relation-transform ,#'string-upcase)
      "{\"BAR\":[{},{}]}")

     ;; Relation with cardinality :map.
     ((:foo ((:bar . (:map . :fez)) ()))
      (:kind-transform nil :relation-transform ,#'string-upcase)
      "{\"BAR\":{}}")

     ((:foo ((:bar . (:map . :fez))
             (((:baz ()) . (:fez 1)))))
      (:kind-transform nil :relation-transform ,#'string-upcase)
      "{\"BAR\":{\"1\":{}}}")

     ((:foo ((:bar . (:map . :fez))
             (((:baz ()) . (:fez 1))
              ((:who ()) . (:fez 2)))))
      (:kind-transform nil :relation-transform ,#'string-upcase)
      "{\"BAR\":{\"1\":{},\"2\":{}}}"))))

(test serialize.member-transform
  "Test supplying a member key function to the `serialize' function."

  (serialize-test-cases
   `(((:foo ((:bar . (:map . :fez)) ()))
      (:kind-transform nil :member-transform ,(constantly "wiz"))
      "{\"bar\":{}}")

     ((:foo ((:bar . (:map . :fez))
             (((:baz ()) . (:fez 1)))))
      (:kind-transform nil :member-transform ,(constantly "wiz"))
      "{\"bar\":{\"wiz\":{}}}")

     ((:foo ((:bar . (:map . :fez))
             (((:baz ()) . (:fez 1))
              ((:who ()) . (:fez 2)))))
      (:kind-transform nil :member-transform ,(constantly "wiz"))
      "{\"bar\":{\"wiz\":{},\"wiz\":{}}}"))))

(test serialize.peek-function
  "Test supplying a \"peek function\" to the `serialize' function."

  (labels ((instead-of-values (value)
             (if (typep value 'boolean)
                 value
                 (values value 'raw)))
           (instead-of-any (value)
             (lambda (builder relation relations-args node)
               (declare (ignore builder relation relations-args node))
               (instead-of-values value)))
           (instead-of-baz (value)
             (lambda (builder relation relations-args node)
               (declare (ignore builder relations-args node))
               (if (eq relation :baz)
                   (instead-of-values value)
                   t))))
    (serialize-test-cases
     `( ;; Normal behavior.
       ((:foo () :bar 1)
        (:peek-function ,(instead-of-any t))
        "{\"__kind\":\"foo\",\"bar\":1}")

       ;; Suppressing nodes.
       ((:foo () :bar 1)
        (:peek-function ,(instead-of-any nil))
        "")

       ((:foo (:baz (((:fez ())))))
        (:peek-function ,(instead-of-baz nil) :kind-transform nil)
        "{\"baz\":[]}")

       ((:foo ((:baz . 1) (((:fez ())))))
        (:peek-function ,(instead-of-baz nil) :kind-transform nil)
        "{}")

       ((:foo ((:baz . ?) (((:fez ())))))
        (:peek-function ,(instead-of-baz nil) :kind-transform nil)
        "{}")

       ((:foo ((:baz . *) (((:fez ())))))
        (:peek-function ,(instead-of-baz nil) :kind-transform nil)
        "{\"baz\":[]}")

       ((:foo ((:baz . (:map . :key)) (((:fez ()) :key 1))))
        (:peek-function ,(instead-of-baz nil) :kind-transform nil)
        "{\"baz\":{}}")

       ;; Replacing nodes with something else.
       ((:foo () :bar 1)
        (:peek-function ,(instead-of-any 5))
        "5")

       ((:foo (:baz (((:fez ())))) :bar 1)
        (:peek-function ,(instead-of-baz 5) :kind-transform nil)
        "{\"bar\":1,\"baz\":[5]}")

       ((:foo ((:baz . 1) (((:fez ())))) :bar 1)
        (:peek-function ,(instead-of-baz 5) :kind-transform nil)
        "{\"bar\":1,\"baz\":5}")

       ((:foo ((:baz . ?) (((:fez ())))) :bar 1)
        (:peek-function ,(instead-of-baz 5) :kind-transform nil)
        "{\"bar\":1,\"baz\":5}")

       ((:foo ((:baz . *) (((:fez ())))) :bar 1)
        (:peek-function ,(instead-of-baz 5) :kind-transform nil)
        "{\"bar\":1,\"baz\":[5]}")

       ((:foo ((:baz . (:map . :key)) (((:fez ()) :key 1))) :bar 1)
        (:peek-function ,(instead-of-baz 5) :kind-transform nil)
        "{\"bar\":1,\"baz\":{\"1\":5}}")))))

(test symbol-transform-cache.flush
  "Test that overflowing the transform caches causes flushes."

  (finishes
    (with-output-to-string (stream)
      (serialize 'list
                 `(:foo () ,@(loop :repeat 100 :collect (gensym) :collect 1))
                 stream
                 :initarg-transform (default-key-and-value-transform
                                        :cache-size 20)))))
