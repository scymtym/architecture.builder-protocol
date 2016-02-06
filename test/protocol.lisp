;;;; protocol.lisp --- Unit tests for the protocol of the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

;;; Utilities

(defmacro with-implicit-and-explicit-builder
    ((builder-var builder) explicit-function &body body)
  (let ((implicit-function (symbolicate explicit-function '*)))
    `(progn
       ;; Explicit builder argument.
       (let ((,builder-var ,builder)) ,@body)
       ;; Implicit builder argument
       (let* ((,builder-var ,builder)
              (*builder* ,builder-var))
         (flet ((,explicit-function (&rest args)
                  (apply ',implicit-function (rest args))))
           ,@body)))))

;;; Global processing tests

(test prepare.smoke
  "Smoke test for the `prepare[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'preparable-mock-builder))
      prepare
    (let ((builder (prepare builder)))
      (is (equalp (mock-node :foo :slots '(:prepared? t))
                  (make-node builder :foo))))))

(test finish.smoke
  "Smoke test for the `finish[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (prepare (make-instance 'finish-mock-builder)))
      finish
    (is (equalp `(:finish ,(mock-node :foo))
                (finish builder (list (make-node builder :foo)))))))

(defun a-wrapper (builder)
  (make-node builder :foo))

(test wrap.smoke
  "Smoke test for the `wrap[*]' functions."

  (macrolet ((test-case (wrapper)
               `(with-implicit-and-explicit-builder
                    (builder (make-instance 'mock-builder))
                    wrap
                  (is (equalp (list (mock-node :foo)) (wrap builder ,wrapper))))))
    (test-case 'a-wrapper)
    (test-case #'a-wrapper)))

;;; Build protocol tests

(test make-node.smoke
  "Smoke test for the `make-node[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      make-node
    (is (equalp (mock-node :foo)
                (make-node builder :foo)))
    (is (equalp (mock-node :foo :slots '(:bar 1))
                (make-node builder :foo :bar 1)))))

(test finish-node.smoke
  "Smoke test for the `finish-node[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'mock-builder))
      finish-node
    (is (equalp (mock-node :foo :finished? t)
                (finish-node builder :foo (mock-node :foo))))))

(test make+finish-node.smoke
  "Smoke test for the `make+finish-node[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'mock-builder))
      make+finish-node
    (is (equalp (mock-node :foo :finished? t)
                (make+finish-node builder :foo)))
    (is (equalp (mock-node :foo :slots '(:bar 1) :finished? t)
                (make+finish-node builder :foo :bar 1)))))

(test relate.smoke
  "Smoke test for `relate[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      relate
    (is (equalp (mock-node :foo
                           :relations `((:baz . ((,(mock-node :bar) . nil)))))
                (relate builder :baz (mock-node :foo) (mock-node :bar))))
    (is (equalp (mock-node :foo
                           :relations `((:baz . ((,(mock-node :bar) . (:fez 2))))))
                (relate builder :baz (mock-node :foo) (mock-node :bar)
                        :fez 2)))))

(test make+finish-node+relations.smoke
  "Smoke test for the `make+finish-node+relations[*]' functions."

  (with-implicit-and-explicit-builder
      (builder (make-instance 'mock-builder))
      make+finish-node+relations
    (is (equalp (mock-node :foo :finished? t)
                (make+finish-node+relations builder :foo '() '())))
    (is (equalp (mock-node :foo :slots '(:bar 1) :finished? t)
                (make+finish-node+relations builder :foo '(:bar 1) '())))
    (is (equalp (mock-node :foo
                           :relations `((:fez (,(mock-node :baz))) )
                           :finished? t)
                (make+finish-node+relations
                 builder :foo '() `((1 :fez ,(mock-node :baz))))))
    (is (equalp (mock-node :foo
                           :relations `((:fez (,(mock-node :baz))))
                           :finished? t)
                (make+finish-node+relations
                 builder :foo '() `((* :fez (,(mock-node :baz)))))))
    ;; :map cardinality.
    (is (equalp (mock-node :foo
                           :relations `((:fez (,(mock-node :baz) :who 1)))
                           :finished? t)
                (make+finish-node+relations
                 builder :foo '() `(((:map . :who) :fez ,(mock-node :baz)
                                     :who 1)))))
    ;; :map cardinality requires key to be present as relation
    ;; argument.
    (signals error
      (make+finish-node+relations
       builder :foo '() `(((:map . :who) :fez ,(mock-node :baz)))))))

;;; Un-build protocol tests

(test node-kind.smoke
  "Smoke test for the `node-kind[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      node-kind
    (let* ((kind    :foo)
           (node    (mock-node kind)))
      (is (eq kind (node-kind builder node))))))

(test node-initargs.smoke
  "Smoke test for the `node-initargs[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      node-initargs
    (let* ((initargs '(:foo 1 :bar "2"))
           (node     (mock-node :foo :slots initargs)))
      (is (equal initargs (node-initargs builder node))))))

(test node-relations.smoke
  "Smoke test for the `node-relations[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      node-relations
    (let ((node (mock-node
                 :foo :relations `((:bar . ((,(mock-node :baz))))))))
      (is (equal '(:bar) (node-relations builder node))))))

(test node-relation.smoke
  "Smoke test for the `node-relation[*]' functions."

  (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
      node-relation
    (let* ((related (mock-node :baz))
           (node    (mock-node :foo :relations `((:bar . ((,related)))))))
      (is (equal `((,related) (()))
                 (multiple-value-list
                  (node-relation builder :bar node)))))))

;;; Node walking protocol

(test walk-nodes.smoke
  "Smoke test for the `walk-nodes[*]' functions."

  (mapc
   (lambda (case)
     (destructuring-bind (tree expected-values expected-calls) case
       (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
           walk-nodes
         (multiple-value-bind (values calls)
             (record-un-build-calls #'walk-nodes builder tree)
           (is (equal expected-values values))
           (is (equal expected-calls calls))))))

   `(,(let ((node (mock-node :foo)))
        `(,node
          (:foo)
          ((:visit nil () ,node :foo () ()))))

     ,(let ((node (mock-node :foo :slots '(:a 1))))
        `(,node
          (:foo)
          ((:visit nil () ,node :foo () (:a 1)))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `((:baz . ((,node-1)))))))
        `(,node-2
          (((:foo)))
          ((:visit nil  () ,node-2 :bar (:baz) ())
           (:visit :baz () ,node-1 :foo ()     ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `((:baz . ((,node-1 :b 2)))))))
        `(,node-2
          (((:foo)))
          ((:visit nil  ()     ,node-2 :bar (:baz) ())
           (:visit :baz (:b 2) ,node-1 :foo ()     ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `(((:baz . ?) . (,node-1))))))
        `(,node-2
          ((:foo))
          ((:visit nil  () ,node-2 :bar ((:baz . ?)) ())
           (:visit :baz () ,node-1 :foo ()           ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                 :relations `(((:baz2 . 1) . (,node-1))))))
        `(,node-2
          ((:foo))
          ((:visit nil   () ,node-2 :bar ((:baz2 . 1)) ())
           (:visit :baz2 () ,node-1 :foo ()           ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `(((:baz . *) . ((,node-1)))))))
        `(,node-2
          (((:foo)))
          ((:visit nil  () ,node-2 :bar ((:baz . *)) ())
           (:visit :baz () ,node-1 :foo ()           ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `(((:baz . (:map . :key))
                                              . ((,node-1 . (:key "foo"))))))))
        `(,node-2
          (((:foo)))
          ((:visit nil  ()           ,node-2 :bar ((:baz . (:map . :key))) ())
           (:visit :baz (:key "foo") ,node-1 :foo ()                       ())))))))

(test walk-nodes.peeking
  "Test peeking functionality of `walk-nodes[*]' functions."

  (mapc
   (lambda (case)
     (destructuring-bind (tree expected-result expected-calls) case
       (with-implicit-and-explicit-builder (builder (make-instance 'mock-builder))
           walk-nodes
         (multiple-value-bind (result calls)
             (record-un-build-calls/peeking #'walk-nodes builder 'string tree)
           (is (equal expected-result result))
           (is (equal expected-calls  calls))))))

   `(,(let ((node (mock-node :foo)))
        `(,node
          (:foo)
          ((:peek  nil () ,node)
           (:visit nil () ,node :foo () ()))))

     ,(let ((node (mock-node :foo :slots '(:a 1))))
        `(,node
          (:foo)
          ((:peek  nil () ,node)
           (:visit nil () ,node :foo () (:a 1)))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `((:baz . ((,node-1)))))))
        `(,node-2
          (((:foo)))
          ((:peek  nil  () ,node-2)
           (:visit nil  () ,node-2 :bar (:baz) ())
           (:peek  :baz () ,node-1)
           (:visit :baz () ,node-1 :foo ()     ()))))

     ,(let* ((node-1 "foo")
             (node-2 (mock-node :bar
                                :relations `((:baz . ((,node-1)))))))
        `(,node-2
          (((nil)))
          ((:peek  nil  () ,node-2)
           (:visit nil  () ,node-2 :bar (:baz) ())
           (:peek  :baz () ,node-1))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `((:baz . ((,node-1 :b 2)))))))
        `(,node-2
          (((:foo)))
          ((:peek  nil  ()     ,node-2)
           (:visit nil  ()     ,node-2 :bar (:baz) ())
           (:peek  :baz (:b 2) ,node-1)
           (:visit :baz (:b 2) ,node-1 :foo ()     ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `(((:baz . ?) . (,node-1))))))
        `(,node-2
          ((:foo))
          ((:peek  nil  () ,node-2)
           (:visit nil  () ,node-2 :bar ((:baz . ?)) ())
           (:peek  :baz () ,node-1)
           (:visit :baz () ,node-1 :foo ()           ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `(((:baz2 . 1) . (,node-1))))))
        `(,node-2
          ((:foo))
          ((:peek  nil   () ,node-2)
           (:visit nil   () ,node-2 :bar ((:baz2 . 1)) ())
           (:peek  :baz2 () ,node-1)
           (:visit :baz2 () ,node-1 :foo ()           ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `(((:baz . *) . ((,node-1)))))))
        `(,node-2
          (((:foo)))
          ((:peek  nil  () ,node-2)
           (:visit nil  () ,node-2 :bar ((:baz . *)) ())
           (:peek  :baz () ,node-1)
           (:visit :baz () ,node-1 :foo ()           ()))))

     ,(let* ((node-1 (mock-node :foo))
             (node-2 (mock-node :bar
                                :relations `(((:baz . (:map . :key))
                                              . ((,node-1 . (:key "foo"))))))))
        `(,node-2
          (((:foo)))
          ((:peek  nil  ()           ,node-2)
           (:visit nil  ()           ,node-2 :bar ((:baz . (:map . :key))) ())
           (:peek  :baz (:key "foo") ,node-1)
           (:visit :baz (:key "foo") ,node-1 :foo ()                       ())))))))
