;;;; util.lisp --- Unit tests for utilities provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(in-suite :architecture.builder-protocol)

(test normalize-relation.smoke
  "Smoke test for the `normalize-relation' function."

  (flet ((do-it (spec)
           (multiple-value-list (normalize-relation spec))))
    (is (equal '(:foo *)           (do-it :foo)))
    (is (equal '(:foo 1)           (do-it '(:foo . 1))))
    (is (equal '(:foo (:map :bar)) (do-it '(:foo . (:map :bar)))))))

(test cardinality-case.smoke
  "Smoke test for the `cardinality-case' macro."

  (flet ((test-case (value expected)
           (is (eq expected (cardinality-case value
                              (1    :one)
                              (?    :maybe-one)
                              (*    :many)
                              (:map :map))))))
    (test-case 1              :one)
    (test-case '?             :maybe-one)
    (test-case '*             :many)
    (test-case '(:map . :key) :map)))

(test cardinality-ecase.error
  "Non-exhaustive `cardinality-ecase' forms should signal an error."

  (signals error
    (macroexpand '(cardinality-ecase c ((1 ?) :foo) ((*) :bar)))))
