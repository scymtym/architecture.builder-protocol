;;;; package.lisp --- Package definition for tests of the universal-builder module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.universal-builder.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:architecture.builder-protocol
   #:architecture.builder-protocol.universal-builder)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the `universal-builder'
    class."))

(cl:in-package #:architecture.builder-protocol.universal-builder.test)

;;; Test suite and external interface

(def-suite :architecture.builder-protocol.universal-builder
  :description
  "Test suite for `universal-builder' class.")

(defun run-tests ()
  (run! :architecture.builder-protocol.universal-builder))
