;;;; package.lisp --- Package definition for unit tests of the xpath module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.xpath.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:architecture.builder-protocol
   #:architecture.builder-protocol.xpath)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the xpath module of the
    architecture.builder-protocol system."))

(cl:in-package #:architecture.builder-protocol.xpath.test)

(def-suite :architecture.builder-protocol.xpath
  :description
  "Root unit test suite for the architecture.builder-protocol.xpath
   system.")

(defun run-tests ()
  (run! :architecture.builder-protocol.xpath))
