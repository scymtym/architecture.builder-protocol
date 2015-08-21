;;;; package.lisp --- Package definition for unit tests of the json module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.json.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:architecture.builder-protocol
   #:architecture.builder-protocol.json)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the json module of the
    architecture.builder-protocol.json system."))

(cl:in-package #:architecture.builder-protocol.json.test)

;;; Test suite

(def-suite :architecture.builder-protocol.json
  :in :architecture.builder-protocol
  :description
  "Unit test suite for the json module of the
   architecture.builder-protocol.json system.")

(defun run-tests ()
  (let ((results (run :architecture.builder-protocol.json)))
    (explain! results)
    (results-status results)))
