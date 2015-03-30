;;;; package.lisp --- Package definition for unit tests of the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.test
  (:use
   #:cl
   #:fiveam

   #:architecture.builder-protocol)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the
    architecture.builder-protocol system"))

(cl:in-package #:architecture.builder-protocol.test)

;;; Root test suite and external interface

(def-suite :architecture.builder-protocol
  :description
  "Root unit test suite for the architecture.builder-protocol
   system.")

(defun run-tests ()
  (let ((results (run :architecture.builder-protocol)))
    (explain! results)
    (results-status results)))
