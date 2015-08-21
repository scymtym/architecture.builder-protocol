;;;; package.lisp --- Package definition for unit tests of the architecture.builder-protocol.print-tree system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.print-tree.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:architecture.builder-protocol.print-tree)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the
    architecture.builder-protocol.print-tree system"))

(cl:in-package #:architecture.builder-protocol.print-tree.test)

;;; Root test suite and external interface

(def-suite :architecture.builder-protocol.print-tree
  :description
  "Root unit test suite for the
   architecture.builder-protocol.print-tree system.")

(defun run-tests ()
  (let ((results (run :architecture.builder-protocol.print-tree)))
    (explain! results)
    (results-status results)))
