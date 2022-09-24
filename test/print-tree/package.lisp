;;;; package.lisp --- Package definition for unit tests of the architecture.builder-protocol.print-tree system.
;;;;
;;;; Copyright (C) 2014-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.print-tree.test
  (:use
   #:cl
   #:fiveam

   #:architecture.builder-protocol.print-tree)

  (:local-nicknames
   (#:a #:alexandria))

  (:import-from #:architecture.builder-protocol
   #:?)

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
  (run! :architecture.builder-protocol.print-tree))
