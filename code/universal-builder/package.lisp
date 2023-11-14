;;;; package.lisp --- Package definition for the universal builder module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.universal-builder
  (:use
   #:cl
   #:alexandria
   #:architecture.builder-protocol)

  (:export
   #:universal-builder)

  (:documentation
   "A builder implementing the (un)build protocols for
    `standard-object's."))
