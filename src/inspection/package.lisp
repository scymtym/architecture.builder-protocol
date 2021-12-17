;;;; package.lisp --- Package definition for the inspection module.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.inspection
  (:use
   #:cl)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:export
   #:as-tree
   #:as-query))
