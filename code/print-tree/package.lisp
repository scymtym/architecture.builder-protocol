;;;; package.lisp --- Package definition for the print-tree module.
;;;;
;;;; Copyright (C) 2015-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.print-tree
  (:use
   #:cl
   #:alexandria

   #:architecture.builder-protocol)

  (:export
   #:print-tree)

  (:documentation
   "Functions for printing \"un-buildable\" nodes as textual trees."))
