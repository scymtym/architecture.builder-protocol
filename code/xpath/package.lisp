;;;; package.lisp --- Package definition for the xpath module .
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.xpath
  (:use
   #:cl
   #:alexandria

   #:architecture.builder-protocol)

  (:import-from #:xpath-protocol
   #:node-p-using-navigator
   #:node-type-p-using-navigator
   #:namespace-uri-using-navigator
   #:namespace-pipe-using-navigator
   #:namespace-prefix-using-navigator
   #:local-name-using-navigator
   #:qualified-name-using-navigator
   #:hash-key-using-navigator
   #:node-equal-using-navigator
   #:node-text-using-navigator
   #:attribute-pipe-using-navigator
   #:parent-node-using-navigator
   #:child-pipe-using-navigator)

  (:import-from #:xpath
   #:empty-pipe
   #:make-pipe)


  ;; Navigator
  (:export
   #:navigator)

  ;; Evaluate protocol
  (:export
   #:evaluate
   #:evaluate-using-navigator)

  ;; Unwrap protocol
  (:export
   #:unwrap)

  (:documentation
   "An XPath protocol implementation based on the \"un-build\"[1]
    protocol.

    In particular, nodes correspond to XML elements whose local-name
    is determined by calling `node-kind'. Attributes of elements are
    computed by calling `node-initargs'. Finally, relations are
    presented as child elements of the elements representing the
    respective \"left\" nodes containing the elements representing the
    respective \"right\" node. Relation arguments are represented as
    attributes of the elements representing the relations.

    [1] See documentation of the build and \"un-build\" protocols for
    descriptions of the terms \"un-build\", \"left\" node and
    \"right\" node."))
