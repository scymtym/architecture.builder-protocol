;;;; package.lisp --- Package definition for the json module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol.json
  (:use
   #:cl
   #:alexandria

   #:architecture.builder-protocol)

  (:export
   #:make-symbol-transform
   #:default-symbol-transform
   #:default-key-and-value-transform
   #:default-kind-key-and-value-transform

   #:raw

   #:make-serializer
   #:serialize-using-serializer
   #:serialize)

  (:documentation
   "JSON export of builder-constructed trees.

    The mapping to JSON constructs is as follows:

      node                     -> object

      initarg                  -> object property with directly
                                  encoded values

      relation of cardinality

        ?                      -> object property with recursively
                                  encoded value, if present

        1                      -> object property with recursively
                                  encoded value

        *                      -> object property with array value
                                  containing recursively encoded values

        (:map KEY)             -> object property with object value
                                  mapping values of the KEY relation
                                  argument to recursively encoded
                                  values

    For example, using the `list' builder, the tree

      (:foo ((:bar . *) (((:baz () :fez 1)))) :whoop 2)

    would be rendered as

                 property with array value containing nodes
                 vvvvvvvvvvvvvvvvv
      {\"whoop\":2,\"bar\":[{\"fez\":1}]}
       ^^^^^^^^^         ^^^^^^^
       initarg           initarg

    ."))
