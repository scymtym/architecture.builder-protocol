;;;; package.lisp --- Package definition for the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol
  (:use
   #:cl
   #:alexandria)

  ;; Types
  (:export
   #:?                              ; cardinality specifier
   #:relation-cardinality)

  ;; Variables
  (:export
   #:*builder*)

  ;; Build protocol
  (:export
   #:prepare                    #:prepare*
   #:finish                     #:finish*
   #:wrap                       #:wrap*

   #:make-node                  #:make-node*
   #:finish-node                #:finish-node*
   #:relate                     #:relate*

   #:add-relations              #:add-relations*
   #:make+finish-node           #:make+finish-node*
   #:make+finish-node+relations #:make+finish-node+relations*)

  ;; Macros related to build protocol
  (:export
   #:node                       #:node*

   #:call-with-builder
   #:with-builder)

  ;; "Un-build" protocol
  (:export
   #:node-kind      #:node-kind*
   #:node-initargs  #:node-initargs*
   #:node-relations #:node-relations*
   #:node-relation  #:node-relation*)

  ;; Node walking protocol
  (:export
   #:walk-nodes     #:walk-nodes*

   #:peeking)

  ;; Macros related to "un-build" protocol
  (:export
   #:call-with-unbuilder
   #:with-unbuilder)

  ;; Delegation protocol and `delegating-mixin'
  (:export
   #:target

   #:delegating-mixin)

  ;; `forwarding-mixin'
  (:export
   #:forwarding-mixin)

  ;; `delaying-mixin'
  (:export
   #:delaying-mixin)

  ;; `order-forcing-mixin'
  (:export
   #:builder-visit-function

   #:order-forcing-mixin)

  ;; `top-down-forcing-builder'
  (:export
   #:top-down-forcing-builder)

  ;; Utilities
  (:export
   #:normalize-relation

   #:cardinality-case #:cardinality-ecase)

  (:documentation
   "This package contains the build and \"un-build\" protocols.

    The build protocol consists of two groups of generic functions

    1. `prepare', `finish' and `wrap'
    2. `make-node', `finish-node' and `relate'

    and the special variable `*builder*'.

    All of the above functions take an explicit builder argument which
    is usually the value of `*builder*' .The convenience functions
    `prepare*', `finish*', `wrap*', `make-node*', `finish-node*' and
    `relate*' lack the builder argument and use the value of
    `*builder*' automatically.

    Further convenience functions `make+finish-node[*]',
    `make+finish-node+relation[*]' and macros `node[*]' combine common
    idioms involving the above functions into a single function or
    macro respectively.

    For construction of an object graph, a client binds
    `*builder*' (for example via `with-builder') to an object of the
    client's choice for which methods on the above protocol generic
    functions exist and calls the object graph constructing
    code (e.g. a parser). This way, the client can obtain different
    representations by supplying different builders and the object
    graph construction code does not have to know the concrete
    representation of the result it constructs.

    This package also contains a builder for constructing list-based
    representations which are useful for debugging and unit
    tests. This builder can be selected by binding `*builder*' to the
    symbol `cl:list'."))
