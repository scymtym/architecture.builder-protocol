;;;; package.lisp --- Package definition for the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.builder-protocol
  (:use
   #:cl
   #:alexandria)

  ;; Variables
  (:export
   #:*builder*)

  ;; Builder protocol
  (:export
   #:prepare          #:prepare*
   #:finish           #:finish*
   #:wrap             #:wrap*

   #:make-node        #:make-node*
   #:finish-node      #:finish-node*
   #:relate           #:relate*

   #:make+finish-node #:make+finish-node*)

  ;; `with-builder' macro
  (:export
   #:call-with-builder
   #:with-builder)

  ;; `top-down-forcing-builder'
  (:export
   #:top-down-forcing-builder)

  (:documentation
   "This package contains the builder protocol.

    The protocol consists of two groups of generic functions

    1. `prepare', `finish' and `wrap'
    2. `make-node', `finish-node' and `relate'

    and the special variable `*builder*'.

    All of the above functions take an explicit builder argument which
    is usually the value of `*builder*' .The convenience functions
    `prepare*', `finish*', `wrap*', `make-node*', `finish-node*' and
    `relate*' lack the builder argument and use the value of
    `*builder*' automatically.

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
