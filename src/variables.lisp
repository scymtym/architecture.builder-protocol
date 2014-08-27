;;;; variables.lisp --- Variables provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:builder-protocol)

(defvar *builder* nil
  "Stores the builder object which should be used for constructing the
   result.")
