;;;; util.lisp --- Utilities used by the builder-protocol.xpath module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.xpath)

(declaim (inline string->name))
(defun string->name (string)
  (substitute #\_ #\/ string))

(defun symbol->name (symbol)
  (string->name (string-downcase symbol)))

(defun symbol->namespace (symbol)
  (if-let ((package (when (typep symbol '(and symbol (not keyword)))
                      (package-name (symbol-package symbol)))))
    (string->name (string-downcase package))
    ""))
