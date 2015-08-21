;;;; macros.lisp --- Unit tests for macros provided by the architecture.builder-protocol system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.test)

(test with-builder.smoke.1
  "Smoke test for first syntax of the `with-builder' macro."

  (let ((result (with-builder ((make-instance
                                'preparable-finish-mock-builder))
                  (make-node *builder* :foo))))
    (is (equalp `(:finish ,(mock-node :foo '(:prepared? t))) result))))

(test with-builder.smoke.2
  "Smoke test for second syntax of the `with-builder' macro."

  (let ((result (with-builder (builder (make-instance
                                        'preparable-finish-mock-builder))
                  (make-node builder :foo))))
    (is (equalp `(:finish ,(mock-node :foo '(:prepared? t))) result))))
