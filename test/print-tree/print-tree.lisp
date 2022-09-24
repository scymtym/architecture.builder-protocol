;;;; print-tree.lisp --- Unit tests for the print-tree module.
;;;;
;;;; Copyright (C) 2015-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:architecture.builder-protocol.print-tree.test)

(in-suite :architecture.builder-protocol.print-tree)

(test print-tree.smoke
  "Smoke test for the `print-tree' function."
  (flet ((test-case (tree expected)
           (is (equal (format nil expected)
                      (with-output-to-string (stream)
                        (with-standard-io-syntax
                          (print-tree 'list tree stream)))))))
    (mapc (a:curry #'apply #'test-case)
          '(((:foo ())
             "FOO")

            ((:foo () :bar 1)
             "FOO~@
              ~2@TBAR: 1")

            ((:foo () :bar 1 :baz 2)
             "FOO~@
              ~2@TBAR: 1~@
              ~2@TBAZ: 2")

            ;; One relation.
            ((:foo ((:fez . ?) (((:whoop ()) . ()))))
             "FOO~@
              └─FEZ: WHOOP")

            ((:foo ((:fez . 1) (((:whoop ()) . ()))))
             "FOO~@
              └─FEZ: WHOOP")

            ((:foo ((:fez . *) (((:whoop ()) . ())
                                ((:whooz ()) . ()))))
             "FOO~@
              ├─FEZ: WHOOP~@
              └─FEZ: WHOOZ")

            ((:foo ((:fez . (:map . :key)) (((:whoop ()) . (:key 1))
                                            ((:whooz ()) . (:key 2)))))
             "FOO~@
              ├─FEZ[1]: WHOOP~@
              └─FEZ[2]: WHOOZ")

            ;; Multiple relations.
            ((:foo ((:fez . ?) (((:whoop ()) . ()))
                    (:faz . ?) (((:whooz ()) . ()))))
             "FOO~@
              ├─FEZ: WHOOP~@
              └─FAZ: WHOOZ")

            ((:foo ((:fez . 1) (((:whoop ()) . ()))
                    (:faz . 1) (((:whooz ()) . ()))))
             "FOO~@
              ├─FEZ: WHOOP~@
              └─FAZ: WHOOZ")

            ((:foo ((:fez . *) (((:whoop ()) . ()))
                    (:faz . *) (((:whooz ()) . ()))))
             "FOO~@
              ├─FEZ: WHOOP~@
              └─FAZ: WHOOZ")

            ((:foo ((:fez . (:map . :key)) (((:whoop ()) . (:key 1)))
                    (:faz . (:map . :koy)) (((:whooz ()) . (:koy 2)))))
             "FOO~@
              ├─FEZ[1]: WHOOP~@
              └─FAZ[2]: WHOOZ")))))

(test print-tree.printers
  "Test supplying custom print functions to `print-tree'."
  (flet ((test-case (tree args expected)
           (is (equal (format nil expected)
                      (with-output-to-string (stream)
                        (with-standard-io-syntax
                          (apply #'print-tree 'list tree stream args)))))))
    (mapc (a:curry #'apply #'test-case)
          `(((:foo ((:fez . *) (((:whoop ()) . ())
                                ((:whooz ()) . ()))))
             (:printers ,(list (cons (lambda (node)
                                       (typep node '(cons (eql :foo))))
                                     (lambda (builder node stream)
                                       (format stream "<~A>"
                                               (architecture.builder-protocol:node-kind
                                                builder node))))))
             "<FOO>~@
              ├─FEZ: WHOOP~@
              └─FEZ: WHOOZ")

            ((:foo ((:fez . *) (((:whoop ()) . ())
                                ((:whooz ()) . ()))))
             (:printers ,(list (cons (lambda (node)
                                       (typep node '(cons (eql :whoop))))
                                     (lambda (builder node stream)
                                       (format stream "<~A>"
                                               (architecture.builder-protocol:node-kind
                                                builder node))))))
             "FOO~@
              ├─FEZ: <WHOOP>~@
              └─FEZ: WHOOZ")

            ((:foo ((:fez . *) (((:whoop ()) . ())
                                ((:whooz ()) . ()))))
             (:printers ,(list (cons (lambda (node)
                                       (typep node '(cons (eql :whooz))))
                                     (lambda (builder node stream)
                                       (format stream "<~A>"
                                               (architecture.builder-protocol:node-kind
                                                builder node))))))
             "FOO~@
              ├─FEZ: WHOOP~@
              └─FEZ: <WHOOZ>")))))
