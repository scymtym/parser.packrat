;;;; util.lisp --- Utilities for grammar unit tests.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.test)

(defun call-as-grammar-test (thunk grammar-class grammar-initargs)
  (let ((grammar (apply #'make-instance grammar-class grammar-initargs)))
    (funcall thunk grammar)))

(defmacro grammar-test ((grammar-var grammar-class &rest grammar-initargs)
                        &body body)
  `(call-as-grammar-test
    (lambda (,grammar-var) ,@body) ,grammar-class (list ,@grammar-initargs)))

(defun run-rules-test (grammar clauses)
  (mapc (lambda+ ((expression &rest cases))
          (mapc (lambda+ ((input (expected-success? expected-position
                                  &optional (expected-value nil expected-value-supplied?))))
                  (let+ (((&values success? position value)
                          (parser.packrat.grammar:parse grammar expression input)))
                    (is (eq expected-success? success?))
                    (is (equalp expected-position position))
                    (when expected-value-supplied?
                      (is (equalp expected-value value)))))
                cases))
        clauses))

(defmacro rules-test ((grammar) &body clauses)
  `(run-rules-test ,grammar (list ,@clauses)))
