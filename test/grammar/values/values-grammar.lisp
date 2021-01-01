;;;; values-grammar.lisp --- Tests for values-grammar class.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.values.test)

(in-suite :parser.packrat.grammar.values)

(test values-grammar.smoke
  "Smoke test for the `values-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar :values '(a b c))

    (let ((function (compile nil (c:compile-rule
                                  grammar 'foo '()
                                  (grammar:parse-expression
                                   grammar '(:transform
                                               (seq (<- x (or 1 2))
                                                    (<- y (or 3 4))
                                                    (<- z (or 5 6)))
                                             (list x y z)))))))
      (multiple-value-bind (success value)
          (funcall function nil 1 3 5)
        (is (eq    t        success))
        (is (equal '(1 3 5) value))))))
