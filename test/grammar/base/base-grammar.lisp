;;;; base-grammar.lisp --- Unit tests for the base-grammar class.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.base.test)

(in-suite :parser.packrat.grammar.base)

(defclass mock-grammar (parser.packrat.grammar.base:base-grammar)
  ())

(defmethod parser.packrat.grammar:default-environment ((grammar    mock-grammar)
                                                       (expression t))
  (make-instance 'parser.packrat.environment:value-environment :value 'value))

(test base-grammar.smoke
  "Smoke test for `base-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar :name :test)
    (rules-test (grammar)
      '((or 1 2)

        (0 (nil 0))
        (1 (t   1 1))
        (2 (t   2 2))))))
