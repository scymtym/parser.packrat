;;;; base-grammar.lisp --- Unit tests for the base-grammar class.
;;;;
;;;; Copyright (C) 2017-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.base.test)

(in-suite :parser.packrat.grammar.base)

(test base-grammar.smoke
  "Smoke test for `base-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar)
    (rules-test (grammar)
      '((or 1 2)

        (0 (nil 0))
        (1 (t   1 1))
        (2 (t   2 2))))))
