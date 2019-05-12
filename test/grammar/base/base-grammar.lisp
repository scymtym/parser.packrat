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
      '((guard evenp)

        (0 (t   0))
        (1 (nil 1))
        (2 (t   2)))

      '((or 1 2)

        (0 (nil 0))
        (1 (t   1 1))
        (2 (t   2 2)))

      '((or (:must 1 "1") 2)

        (0 (:fatal 0 "1"))
        (1 (t      1 1))
        (2 (:fatal 2 "1"))))))
