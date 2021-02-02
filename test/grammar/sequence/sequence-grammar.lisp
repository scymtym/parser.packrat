;;;; sequence-grammar.lisp --- Unit tests for the sequence-grammar class.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sequence.test)

(in-suite :parser.packrat.grammar.sequence)

(test sequence-grammar.smoke
  "Smoke test for `sequence-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar)
    (rules-test (grammar)
      '((or (seq #\f #\o #\o (* (seq #\b #\a)) #\r)
            (seq (seq #\f #\o #\o) (seq #\b #\a #\z)))

        ("foobar" (t 6))))))
