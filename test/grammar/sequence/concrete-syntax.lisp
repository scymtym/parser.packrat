;;;; concrete-syntax.lisp --- Unit tests for the concrete syntax of the sequence grammar.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sequence.test)

(in-suite :parser.packrat.grammar.sequence)

(test concrete-syntax.smoke
  "Smoke test for the concrete syntax of the sequence grammar."

  (grammar-test (grammar 'mock-grammar)
    (mapc (lambda (expression)
            (finishes (parser.packrat.grammar:parse-expression grammar expression)))

          '(;; Repetition
            (* :any)
            (* :any 1)
            (* :any 1 2)
            (* 3)
            (* 3 1)
            (* 3 1 2)

            ;; Sequence
            (seq)
            (seq 1)
            (seq 1 2)

            ;; Zero or more
            (? 1)

            ;; One or more
            (+ :any)
            (+ 1)

            ;; Bounds
            (bounds (start end) 1)))))
