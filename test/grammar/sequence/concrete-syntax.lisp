;;;; concrete-syntax.lisp --- Unit tests for the concrete syntax of the sequence grammar.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sequence.test)

(in-suite :parser.packrat.grammar.sequence)

(test concrete-syntax.smoke
  "Smoke test for the concrete syntax of the sequence grammar."

  (grammar-test (grammar 'parser.packrat.grammar.sequence::sequence-grammar
                         :name            :test
                         :meta-grammar    'parser.packrat.grammar.sequence::meta-grammar
                         :meta-start-rule 'parser.packrat.grammar.base::expression)
    (mapc (lambda (expression)
            (let ((parser.packrat.grammar::*bootstrapping* nil))
              (finishes (parser.packrat.grammar:parse-expression grammar expression))))

          '(;; Repetition
            (* :any)
            (* :any 1)
            (* :any 1 2)

            ;; Sequence
            (:seq)
            (:seq 1)
            (:seq 1 2)

            ;; Zero or more
            (parser.packrat.grammar.sequence::? 1)

            ;; One or more
            (+ 1)

            ;; Bounds
            (parser.packrat.grammar.sequence::bounds (start end) 1)))))
