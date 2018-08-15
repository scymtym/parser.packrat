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
          
          '((* :any)
            (* :any 1)
            (* :any 1 2)

            (:seq)
            (:seq 1)
            (:seq 1 2)
                      
            (parser.packrat.grammar.sequence::? 1)

            (+ 1)))))

