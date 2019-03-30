;;;; concrete-syntax.lisp --- Unit tests for the concrete syntax of the sexp grammar.
;;;;
;;;; Copyright (C) 2017-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sexp.test)

(in-suite :parser.packrat.grammar.sexp)

(test concrete-syntax.smoke
  "Smoke test for the concrete syntax of the sexp grammar."

  (grammar-test (grammar 'mock-grammar)
    (mapc (lambda (expression)
            (finishes (parser.packrat.grammar:parse-expression grammar expression)))

          '(;; Structure
            (structure symbol (name symbol-name))
            (structure 'type (name symbol-name))

            ;; `list' and `list*'
            (list a b)
            (list* a b)
            (list* a (rest b))

            ;; `vector' and `vector*'
            (vector a b)
            (vector* a b)

            ;; `cons'
            (cons a b)

            ;; `value'
            (parser.packrat.grammar.sexp::value (a) b)))))
