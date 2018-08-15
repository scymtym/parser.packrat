;;;; concrete-syntax.lisp --- Unit tests for the concrete syntax of the sexp grammar.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sexp.test)

(in-suite :parser.packrat.grammar.sexp)

(test concrete-syntax.smoke
  "Smoke test for the concrete syntax of the sexp grammar."

  (grammar-test (grammar 'parser.packrat.grammar.sexp::sexp-grammar :name :test)
    (mapc (lambda (expression)
            (finishes (parser.packrat.grammar:parse-expression grammar expression)))

          '((structure symbol (name symbol-name))

            (list a b)
            (list* a b)
            (list* a (rest b))

            (vector a b)
            (vector* a b)

            (cons a b)

            (parser.packrat.grammar.sexp::value (a) b)))))
