;;;; concrete-syntax.lisp --- Unit tests for the concrete syntax of the base grammar.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.base.test)

(in-suite :parser.packrat.grammar.base)

(test concrete-syntax.smoke
  "Smoke test for the concrete syntax of the base grammar."

  (grammar-test (grammar 'parser.packrat.grammar.base:base-grammar
                         :name            :test
                         :meta-grammar    'parser.packrat.grammar.base::meta-grammar
                         :meta-start-rule 'parser.packrat.grammar.base::expression)
    (mapc (lambda (expression)
            (let ((parser.packrat.grammar::*bootstrapping* nil))
              (finishes (print (parser.packrat.grammar:parse-expression grammar expression)))))

          '(;; constant
            1
            :foo
            'foo

            ;;
            name
            (:<- name)
            (:<- name 1)

            (:<<- name)
            (:<<- name 1)

            ;;
            (not 1)

            (and)
            (and 1)
            (and1 2)

            (or)
            (or 1)
            (or 1 2)

            (:compose)
            (:compose 1)
            (:compose 1 2)

            ;; rule-invocation-expression
            (foo)
            (foo 1)
            (foo 1 2)
            ((foo bar))
            ((foo bar) 1)
            ((foo bar) 1 2)

            ;; next-rule-invocation-expression
            (:next-rule)
            (:next-rule 1)
            (:next-rule 1 2)

            (and (:<- foo (not 1)) 'bar)))))
