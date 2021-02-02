;;;; package.lisp --- Package definition for tests for the grammar.sequence module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.sequence.test
  (:use
   #:cl
   #:let-plus

   #:fiveam)

  (:import-from #:parser.packrat.grammar.sequence
   #:? #:seq #:bounds)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.sequence.test)

;;; Test suite

(def-suite :parser.packrat.grammar.sequence)

(defun run-tests ()
  (run! :parser.packrat.grammar.sequence))

;;; Meta grammar for tests

(parser.packrat:defgrammar meta-grammar
  (:class   parser.packrat.grammar.sexp:sexp-grammar)
  (:cached? nil)
  (:use     parser.packrat.grammar.sequence::meta-grammar
            parser.packrat.grammar.base::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule parser.packrat.grammar.base::expression (context)
    (or ((parser.packrat.grammar.base::expression parser.packrat.grammar.sequence::meta-grammar) context)
        ((parser.packrat.grammar.base::expression parser.packrat.grammar.base::meta-grammar) context)))

;;; Mock grammar

(defclass mock-grammar (parser.packrat.grammar.sequence::sequence-grammar)
  ()
  (:default-initargs
   :name            :test
   :meta-grammar    'meta-grammar
   :meta-start-rule 'parser.packrat.grammar.base::expression))
