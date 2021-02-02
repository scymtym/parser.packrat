;;;; package.lisp --- Package definition for tests for the stream grammar module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.stream.test
  (:use
   #:cl

   #:fiveam)

  (:import-from #:parser.packrat.grammar.sequence
   #:seq)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.stream.test)

;;; Test suite

(def-suite :parser.packrat.grammar.stream)

(defun run-tests ()
  (run! :parser.packrat.grammar.stream))

;;; Mock grammar

(defclass mock-grammar (parser.packrat.grammar.stream::stream-grammar)
  ()
  (:default-initargs
   :meta-grammar    'parser.packrat.grammar.sexp::meta-grammar
   :meta-start-rule 'parser.packrat.grammar.base::expression
   :name            :test))
