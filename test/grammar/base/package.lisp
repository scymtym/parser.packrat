;;;; package.lisp --- Package definition for tests for the grammar.base module.
;;;;
;;;; Copyright (C) 2017-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.base.test
  (:use
   #:cl
   #:let-plus

   #:fiveam

   #:parser.packrat.grammar.base)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.base.test)

;;; Test suite

(def-suite :parser.packrat.grammar.base)

(defun run-tests ()
  (run! :parser.packrat.grammar.base))

;;; Mock grammar

(defclass mock-grammar (parser.packrat.grammar.base:base-grammar)
  ()
  (:default-initargs
   :name            :test
   :meta-grammar    'parser.packrat.grammar.base::meta-grammar
   :meta-start-rule 'parser.packrat.grammar.base::expression))

(defmethod parser.packrat.grammar:default-environment ((grammar    mock-grammar)
                                                       (expression t))
  (make-instance 'parser.packrat.environment:value-environment :value 'value))
