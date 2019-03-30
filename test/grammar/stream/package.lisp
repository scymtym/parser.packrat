(cl:defpackage #:parser.packrat.grammar.stream.test
  (:use
   #:cl

   #:fiveam)

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
