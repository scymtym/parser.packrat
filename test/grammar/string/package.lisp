(cl:defpackage #:parser.packrat.grammar.string.test
  (:use
   #:cl

   #:fiveam)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.string.test)

;;; Test suite

(def-suite :parser.packrat.grammar.string)

(defun run-tests ()
  (run! :parser.packrat.grammar.string))

;;; Mock grammar

(defclass mock-grammar (parser.packrat.grammar.string::simple-string-grammar)
  ()
  (:default-initargs
   :name :test))
