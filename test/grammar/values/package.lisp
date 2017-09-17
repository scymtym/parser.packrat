(cl:defpackage #:parser.packrat.grammar.values.test
  (:use
   #:cl

   #:fiveam)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.values.test)

(def-suite :parser.packrat.grammar.values)

(defun run-tests ()
  (run! :parser.packrat.grammar.values))
