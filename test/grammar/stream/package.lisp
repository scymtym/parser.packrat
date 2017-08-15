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

(def-suite :parser.packrat.grammar.stream)

(defun run-tests ()
  (run! :parser.packrat.grammar.stream))
