(cl:defpackage #:parser.packrat.grammar.string.test
  (:use
   #:cl

   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.string.test)

(def-suite :parser.packrat.grammar.string)

(defun run-tests ()
  (run! :parser.packrat.grammar.string))
