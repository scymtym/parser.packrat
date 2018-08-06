(cl:defpackage #:parser.packrat.grammar.tree.test
  (:use
   #:cl

   #:fiveam)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.tree.test)

(def-suite :parser.packrat.grammar.tree)

(defun run-tests ()
  (run! :parser.packrat.grammar.tree))
