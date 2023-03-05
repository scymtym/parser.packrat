(cl:defpackage #:parser.packrat.test
  (:use
   #:cl

   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.test)

;;; Test suite

(def-suite :parser.packrat.interface)

(defun run-tests ()
  (run! :parser.packrat.interface))
