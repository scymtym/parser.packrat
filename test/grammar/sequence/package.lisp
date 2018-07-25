;;;; package.lisp --- Package definition for tests for the grammar.sequence module.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.sequence.test
  (:use
   #:cl
   #:let-plus

   #:fiveam)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.sequence.test)

(def-suite :parser.packrat.grammar.sequence)

(defun run-tests ()
  (run! :parser.packrat.grammar.sequence))
