;;;; package.lisp --- Package definition for tests for the grammar.base module.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.base.test
  (:use
   #:cl
   #:let-plus

   #:fiveam)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.base.test)

(def-suite :parser.packrat.grammar.base)

(defun run-tests ()
  (run! :parser.packrat.grammar.base))
