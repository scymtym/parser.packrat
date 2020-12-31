;;;; package.lisp --- Expression compilation in the grammar.string module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.string.test
  (:use
   #:cl

   #:fiveam)

  (:local-nicknames
   (#:grammar #:parser.packrat.grammar)
   (#:c       #:parser.packrat.compiler))

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:import-from #:parser.packrat.grammar.sequence
   #:seq)

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
