;;;; package.lisp --- Package definition for tests of the grammar.values module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.values.test
  (:use
   #:cl

   #:fiveam)

  (:local-nicknames
   (#:c       #:parser.packrat.compiler)

   (#:grammar #:parser.packrat.grammar))

  (:import-from #:parser.packrat.grammar.base
   #:<-)

  (:import-from #:parser.packrat.grammar.sequence
   #:seq)

  (:import-from #:parser.packrat.grammar.test
   #:grammar-test
   #:rules-test)

  (:export
   #:run-tests))

(cl:in-package #:parser.packrat.grammar.values.test)

;;; Test suite

(def-suite :parser.packrat.grammar.values)

(defun run-tests ()
  (run! :parser.packrat.grammar.values))

;;; Mock grammar

(defclass mock-grammar (parser.packrat.grammar.sexp::sexp-grammar)
  ((%values :initarg :values
            :reader  values*))
  (:default-initargs
   :name            :test
   :meta-grammar    'parser.packrat.grammar.sexp::meta-grammar
   :meta-start-rule 'parser.packrat.grammar.base::expression))

(defmethod parser.packrat.grammar:default-environment ((grammar mock-grammar) (expression t))
  (let* ((values (values* grammar))
         (value  (first values)))
    (make-instance 'parser.packrat.grammar.values::values-environment :value  value
                                                                      :values values)))
