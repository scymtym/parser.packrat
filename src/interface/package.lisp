;;;; package.lisp --- Package definition for the interface module.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.base)

  (:local-nicknames
   (#:exp     #:parser.packrat.expression)
   (#:env     #:parser.packrat.environment)
   (#:grammar #:parser.packrat.grammar)

   (#:base    #:parser.packrat.grammar.base))

  (:import-from #:trivial-with-current-source-form
   #:with-current-source-form)

  ;; Parsing protocol
  (:export
   #:parse)

  ;; Macros
  (:export
   #:defgrammar
   #:in-grammar

   #:defrule

   #:match #:ematch #:cmatch))
