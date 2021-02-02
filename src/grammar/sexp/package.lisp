;;;; package.lisp --- Package definition for the grammar.sexp module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.sexp
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.compiler)

  (:import-from #:parser.packrat.expression
   #:sub-expression
   #:sub-expressions)

  (:import-from #:parser.packrat.grammar.base
   #:guard
   #:<- #:<<-
   #:must)

  (:local-nicknames
   (#:bp      #:architecture.builder-protocol)

   (#:exp     #:parser.packrat.expression)
   (#:env     #:parser.packrat.environment)

   (#:grammar #:parser.packrat.grammar)
   (#:base    #:parser.packrat.grammar.base)
   (#:seq     #:parser.packrat.grammar.sequence))

  (:export
   #:sexp-grammar))
