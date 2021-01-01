;;;; package.lisp --- Package definition for grammar.stream module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.stream
  (:use
   #:cl
   #:let-plus)

  (:local-nicknames
   (#:a       #:alexandria)

   (#:exp     #:parser.packrat.expression)
   (#:env     #:parser.packrat.environment)
   (#:c       #:parser.packrat.compiler)

   (#:grammar #:parser.packrat.grammar)
   (#:base    #:parser.packrat.grammar.base)
   (#:seq     #:parser.packrat.grammar.sequence)))
