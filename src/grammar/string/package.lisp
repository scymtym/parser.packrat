;;;; package.lisp --- Package definition for the grammar.string module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.string
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:exp     #:parser.packrat.expression)
   (#:env     #:parser.packrat.environment)
   (#:c       #:parser.packrat.compiler)

   (#:grammar #:parser.packrat.grammar)
   (#:base    #:parser.packrat.grammar.base)
   (#:seq     #:parser.packrat.grammar.sequence))

  (:export
   #:simple-string-grammar))
