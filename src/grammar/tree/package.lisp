;;;; package.lisp --- Package definition for the grammar.tree module.
;;;;
;;;; Copyright (C) 2018-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.tree
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.compiler)

  (:local-nicknames
   (#:exp  #:parser.packrat.expression)

   (#:env  #:parser.packrat.environment)

   (#:base #:parser.packrat.grammar.base)
   (#:seq  #:parser.packrat.grammar.sequence))

  (:import-from #:parser.packrat.grammar.base
   #:<-))
