;;;; package.lisp --- Package definition for the bootstrap module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.bootstrap
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.expression)

  (:shadowing-import-from #:parser.packrat.expression
   #:variable)

  (:shadowing-import-from #:parser.packrat.grammar.base
   #:value)

  (:import-from #:parser.packrat.grammar.base
   #:guard
   #:<- #:<<-
   #:must
   #:next-rule)

  (:import-from #:parser.packrat.grammar.sequence
   #:seq #:?)

  (:local-nicknames
   (#:base #:parser.packrat.grammar.base)
   (#:seq  #:parser.packrat.grammar.sequence))

  (:export
   #:parse))
