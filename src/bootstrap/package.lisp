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
   #:must)

  (:import-from #:parser.packrat.grammar.sequence
   #:?)

  (:local-nicknames
   (#:base #:parser.packrat.grammar.base)
   (#:seq  #:parser.packrat.grammar.sequence))

  (:export
   #:parse))
