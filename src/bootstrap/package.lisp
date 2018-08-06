(cl:defpackage #:parser.packrat.bootstrap
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.expression)

  (:shadowing-import-from #:parser.packrat.expression
   #:variable)

  (:local-nicknames
   (#:base #:parser.packrat.grammar.base)
   (#:seq  #:parser.packrat.grammar.sequence)))
