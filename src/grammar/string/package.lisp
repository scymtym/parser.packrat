(cl:defpackage #:parser.packrat.grammar.string
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.compiler)

  (:local-nicknames
   (#:exp     #:parser.packrat.expression)
   (#:env     #:parser.packrat.environment)

   (#:grammar #:parser.packrat.grammar)
   (#:base    #:parser.packrat.grammar.base)
   (#:seq     #:parser.packrat.grammar.sequence))

  (:export
   #:simple-string-grammar))
