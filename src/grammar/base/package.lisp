(cl:defpackage #:parser.packrat.grammar.base
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.grammar
   #:parser.packrat.compiler)

  (:local-nicknames
   (#:bp    #:architecture.builder-protocol)

   (#:cache #:parser.packrat.cache)
   (#:exp   #:parser.packrat.expression)
   (#:env   #:parser.packrat.environment))

  ;; Expressions
  (:export
   #:anything-expression

   #:terminal-expression

   #:not-expression
   #:and-expression
   #:or-expression

   #:constant-expression
   #:variable-reference

   #:rule-invocation

   #:rule
   #:arguments
   #:set-expression
   #:push-expression

   #:transform-expression
   #:code

   #:predicate-expression
   #:predicate)

  ;; Grammar
  (:export
   #:base-grammar))
