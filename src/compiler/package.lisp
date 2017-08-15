(cl:defpackage #:parser.packrat.compiler
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:bp  #:architecture.builder-protocol)

   (#:exp #:parser.packrat.expression)
   (#:env #:parser.packrat.environment))

  (:export
   #:compile-expression
   #:compile-rule
   #:compile-rule-using-environment)

  ;; Code generation utilities
  (:export
   #:maybe-let
   #:maybe-let*
   #:maybe-progn))
