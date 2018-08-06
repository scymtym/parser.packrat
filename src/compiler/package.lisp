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
   #:compile-expression)
  
  ;; Invocation compilation protocol
  (:export
   #:validate-invocation)

  (:export
   #:compile-rule
   #:compile-rule-using-environment
   #:prepare-expression)

  ;; Code generation utilities
  (:export
   #:maybe-let
   #:maybe-let*
   #:maybe-progn))
