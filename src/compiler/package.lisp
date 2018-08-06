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
   #:prepare-expression
   #:compile-expression)

  ;; Invocation compilation protocol
  (:export
   #:validate-invocation)

  ;; Rule compilation protocol
  (:export
   #:compile-rule
   #:compile-rule-using-environment
   #:make-rule-lambda)

  ;; Code generation utilities
  (:export
   #:maybe-let
   #:maybe-let*
   #:maybe-progn))
