(cl:defpackage #:parser.packrat.grammar.sequence
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.compiler)

  (:local-nicknames
   (#:bp   #:architecture.builder-protocol)

   (#:exp  #:parser.packrat.expression)
   (#:env  #:parser.packrat.environment)

   (#:base #:parser.packrat.grammar.base))

  ;; Expressions
  (:export
   #:min-repetitions
   #:max-repetitions

   #:repetition-expression

   #:sequence-expression)

  ;; Sequence environment protocol
  (:export
   #:sequence*
   #:position*
   #:end

   #:sequence-environment)

  ;; List environment protocol and class
  (:export
   #:tail

   #:list-environment)

  ;; Vector environment class
  (:export
   #:vector-environment)

  ;; Mixins
  (:export
   #:sequential-grammar-mixin
   #:sequential-environment-mixin)

  ;; Sequence grammar
  (:export
   #:sequence-grammar))
