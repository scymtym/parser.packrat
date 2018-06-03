(cl:defpackage #:parser.packrat.expression
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  ;; TODO sort protocols
  (:shadow
   #:variable)

  (:export
   #:walk-expression)

  ;; Multiple sub-expressions protocol
  (:export
   #:sub-expressions)

  ;; Single sub-expression protocol
  (:export
   #:sub-expression)

  ;; Value protocol
  (:export
   #:value)

  ;; Variable protocol
  (:export
   #:variable
   #:mode

   #:direct-variable-references
   #:variable-references)

  ;; Rule invocation protocol
  (:export
   #:grammar
   #:rule
   #:arguments)

  (:export
   #:expression)

  (:export
   #:sub-expression-mixin
   #:single-sub-expression-mixin)

  ;; Macros
  (:export
   #:define-expression-class))
