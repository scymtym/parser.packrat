(cl:defpackage #:parser.packrat.expression
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:shadow
   #:variable)

  ;; Traversal protocol
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

  ;; Variables protocol
  (:export
   #:variable
   #:mode

   #:direct-variable-references
   #:variable-references)

  ;; Classes
  (:export
   #:expression)

  ;; Mixins
  (:export
   #:sub-expression-mixin
   #:single-sub-expression-mixin)

  ;; Macros
  (:export
   #:define-expression-class))
