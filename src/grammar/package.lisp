(cl:defpackage #:parser.packrat.grammar
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:c #:parser.packrat.compiler))

  ;; Conditions
  (:export
   #:grammar-condition
   #:grammar

   #:rule-condition
   #:rule

   #:grammar-missing-error

   #:rule-missing-error)

  ;; Name protocol
  (:export
   #:name)

  ;; Dependency protocol
  (:export
   #:dependencies
   #:dependents)

  ;; Rule protocol
  (:export
   #:expression
   #:rule-function)

  ;; Grammar protocol
  (:export
   #:find-rule

   #:ensure-rule
   #:ensure-rule-using-rule

   #:parse-expression

   #:parse)

  ;; Grammar namespace
  (:export
   #:find-grammar)

  ;; Mixins
  (:export
   #:named-mixin)

  (:export
   #:rule-storage-mixin
   #:ensure-grammar
   #:ensure-grammar-using-grammar))
