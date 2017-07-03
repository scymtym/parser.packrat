(cl:defpackage #:parser.packrat.grammar
  (:use
   #:cl
   #:alexandria
   #:let-plus)

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

  (:export
   #:dependencies
   #:dependents)

  ;; Grammar protocol
  (:export
   #:find-rule

   #:ensure-rule
   #:ensure-rule-using-rule

   #:parse-expression

   #:parse)

  ;; Rule protocol
  (:export
   #:expression
   #:rule-function)

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
