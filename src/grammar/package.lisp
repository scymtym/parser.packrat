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

   #:rule-missing-error

   #:expression-syntax-error
   #:expression)

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
   #:rules
   #:rules/alist

   #:find-rule                      ; also `setf'

   #:ensure-rule
   #:ensure-rule-using-rule

   #:parse-expression

   #:parse

   #:default-environment)

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
