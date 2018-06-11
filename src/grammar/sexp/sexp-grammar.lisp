(cl:in-package #:parser.packrat.grammar.sexp)

(defclass sexp-grammar (seq:sequential-grammar-mixin ; TODO needed?
                        parser.packrat.grammar.base:base-grammar)
  ())

(defmethod grammar:default-environment ((grammar sexp-grammar) (expression t))
  (make-instance 'env:value-environment :value 'object))
