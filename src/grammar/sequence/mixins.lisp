(cl:in-package #:parser.packrat.grammar.sequence)

(defclass sequential-grammar-mixin ()
  ())

(defmethod grammar:default-environment ((grammar    sequential-grammar-mixin)
                                        (expression t))
  (make-instance 'sequence-environment))
