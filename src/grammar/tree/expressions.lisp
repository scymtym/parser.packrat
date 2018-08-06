(cl:in-package #:parser.packrat.grammar.tree)

(defclass ancestor-expression-mixin (exp::value-environment-needing-mixin
                                     exp:single-sub-expression-mixin)
  ())

(exp:define-expression-class children (ancestor-expression-mixin)
  ())

(exp:define-expression-class ancestors (ancestor-expression-mixin)
  ())
