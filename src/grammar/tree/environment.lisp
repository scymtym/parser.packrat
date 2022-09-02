(cl:in-package #:parser.packrat.grammar.tree)

(defclass generator-environment (env:environment
                                 seq:sequential-environment-mixin)
  ;; TODO make a two-part state with value and generator in separate variables
  ((%generator :initarg :generator
               :type    symbol
               :reader  generator)))

(env:define-state-methods generator-environment
  (generator)
  ())
