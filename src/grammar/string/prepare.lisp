(cl:in-package #:parser.packrat.grammar.string)

(defmethod prepare-expression :around ((grammar     simple-string-grammar)
                                       (environment seq:sequential-environment-mixin)
                                       (expression  base:terminal-expression))
  (typecase (exp:value expression)
    (string
     (change-class expression 'string-terminal-expression))
    (t
     (call-next-method))))
