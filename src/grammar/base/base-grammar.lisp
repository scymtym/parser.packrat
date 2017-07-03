(cl:in-package #:parser.packrat.grammar.base)

(defclass base-grammar (named-mixin
                        rule-storage-mixin
                        print-items:print-items-mixin)
  ())
