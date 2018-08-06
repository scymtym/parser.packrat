(cl:in-package #:parser.packrat.grammar.sexp)

(defclass sexp-grammar (parser.packrat.grammar.base:base-grammar)
  ()
  (:documentation
   "A grammar class for matching Lisp objects.

    In particular, this grammar class adds constructs for matching
    lists (`list', `list*', `rest'), vectors (`vector', `vector*') and
    structures including conses (`structure', `cons').

    Rules defined in grammars of this class operate on objects but can
    switch to sequence-based processing using the `as-list' and
    `as-vector' constructs."))

(defmethod grammar:default-environment ((grammar sexp-grammar) (expression t))
  (make-instance 'env:value-environment :value 'object))
