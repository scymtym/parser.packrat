(cl:in-package #:parser.packrat.grammar.base)

(defclass base-grammar (named-mixin
                        rule-storage-mixin
                        print-items:print-items-mixin)
  ())
(declaim (inline %make-context))
(defstruct (context
            (:constructor %make-context (grammar)))
  (cache   (make-hash-table :test #'equal) :read-only t)
  (grammar nil                             :read-only t))

(defmethod parser.packrat.grammar::make-context ((grammar    base-grammar)
                                  (expression t)
                                  (input      t))
  (%make-context grammar))
