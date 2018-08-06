(cl:in-package #:parser.packrat.grammar.sequence)

(defclass sequence-grammar (parser.packrat.grammar.base:base-grammar
                            sequential-grammar-mixin)
  ((sequence-type :initarg :sequence-type
                  :reader  sequence-type)
   (element-type  :initarg :element-type
                  :reader  element-type)
   (index-type    :initarg :index-type
                  :reader  index-type))
  (:default-initargs
   :sequence-type 'sequence
   :element-type  't
   :index-type    'array-index))

(defmethod grammar:default-environment ((grammar    sequence-grammar)
                                        (expression t))
  (make-instance 'sequence-environment))

(defmethod grammar:parse ((grammar    sequence-grammar)
                          (expression function)
                          (input      sequence))
  (let ((context (grammar::make-context grammar expression input)))
    (funcall expression context 0 input (length input))))
