(cl:in-package #:parser.packrat.grammar.string)

(defclass simple-string-grammar (seq:sequence-grammar)
  ()
  (:default-initargs
   :sequence-type '(and simple-string (not (or simple-base-string (simple-array nil 1))))
   :element-type  'character
   :index-type    'array-index))

(defmethod grammar:default-environment ((grammar    simple-string-grammar)
                                        (expression t))
  (make-instance 'seq:vector-environment))

(defmethod grammar:parse ((grammar    simple-string-grammar)
                          (expression function)
                          (input      string))
  (let ((context (grammar::make-context grammar expression input)))
    (funcall expression context 0 input (length input))))
