;;;; simple-string-grammar.lisp --- Grammar class for simple strings.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.string)

(defclass simple-string-grammar (seq:sequence-grammar)
  ()
  (:default-initargs
   :sequence-type   '(and simple-string (not (or simple-base-string (simple-array nil 1))))
   :element-type    'character
   :index-type      'array-index

   :meta-grammar    'meta-grammar
   :meta-start-rule 'base::expression))

(defmethod grammar:default-environment ((grammar    simple-string-grammar)
                                        (expression t))
  (make-instance 'seq:vector-environment))

(defmethod grammar:parse ((grammar    simple-string-grammar)
                          (expression function)
                          (input      string))
  (let ((context (grammar::make-context grammar expression input)))
    (funcall expression context 0 input (length input))))
