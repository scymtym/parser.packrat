(cl:in-package #:parser.packrat.grammar.sequence)

(defgeneric sequence-type (grammar)
  (:documentation
   "Return a type specifier describing the sequences accepted by GRAMMAR."))

(defgeneric element-type (grammar)
  (:documentation
   "Return a type specifier describing the elements of sequences accepted by GRAMMAR."))

(defgeneric index-type (grammar)
  (:documentation
   "Return a type specified describing the sequence indices used by GRAMMAR."))
