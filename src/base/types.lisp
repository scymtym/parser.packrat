(cl:in-package #:parser.packrat.base)

(deftype grammar-designator ()
  "Things which can be coerced into STRINGs by CL:STRING can designate
   grammars."
  '(or symbol string))

(deftype rule-designator ()
  "TODO"
  '(or symbol string))
