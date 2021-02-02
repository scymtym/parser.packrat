;;;; types.lisp --- Types provided by the base module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.base)

(deftype grammar-designator ()
  "An object naming a grammar.

   Basically things which can be coerced into `string's by the
   function `cl:string'."
  '(or symbol string))

(deftype rule-designator ()
  "An object naming a rule within a given grammar.

   Basically things which ca be coerced into `string's by the function
  `cl:string'."
  '(or symbol string))
