(cl:in-package #:parser.packrat.grammar.sequence)

(defgeneric compile-bounds-test (grammar environment in-cont out-cont)
  (:documentation
   "TODO"))

(defgeneric compile-access (grammar environment cont)
  (:documentation
   "TODO"))

(defgeneric compile-advance (grammar environment cont
                             &key amount to) ; TODO amount and to seem unused; remove
  (:documentation
   "TODO"))
