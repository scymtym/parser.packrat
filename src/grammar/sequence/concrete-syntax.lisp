(cl:in-package #:parser.packrat.grammar.sequence)

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use base::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

;;;

(parser.packrat:defrule base::expression ()
    (or (base:anything-expression)
        (base:constant-expression)

        (repetition-expression)
        (sequence-expression)

        (?-expression)
        (+-expression)))

(parser.packrat:defrule repetition-expression ()
    (list '* (:<- sub-expression (base::expression))
          (* (:<- min (base::expression)) 0 1) ; TODO use ? for min max
          (* (:<- max (base::expression)) 0 1))
  (bp:node* (:repetition)
    (1    :sub-expression  sub-expression)
    (bp:? :max-repetitions min)
    (bp:? :min-repetitions max)))

(parser.packrat:defrule sequence-expression ()
    (list (or :seq 'seq) (* (:<<- element-expressions (base::expression))))
  (bp:node* (:sequence)
    (* :sub-expression (nreverse element-expressions))))

;;; Syntactic sugar

(defmacro define-macro-rule (name expression expansion) ; TODO there is also a define-macro rule in sexp/concrete-syntax
  `(parser.packrat:defrule ,name ()
       (:compose (:transform ,expression ,expansion)
                 (:<- result (base::expression))) ; TODO shouldn't the result variable be unnecessary?
     result))

(define-macro-rule ?-expression
    (list '? expression)
  `(* ,expression 0 1))

(define-macro-rule +-expression
    (list '+ expression)
  `(* ,expression 1))
