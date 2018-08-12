(cl:in-package #:parser.packrat.grammar.sequence)

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use base::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule base::expression ()
    (or (base:anything-expression)
        (base:constant-expression)

        (repetition-expression)
        (sequence-expression)))

(parser.packrat:defrule repetition-expression ()
    (list '* (:<- sub-expression (base::expression)))
  (bp:node* (:repetition)
    (* :sub-expression sub-expression)))

(parser.packrat:defrule sequence-expression ()
    (list (or :seq 'seq) (* (:<<- element-expressions (base::expression))))
  (bp:node* (:sequence)
    (* :sub-expression (nreverse element-expressions))))
