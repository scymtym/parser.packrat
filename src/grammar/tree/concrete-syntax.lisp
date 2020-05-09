(cl:in-package #:parser.packrat.grammar.tree)

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use parser.packrat.grammar.base::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule ancestors-expression (context)
    (list 'ancestors (:<- sub-expression (parser.packrat.grammar.base::expression context)))
  (make-instance 'ancestors-expression :sub-expression sub-expression))

(parser.packrat:defrule children-expression (context)
  (list 'children (:<- sub-expression (parser.packrat.grammar.base::expression context)))
  (make-instance 'children-expression :sub-expression sub-expression))
