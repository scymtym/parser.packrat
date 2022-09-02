(cl:in-package #:parser.packrat.grammar.tree)

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use parser.packrat.grammar.sexp::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule ancestors-expression (context)
    (list 'ancestors (<- sub-expression (base::expression context)))
  (make-instance 'ancestors-expression :sub-expression sub-expression))

(parser.packrat:defrule children-expression (context)
    (list 'children (<- sub-expression (base::expression context)))
  (make-instance 'children-expression :sub-expression sub-expression))

(parser.packrat:defrule base::expression (context)
  (or (ancestors-expression context)
      (children-expression context)

      ((base::expression parser.packrat.grammar.sexp::meta-grammar) context)))
