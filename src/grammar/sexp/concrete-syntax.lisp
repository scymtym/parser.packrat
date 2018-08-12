(cl:in-package #:parser.packrat.grammar.sexp)

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use seq::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule base::expression ()
    (or ; ((expression sequence-meta-grammar))

        (base:anything-expression)
        (base:constant-expression)

        (base::transform-expression)

        (seq:repetition-expression)
        (seq:sequence-expression)

        (structure-expression)

        (list-elements-expression)
        (rest-expression)
        (vector-elements-expression)

        (list-expression)
        (list*-expression)
        (vector-expression)
        (vector*-expression)
        (cons-expression)))

(parser.packrat:defrule structure-expression ()
    (list 'structure
          (:<- type (base::expression))
          (* (list (:<<- readers) ; TODO must be a function name
                   (:<<- sub-expressions (base::expression)))))
  (bp:node* (:structure)
    (1 :type           type)
    (* :reader         readers)
    (* :sub-expression sub-expressions)))

(parser.packrat:defrule list-elements-expression ()
    (list 'list-elements (:<- elements-expression (base::expression)))
  (bp:node* (:as-list)
    (1 :sub-expression elements-expression)))

(parser.packrat:defrule rest-expression ()
   (list 'rest (:<- rest-expression (base::expression)))
 (bp:node* (:rest)
   (1 :sub-expression rest-expression)))

(parser.packrat:defrule vector-elements-expression ()
    (list 'vector-elements (:<- elements-expression (base::expression)))
  (bp:node* (:as-vector)
    (1 :sub-expression elements-expression)))

(defmacro define-macro-rule (name expression expansion)
  `(parser.packrat:defrule ,name ()
       (:compose (:transform ,expression ,expansion)
                 (:<- result (base::expression)))
     result))

(define-macro-rule list-expression
    (list* 'list element-expressions)
  `(list-elements (:seq ,@element-expressions)))

(define-macro-rule list*-expression
    (list 'list* (* (and (:seq :any :any) (:<<- element-expressions :any))) last)
  `(list-elements (:seq ,@(nreverse element-expressions) (rest ,last))))

(define-macro-rule vector-expression
    (list* 'vector element-expressions)
  `(vector-elements (:seq ,@element-expressions)))

(define-macro-rule vector*-expression
    (list* 'vector* element-expressions)
  `(vector-elements (:seq ,@element-expressions)))

(define-macro-rule cons-expression
    (list 'cons car-expression cdr-expression)
  `(structure consp (car ,car-expression) (cdr ,cdr-expression)))
