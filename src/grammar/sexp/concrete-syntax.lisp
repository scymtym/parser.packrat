(cl:in-package #:parser.packrat.grammar.sexp)

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use seq::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule grammar:expression (context)
    (or (structure-expression context)

        (list-elements-expression context)
        (rest-expression context)
        (vector-elements-expression context)

        (list-expression context)
        (list*-expression context)
        (vector-expression context)
        (vector*-expression context)

        (cons-expression context)

        (value-expression context)

        ((base::expression seq::meta-grammar) context)

        ((base::expression base::meta-grammar) context)))

(parser.packrat:defrule structure-expression (context)
    (list 'structure
          (:<- type (grammar:expression :value))
          (* (list (:<<- readers) ; TODO must be a function name
                   (:<<- sub-expressions (base::expression context)))))
  (bp:node* (:structure)
    (1 :type           type)
    (* :reader         (nreverse readers))
    (* :sub-expression (nreverse sub-expressions))))

(parser.packrat:defrule list-elements-expression (context)
    (list 'list-elements (:<- elements-expression (grammar:expression context)))
  (bp:node* (:as-list)
    (1 :sub-expression elements-expression)))

(parser.packrat:defrule rest-expression (context)
   (list 'rest (:<- rest-expression (base::expression context)))
 (bp:node* (:rest)
   (1 :sub-expression rest-expression)))

(parser.packrat:defrule vector-elements-expression (context)
    (list 'vector-elements
          (:<- elements-expression (grammar:expression context)))
  (bp:node* (:as-vector)
    (1 :sub-expression elements-expression)))

;;; Syntactic sugar

(defmacro define-macro-rule (name expression expansion)
  `(parser.packrat:defrule ,name (context)
       (:compose (:transform ,expression ,expansion)
                 (base::expression context))))

(define-macro-rule list-expression
    (list* 'list element-expressions)
  `(list-elements (:seq ,@element-expressions)))

(define-macro-rule list*-expression
    (list 'list* (* (and (:seq :any :any) (:<<- element-expressions))) last)
  `(list-elements (:seq ,@(nreverse element-expressions) (rest ,last))))

(define-macro-rule vector-expression
    (list* 'vector element-expressions)
  `(vector-elements (:seq ,@element-expressions)))

(define-macro-rule vector*-expression
    (list* 'vector* element-expressions)
  `(vector-elements (:seq ,@element-expressions)))

;;;

(define-macro-rule cons-expression
    (list 'cons car-expression cdr-expression)
  `(structure 'cons (car ,car-expression) (cdr ,cdr-expression)))

;;;

(define-macro-rule value-expression
    (list 'value (list (:guard value symbolp)
          (:<- expression (base::expression))))
  `(and (<- ,value) ,expression))
