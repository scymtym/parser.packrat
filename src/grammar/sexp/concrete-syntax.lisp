;;;; concrete-syntax.lisp --- Meta-grammar rules for the sexp grammar module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sexp)

(parser.packrat:defgrammar meta-grammar
  (:class   parser.packrat.grammar.sexp:sexp-grammar)
  (:cached? nil)
  (:use     seq::meta-grammar)) ; TODO base meta-grammar
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule base::expression (context)
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
          (<- type (base::expression! :value))
          (* (and (list* :any)
                  (must (list (<<- readers #+TODO (base::function-name)) ; TODO must be a function name
                              (<<- sub-expressions (base::expression! context)))
                        "a list of two elements"))))
  (bp:node* (:structure)
    (1 :type           type)
    (* :reader         (nreverse readers))
    (* :sub-expression (nreverse sub-expressions))))

(parser.packrat:defrule list-elements-expression (context)
    (list 'list-elements (<- elements-expression (base::expression! context)))
  (bp:node* (:as-list)
    (1 :sub-expression elements-expression)))

(parser.packrat:defrule rest-expression (context)
   (list 'rest (<- rest-expression (base::expression! context)))
 (bp:node* (:rest)
   (1 :sub-expression rest-expression)))

(parser.packrat:defrule vector-elements-expression (context)
    (list 'vector-elements
          (<- elements-expression (base::expression! context)))
  (bp:node* (:as-vector)
    (1 :sub-expression elements-expression)))

;;; Syntactic sugar

(defmacro define-macro-rule (name expression expansion)
  `(parser.packrat:defrule ,name (context)
       (:compose (:transform ,expression ,expansion)
                 (base::expression context))))

(define-macro-rule list-expression
    (list* 'list element-expressions)
  `(list-elements (seq ,@element-expressions)))

(define-macro-rule list*-expression
    (list 'list* (* (and (seq :any :any) (<<- element-expressions))) last)
  `(list-elements (seq ,@(nreverse element-expressions) (rest ,last))))

(define-macro-rule vector-expression
    (list* 'vector element-expressions)
  `(vector-elements (seq ,@element-expressions)))

(define-macro-rule vector*-expression
    (list* 'vector* element-expressions)
  `(vector-elements (seq ,@element-expressions)))

;;;

(define-macro-rule cons-expression
    (list 'cons car-expression cdr-expression)
  `(structure 'cons (car ,car-expression) (cdr ,cdr-expression)))

;;;

(define-macro-rule value-expression
    (list 'value (list (<- variable (base::variable-name!)))
          (<- expression (base::expression! context)))
  `(and (<- ,variable) ,expression))
