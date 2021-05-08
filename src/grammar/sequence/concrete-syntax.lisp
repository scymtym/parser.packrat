;;;; concrete-syntax.lisp --- Meta-grammar rules for the sequence grammar module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sequence)

(parser.packrat:defgrammar meta-grammar
  (:class   parser.packrat.grammar.sexp:sexp-grammar)
  (:cached? nil)
  (:use     base::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

;;;

(parser.packrat:defrule base::expression (context)
    (or (repetition-expression context)
        (sequence-expression context)

        (?-expression context)
        (+-expression context)

        (bounds-expression context)))

(parser.packrat:defrule repetition-expression (context)
    (list '* (<- sub-expression (base::expression context))
          (? (seq (<- min (base::expression :value))
                  (? (<- max (base::expression :value))))))
  (bp:node* (:repetition)
    (1    :sub-expression  sub-expression)
    (bp:? :min-repetitions min)
    (bp:? :max-repetitions max)))

(parser.packrat:defrule sequence-expression (context)
    (base::value (source)
      (list (or :seq 'seq) ; TODO remove keyword version
            (* (<<- element-expressions (base::expression context)))))
  (bp:node* (:sequence :source source)
    (* :sub-expression (nreverse element-expressions))))

;;; Syntactic sugar

(defmacro define-macro-rule (name expression expansion) ; TODO there is also a define-macro rule in sexp/concrete-syntax
  `(parser.packrat:defrule ,name (context)
     (:compose (:transform ,expression ,expansion)
               (base::expression context))))

(define-macro-rule ?-expression
    (list '? expression)
  `(* ,expression 0 1))

(define-macro-rule +-expression
    (list '+ expression)
  `(* ,expression 1))

(define-macro-rule bounds-expression
    (list* 'bounds (list (must (guard start symbolp) "must be a symbol")
                         (must (guard end symbolp) "must be a symbol"))
           expressions)
  (let ((prefix (butlast expressions))
        (last   (lastcar expressions))
        (result (gensym)))
    `(seq (<- ,start position)
          ,@prefix
          (<- ,result ,last)
          (:transform (<- ,end position) ,result))))
