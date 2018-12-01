(cl:in-package #:parser.packrat.grammar.sequence)

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar)
  (:use base::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

;;;

(parser.packrat:defrule base::expression ()
    (or (base:anything-expression)
        (base:constant-expression :default)

        (base:set-expression)
        (base:push-expression)

        (repetition-expression)
        (sequence-expression)

        (?-expression)
        (+-expression)

        (bounds-expression)))

(parser.packrat:defrule repetition-expression ()
    (list '* (:<- sub-expression (base::expression))
          (? (:seq (:<- min (or (base::constant-expression :value)
                                (base::expression)))
                   (? (:<- max (or (base::constant-expression :value)
                                   (base::expression)))))))
  (bp:node* (:repetition)
    (1    :sub-expression  sub-expression)
    (bp:? :min-repetitions min)
    (bp:? :max-repetitions max)))

(parser.packrat:defrule sequence-expression ()
    (list (or :seq 'seq) (* (:<<- element-expressions (base::expression))))
  (bp:node* (:sequence)
    (* :sub-expression (nreverse element-expressions))))

;;; Syntactic sugar

(defmacro define-macro-rule (name expression expansion) ; TODO there is also a define-macro rule in sexp/concrete-syntax
  `(parser.packrat:defrule ,name ()
       (:compose (:transform ,expression ,expansion)
                 (base::expression))))

(define-macro-rule ?-expression
    (list '? expression)
  `(* ,expression 0 1))

(define-macro-rule +-expression
    (list '+ expression)
  `(* ,expression 1))

(define-macro-rule bounds-expression
    (list* 'bounds (list (:guard start symbolp) (:guard end symbolp))
           expressions)
  `(:seq (base::<- ,start :position) ,@expressions (base::<- ,end :position)))
