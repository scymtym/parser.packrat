(cl:in-package #:parser.packrat.grammar.base)

;;; Grammar

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar))
(parser.packrat:in-grammar meta-grammar)

;;; Rules

(parser.packrat:defrule expression ()
    (or (predicate-expression)
        (anything-expression)

        (constant-expression)

        (set-expression)
        (push-expression)

        (not-expression)
        (and-expression)
        (or-expression)
        (compose-expression)

        (transform-expression)

        (rule-invocation-expression)
        (next-rule-invocation-expression)))

;;; Predicate and anything

(parser.packrat:defrule function-name-or-partial-application ()
    (or (:guard name symbolp)
        (list* (:guard name symbolp) arguments))
  (list* name arguments))

(parser.packrat:defrule predicate-expression ()
    (list* :guard
           (* (:<- sub-expression (expression)) 0 1)
           (:<- predicate (function-name-or-partial-application)))
  (bp:node* (:predicate :predicate predicate)
    (1 :sub-expression sub-expression)))

(parser.packrat:defrule anything-expression ()
    (or ':any 'any)
  (bp:node* (:anything)))

;;; Constant

(parser.packrat:defrule constant-expression () ; TODO either constant or terminal
    (or (list 'quote value)
        (:<- value (:guard (typep '(not (or cons (and symbol (not keyword))))))))
  (bp:node* (:terminal :value value)))

;;; Variables

(parser.packrat:defrule variable-name ()
    (:guard (typep '(and symbol (not (or keyword null))))))

(parser.packrat:defrule set-expression ()
    (or (:<- variable (variable-name))
        (list (or :<- '<-) (:<- variable (variable-name))
              (* (:<- sub-expression (expression)) 0 1)))
  (bp:node* (:set :variable variable)
    (1 :sub-expression (or sub-expression (make-instance 'anything-expression)))))

(parser.packrat:defrule push-expression ()
    (list (or :<<- '<<-) (:<- variable (variable-name))
          (* (:<- sub-expression (expression)) 0 1))
  (bp:node* (:push :variable variable)
    (1 :sub-expression (or sub-expression (make-instance 'anything-expression)))))

;;; Combinators

(parser.packrat:defrule not-expression ()
    (list 'not (:<- sub-expression (expression)))
  (bp:node* (:not)
    (1 :sub-expression sub-expression)))

(macrolet
    ((define-combinator-rule (combinator)
       (let ((rule-name (symbolicate combinator '#:-expression))
             (kind      (make-keyword combinator)))
         `(parser.packrat:defrule ,rule-name ()
              (list ',combinator (* (:<<- sub-expressions (expression))))
            (bp:node* (,kind)
              (* :sub-expression (nreverse sub-expressions)))))))
  (define-combinator-rule and)
  (define-combinator-rule or)
  (define-combinator-rule compose))

;;;

(parser.packrat:defrule transform-expression ()
    (list* :transform (:<- sub-expression (expression)) code)
  (bp:node* (:transform)
    (1 :sub-expression sub-expression)
    (1 :code           code)))

;;; Rule invocation

(parser.packrat:defrule rule-invocation-expression ()
    (list (or (:<- rule-name (:guard symbolp))
              (list (:<- rule-name    (:guard symbolp))
                    (:<- grammar-name (:guard symbolp))))
          (* (:<<- arguments (expression #+todo :argument))))
  (bp:node* (:rule-invocation :grammar grammar-name
                              :rule    rule-name)
    (* :sub-expression (nreverse arguments))))

(parser.packrat:defrule next-rule-invocation-expression (#+todo context)
    (list (or :next-rule 'next-rule) (* (:<<- arguments (expression #+todo :argument))))
  (bp:node* (:next-rule-invocation)
    (* :sub-expression (nreverse arguments))))
