(cl:in-package #:parser.packrat.grammar.base)

;;; Grammar

(parser.packrat:defgrammar meta-grammar
  (:class parser.packrat.grammar.sexp:sexp-grammar))
(parser.packrat:in-grammar meta-grammar)

;;; Rules

(parser.packrat:defrule expression ()
    (or (predicate-expression)
        (anything-expression)

        (constant-expression :default)

        (set-expression)
        (push-expression)

        (not-expression)
        (and-expression)
        (or-expression)

        (compose-expression)
        (transform-expression)

        ;; Must be last
        (rule-invocation-expression)
        (next-rule-invocation-expression)))

;;; Predicate and anything

(parser.packrat:defrule function-name-or-partial-application ()
    (or (:guard name symbolp)
        (list* (:guard name symbolp) arguments))
  (list* name arguments))

(parser.packrat:defrule predicate-expression ()
    (list :guard
          (or (:seq (:<- sub-expression (expression))
                    (:<- predicate (function-name-or-partial-application)))
              (:<- predicate (function-name-or-partial-application))))
  (bp:node* (:predicate :predicate predicate)
    (1 :sub-expression (or sub-expression (bp:node* (:anything))))))

(parser.packrat:defrule anything-expression ()
    (or ':any 'any)
  (bp:node* (:anything)))

;;; Constant

(parser.packrat:defrule constant-expression (context) ; TODO either constant or terminal
    (or (list 'quote value)
        (:guard value (typep '(not (or cons (and symbol (not keyword)))))))
  (ecase context
    (:value   (bp:node* (:constant :value value)))
    (:default (bp:node* (:terminal :value value)))))

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

;;; Logical connectives

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
  (define-combinator-rule :compose)) ; TODO

;;;

(parser.packrat:defrule transform-expression ()
    (list* :transform (:<- sub-expression (expression)) code)
  (bp:node* (:transform :code code)
    (1 :sub-expression sub-expression)))

;;; Rule invocation

(parser.packrat:defrule rule-invocation-expression ()
    (list (or (:guard rule-name symbolp)
              (list (:guard rule-name    symbolp)
                    (:guard grammar-name symbolp)))
          (* (:<<- arguments (or (constant-expression :value) (expression #+todo :argument)))))
  (bp:node* (:rule-invocation :grammar grammar-name
                              :rule    rule-name)
    (* :sub-expression (nreverse arguments))))

(parser.packrat:defrule next-rule-invocation-expression (#+todo context)
    (list (or :next-rule 'next-rule) (* (:<<- arguments (expression #+todo :argument))))
  (bp:node* (:next-rule-invocation)
    (* :sub-expression (nreverse arguments))))
