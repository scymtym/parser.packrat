;;;; package.lisp --- Meta-grammar rules for the base grammar module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.base)

;;; Grammar

(parser.packrat:defgrammar meta-grammar
  (:class   parser.packrat.grammar.sexp:sexp-grammar)
  (:cached? nil))
(parser.packrat:in-grammar meta-grammar)

;;; Rules

(parser.packrat:defrule expression (context)
    (or (predicate-expression context)
        (anything-expression context)

        (constant-expression context)

        (set-expression context)
        (push-expression context)

        (must-expression context)

        (not-expression context)
        (and-expression context)
        (or-expression context)

        (compose-expression context)
        (transform-expression context)

        ;; These two must be last; next-rule must be before generic
        ;; rule-invocation.
        (next-rule-invocation-expression context)
        (rule-invocation-expression context)))

(parser.packrat:defrule expression! (context)
    (:must (expression context) "must be an expression"))

;;; Predicate and anything

(parser.packrat:defrule function-name-or-partial-application ()
    (or (guard name symbolp)
        (list* (guard name symbolp) arguments))
  (list* name arguments))

(parser.packrat:defrule function-name-or-partial-application! ()
    (:must (function-name-or-partial-application) "must be a function name or partial application"))

(parser.packrat:defrule predicate-expression (context)
    (list (or :guard 'guard)
          (or (:seq (<- sub-expression (expression! context))
                    (<- predicate (function-name-or-partial-application!)))
              (<- predicate (function-name-or-partial-application!))))
  (bp:node* (:predicate :predicate predicate)
    (1 :sub-expression (or sub-expression (bp:node* (:anything))))))

(parser.packrat:defrule anything-expression (context)
    (or ':any 'any)
  (bp:node* (:anything)))

;;; Constant

(parser.packrat:defrule constant-expression (context)
    (or (list 'quote value)
        (guard value (typep '(not (or cons (and symbol (not keyword)))))))
  (ecase context
    (:value   (bp:node* (:constant :value value)))
    (:default (bp:node* (:terminal :value value)))))

;;; Variables

(parser.packrat:defrule variable-name ()
    (guard (typep '(and symbol (not (or keyword null))))))

(parser.packrat:defrule variable-name! ()
    (:must (variable-name) "must be a variable name"))

(parser.packrat:defrule implicit-list (action)
    (list (* (<<- variables (variable-name!)) 1))
  (let ((expressions
          (loop :for variable :in (nreverse variables)
                :collect (bp:node* (action :variable variable)
                           (1 :sub-expression (bp:node* (:anything)))))))
    (bp:node* (:as-list)
      (1 :sub-expression (bp:node* (:sequence)
                           (* :sub-expression expressions))))))

(parser.packrat:defrule set-expression/simple (context)
    (or (<- variable1 (variable-name))
        (list (or :<- '<-)
              (and (not (list* :any)) (<- variable2 (variable-name!)))
              (* (<- sub-expression (expression! context)) 0 1)))
  (if (and variable1 (eq context :value))
      (bp:node* (:variable-reference :variable variable1))
      (bp:node* (:set :variable (or variable1 variable2))
        (1 :sub-expression (or sub-expression (bp:node* (:anything)))))))

(parser.packrat:defrule set-expression/compose (context)
    (list '<-
          (<- set (or (implicit-list :set) (expression! context)))
          (* (<<- sub-expressions (expression! context))))
  (when (eq context :value)
    (:fail))
  (if sub-expressions
      (bp:node* (:compose)
        (* :sub-expression (append sub-expressions (list set))))
      set))

(parser.packrat:defrule set-expression (context)
    (or (set-expression/simple  context)
        (set-expression/compose context)))

(parser.packrat:defrule push-expression/simple (context)
    (list (or :<<- '<<-)
          (and (not (list* :any)) (<- variable (variable-name!)))
          (* (<- sub-expression (expression! context)) 0 1))
  (bp:node* (:push :variable variable)
    (1 :sub-expression (or sub-expression (bp:node* (:anything))))))

(parser.packrat:defrule push-expression/compose (context)
    (list '<<-
          (<- push (or (implicit-list :push) (expression! context)))
          (* (<<- sub-expressions (expression! context))))
  (when (eq context :value)
    (:fail))
  (if sub-expressions
      (bp:node* (:compose)
        (* :sub-expression (append sub-expressions (list push))))
      push))

(parser.packrat:defrule push-expression (context)
    (or (push-expression/simple  context)
        (push-expression/compose context)))

;;; Must

(parser.packrat:defrule must-expression (context)
    (list (or :must 'must)
          (<- sub-expression (expression! context))
          (* (guard message stringp) 0 1))
  (bp:node* (:must :message message)
    (1 :sub-expression sub-expression)))

;;; Logical connectives

(parser.packrat:defrule not-expression (context)
    (list 'not (<- sub-expression (expression! context)))
  (bp:node* (:not)
    (1 :sub-expression sub-expression)))

(macrolet
    ((define-combinator-rule (combinator)
       (let ((rule-name (symbolicate combinator '#:-expression))
             (kind      (make-keyword combinator)))
         `(parser.packrat:defrule ,rule-name (context)
              (list ',combinator (* (<<- sub-expressions (expression! context))))
            (bp:node* (,kind)
              (* :sub-expression (nreverse sub-expressions)))))))
  (define-combinator-rule and)
  (define-combinator-rule or)
  (define-combinator-rule :compose)) ; TODO

;;;

(parser.packrat:defrule transform-expression (context)
    (list* :transform
           (<- sub-expression (expression! context)) ; TODO this is not optional. make that work with :must
           code)
  (bp:node* (:transform :code code)
    (1 :sub-expression sub-expression)))

;;; Rule invocation

(parser.packrat:defrule rule-name ()
    (guard symbolp))

(parser.packrat:defrule rule-name! ()
    (:must (rule-name) "must be a rule name"))

(parser.packrat:defrule grammar-name ()
    (guard symbolp))

(parser.packrat:defrule grammar-name! ()
    (:must (grammar-name) "must be a grammar name"))

(parser.packrat:defrule rule-invocation-expression (context)
    (list (or (list (<- rule-name    (rule-name!))
                    (<- grammar-name (grammar-name!)))
              (<- rule-name (rule-name)))
          (* (<<- arguments (expression! :value))))
  (bp:node* (:rule-invocation :grammar grammar-name
                              :rule    rule-name)
    (* :sub-expression (nreverse arguments))))

(parser.packrat:defrule next-rule-invocation-expression (context)
    (list (or :next-rule 'next-rule) (* (<<- arguments (expression! :value))))
  (bp:node* (:next-rule-invocation)
    (* :sub-expression (nreverse arguments))))
