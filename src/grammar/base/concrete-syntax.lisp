;;;; package.lisp --- Meta-grammar rules for the base grammar module.
;;;;
;;;; Copyright (C) 2017-2022 Jan Moringen
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

        (position-expression context) ; must be early

        (constant-expression context)

        (set-expression context)
        (push-expression context)
        (append-expression context)

        (must-expression context)

        (not-expression context)
        (and-expression context)
        (or-expression context)

        (compose-expression context)
        (transform-expression context)

        ;; Syntactic sugar
        (value-expression context)

        ;; These two must be last; next-rule must be before generic
        ;; rule-invocation.
        (next-rule-invocation-expression context)
        (rule-invocation-expression context)))

(parser.packrat:defrule expression! (context)
    (must (expression context) "must be an expression"))

;;; Predicate and anything

(parser.packrat:defrule function-name-or-partial-application ()
    (or (guard name symbolp)
        (list* (guard name symbolp) arguments))
  (list* name arguments))

(parser.packrat:defrule function-name-or-partial-application! ()
    (must (function-name-or-partial-application)
          "must be a function name or partial application"))

(parser.packrat:defrule predicate-expression (context)
    (value (source)
      (list (or :guard 'guard)
            (or (parser.packrat.grammar.sequence:seq
                 (<- sub-expression (expression! context))
                 (<- predicate (function-name-or-partial-application!)))
                (<- predicate (function-name-or-partial-application!)))))
  (bp:node* (:predicate :predicate predicate :source source)
    (1 :sub-expression (or sub-expression (bp:node* (:anything))))))

(parser.packrat:defrule anything-expression (context)
    (or ':any 'any)
  (bp:node* (:anything)))

;;; Constant

(parser.packrat:defrule constant-expression (context)
    (value (source)
      (or (list 'quote value)
          (guard value (typep '(not (or cons (and symbol (not keyword))))))))
  (ecase context
    (:value   (bp:node* (:constant :value value :source source)))
    (:default (bp:node* (:terminal :value value :source source)))))

;;; Variables

(parser.packrat:defrule variable-name ()
    (guard (typep '(and symbol (not (or keyword null))))))

(parser.packrat:defrule variable-name! ()
    (must (variable-name) "must be a variable name"))

(parser.packrat:defrule implicit-list (action)
    (value (source)
      (list (* (<<- variables (variable-name!)) 1)))
  (let ((expressions
          (loop :for variable :in (nreverse variables)
                :collect (bp:node* (action :variable variable)
                           (1 :sub-expression (bp:node* (:anything)))))))
    (bp:node* (:as-list :source source)
      (1 :sub-expression (bp:node* (:sequence)
                           (* :sub-expression expressions))))))

(parser.packrat:defrule set-expression/simple (context)
    (value (source)
      (or (<- variable1 (variable-name))
          (list (or :<- '<-)
                (and (not (list* :any)) (<- variable2 (variable-name!)))
                (* (<- sub-expression (expression! context)) 0 1))))
  (if (and variable1 (eq context :value))
      (bp:node* (:variable-reference :variable variable1 :source source))
      (bp:node* (:set :variable (or variable1 variable2) :source source)
        (1 :sub-expression (or sub-expression (bp:node* (:anything)))))))

(parser.packrat:defrule set-expression/compose (context)
    (value (source)
      (list '<-
            (<- set (or (implicit-list :set) (expression! context)))
            (* (<<- sub-expressions (expression! context)))))
  (when (eq context :value)
    (:fail))
  (if sub-expressions
      (bp:node* (:compose :source source)
        (* :sub-expression (append sub-expressions (list set))))
      set))

(parser.packrat:defrule set-expression (context)
    (or (set-expression/simple  context)
        (set-expression/compose context)))

(parser.packrat:defrule push-expression/simple (context)
    (value (source)
      (list (or :<<- '<<-)
            (and (not (list* :any)) (<- variable (variable-name!)))
            (* (<- sub-expression (expression! context)) 0 1)))
  (bp:node* (:push :variable variable :source source)
    (1 :sub-expression (or sub-expression (bp:node* (:anything))))))

(parser.packrat:defrule push-expression/compose (context)
    (value (source)
      (list '<<-
            (<- push (or (implicit-list :push) (expression! context)))
            (* (<<- sub-expressions (expression! context)))))
  (when (eq context :value)
    (:fail))
  (if sub-expressions
      (bp:node* (:compose :source source)
        (* :sub-expression (append sub-expressions (list push))))
      push))

(parser.packrat:defrule push-expression (context)
    (or (push-expression/simple  context)
        (push-expression/compose context)))

(parser.packrat:defrule append-expression/simple (context)
    (value (source)
      (list '<>-
            (and (not (list* :any)) (<- variable (variable-name!)))
            (* (<- sub-expression (expression! context)) 0 1)))
  (bp:node* (:append :variable variable :source source)
    (1 :sub-expression (or sub-expression (bp:node* (:anything))))))

(parser.packrat:defrule append-expression/compose (context)
    (value (source)
      (list '<>-
            (<- append (or (implicit-list :append) (expression! context)))
            (* (<<- sub-expressions (expression! context)))))
  (when (eq context :value)
    (:fail))
  (if sub-expressions
      (bp:node* (:compose :source source)
        (* :sub-expression (append sub-expressions (list append))))
      append))

(parser.packrat:defrule append-expression (context)
  (or (append-expression/simple  context)
      (append-expression/compose context)))

;;; Must

(parser.packrat:defrule must-expression (context)
    (value (source)
      (list (or :must 'must)
            (<- sub-expression (expression! context))
            (* (guard message stringp) 0 1)))
  (bp:node* (:must :message message :source source)
    (1 :sub-expression sub-expression)))

;;; Logical connectives

(macrolet
    ((define-combinator-rule (combinator arity)
       (let ((rule-name (symbolicate combinator '#:-expression))
             (kind      (make-keyword combinator)))
         `(parser.packrat:defrule ,rule-name (context)
              (value (source)
                (list ',combinator
                      ,(ecase arity
                         (1 `(<- sub-expression (expression! context)))
                         (* `(* (<<- sub-expressions (expression! context)))))))
            (bp:node* (,kind :source source)
              ,(ecase arity
                 (1 `(1 :sub-expression sub-expression))
                 (* `(* :sub-expression (nreverse sub-expressions)))))))))
  (define-combinator-rule not      1)
  (define-combinator-rule and      *)
  (define-combinator-rule or       *)
  (define-combinator-rule :compose *)  ; TODO
  )

;;;

(parser.packrat:defrule transform-expression (context)
    (value (source)
      (list* (or :transform 'transform) ; TODO export
             (<- sub-expression (expression! context)) ; TODO this is not optional. make that work with :must
             code))
  (bp:node* (:transform :code code :source source)
    (1 :sub-expression sub-expression)))

;;; Rule invocation

(parser.packrat:defrule rule-name ()
    (guard symbolp))

(parser.packrat:defrule rule-name! ()
    (must (rule-name) "must be a rule name"))

(parser.packrat:defrule grammar-name ()
    (guard symbolp))

(parser.packrat:defrule grammar-name! ()
    (must (grammar-name) "must be a grammar name"))

(parser.packrat:defrule rule-invocation-expression (context)
    (value (source)
      (list (or (list    (<- rule-name         (rule-name!))
                         (<- grammar-name      (grammar-name!))
                      (* (<- switch-to-grammar (grammar-name!)) 0 1))
                (<- rule-name (rule-name)))
            (* (<<- arguments (expression! :value)))))
  (bp:node* (:rule-invocation :grammar           grammar-name
                              :rule              rule-name
                              :switch-to-grammar switch-to-grammar
                              :source            source)
    (* :sub-expression (nreverse arguments))))

(parser.packrat:defrule next-rule-invocation-expression (context)
    (value (source)
      (list (or :next-rule 'next-rule)
            (* (<<- arguments (expression! :value)))))
  (bp:node* (:next-rule-invocation :source source)
    (* :sub-expression (nreverse arguments))))

;;; Position

(parser.packrat:defrule position-expression (context)
    'position
  (bp:node* (:position)))

;;; Syntactic sugar

(defmacro define-macro-rule (name expression expansion)
  `(parser.packrat:defrule ,name (context)
     (:compose (:transform ,expression ,expansion)
               (expression! context))))

(define-macro-rule value-expression
    (list 'value (list object) expression)
  `(and ,object ,expression))
