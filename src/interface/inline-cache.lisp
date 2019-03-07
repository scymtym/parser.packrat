(cl:in-package #:parser.packrat)

(defclass inline-cache (c2mop:funcallable-standard-object)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance inline-cache) &key)
  (c2mop:set-funcallable-instance-function
   instance (curry #'initial-function instance)))

(defun initial-function (instance grammar expression input)
  ;; Compile EXPRESSION into a function and replace the instance
  ;; function (which is currently this function).
  (c2mop:set-funcallable-instance-function
   instance (make-inline-cache-function grammar expression))
  ;; Call INSTANCE again which will use the new instance function.
  (funcall instance grammar expression input))

;;; Utilities

(defun make-inline-rule-lambda (grammar expression)
  (let* ((environment       (grammar:default-environment
                             grammar expression))
         (state-variables   (env:state-variables environment))
         (free-variables    (map 'list #'exp:variable
                                 (exp:variable-references
                                  expression :filter (of-type 'base:variable-reference-expression)))))
    (values (parser.packrat.compiler:compile-rule
             grammar free-variables expression)
            ; state-variables
            free-variables)))

(defun simple-invocation? (expression)
  (and (typep expression 'base:rule-invocation-expression)
       (every (of-type '(or base:variable-reference-expression
                            base:constant-expression))
              (base:arguments expression))))

(defun make-inline-cache-form (grammar expression ; free-variables
                               )
  (let* ((expression/parsed (grammar:parse-expression grammar expression))
         (invocation?       (simple-invocation? expression/parsed)))
    (if invocation?
        (make-inline-cache-form/invocation grammar expression/parsed)
        (let+ (((&values rule-lambda    ; free-variables
                         )
                (make-inline-rule-lambda grammar expression/parsed)))
          `(lambda (grammar expression input)
             (if (and (eq                      grammar    ,grammar)
                      (expressions-compatible? expression ',expression))
                 ;; Fast path
                 (grammar:parse ,grammar ,rule-lambda input)
                 ;; Slow path
                 (grammar:parse grammar expression input)))))))

(defun make-bindings (arguments-var variables)
  (loop :for tail = `(tail ,arguments-var) :then `(tail (rest tail))
        :for variable :in variables
        :collect tail
        :collect `(,variable (first tail))
        :collect `(,variable (if (and (consp ,variable) (eq (first ,variable) 'quote))
                                 (second ,variable)
                                 ,variable))))

(defun make-inline-cache-form/invocation (grammar expression)
  (let+ ((rule           (parser.packrat.grammar.base:rule expression))
         (count          (length (parser.packrat.grammar.base:arguments
                                  expression)))
         (names          (map-into (make-list count) #'gensym))
         (new-expression (make-instance 'parser.packrat.grammar.base:rule-invocation-expression
                                        :rule      rule
                                        :arguments (map 'list (lambda (variable)
                                                                (make-instance 'parser.packrat.grammar.base:variable-reference-expression
                                                                               :variable variable))
                                                        names)))
         ((&values (&ign rule-lambda-list &rest rule-body) ; free-variables
                   )
          (make-inline-rule-lambda grammar new-expression)))
    `(lambda (grammar expression input)
       (if (and (eq                        grammar    ,grammar)
                (expression-is-invocation? expression ',rule))
           ;; Fast path
           (let* (,@(when names
                      `((arguments (rest expression))))
                  ,@(make-bindings 'arguments names))
             (grammar:parse ,grammar (lambda ,(remove-if (rcurry #'member names) rule-lambda-list) ,@rule-body) input))
           ;; Slow path
           (grammar:parse grammar expression input)))))

(defun make-inline-cache-function (grammar expression)
  (compile nil (make-inline-cache-form grammar expression)))

;;; Runtime

(declaim (inline expressions-compatible?
                 expression-is-invocation?))

(defun expressions-compatible? (new-expression old-expression)
  (equal old-expression new-expression))

(defun expression-is-invocation? (new-expression rule)
  (and (consp new-expression)
       (eq (first new-expression) rule)))
