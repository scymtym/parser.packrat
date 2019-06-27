(cl:in-package #:parser.packrat.compiler)

;;; Expression compilation protocol

(defgeneric prepare-expression (grammar environment expression)
  (:documentation
   "A \"source transform\" that prepares EXPRESSION for compilation."))

(defgeneric compile-expression (grammar environment expression
                                success-cont failure-cont)
  (:documentation
   "Return code implementing the semantics of EXPRESSION.

    GRAMMAR is the grammar in the context of which the code will be
    evaluated.

    ENVIRONMENT describes the lexical and static aspects of the
    processed input and bound variables.

    EXPRESSION is the abstract syntax node that should be compiled.

    SUCCESS-CONT is a function that should be called with an
    environment representing the state after EXPRESSION has succeeded
    as the sole argument to obtain the code that should be executed if
    EXPRESSION succeeds.

    FAILURE-CONT is a function that should be called with an
    environment representing the state after EXPRESSION has succeeded
    as the sole argument to obtain the code that should be executed if
    EXPRESSION succeeds."))

;;; Invocation compilation protocol

(defgeneric validate-invocation (grammar environment invocation)
  (:documentation
   "Return true if INVOCATION is valid for GRAMMAR and environment.

    INVOCATION is a rule invocation expression.

    Methods must return true iff passing the state variables in
    ENVIRONMENT to the rule invoked by INVOCATION is consistent with
    the calling convention of GRAMMAR (or the grammar containing the
    invoked rule)."))

;;; Rule compilation protocol

(defgeneric compile-rule (grammar name parameters expression &key environment)
  (:documentation
   "Return a lambda expression implementing the behavior of EXPRESSION.

    The body of the lambda expression is obtained by (via
    `compile-rule-using-environment') applying `compile-expression' to
    GRAMMAR, ENVIRONMENT and EXPRESSION.

    GRAMMAR specifies the calling convention used by the returned
    lambda expression and may influence the compilation of EXPRESSION.

    PARAMETERS is a list of names of parameters the resulting rule
    function should accept.

    EXPRESSION describes the behavior of the rule.

    ENVIRONMENT, if supplied, may specify the calling convention and
    is the environment for compiling EXPRESSION in. If not supplied,
    the result of calling `parser.packrat.grammar:default-environment'
    on GRAMMAR and EXPRESSION is used by default."))

(defgeneric compile-rule-using-environment
    (grammar name parameters environment expression)
  (:documentation
   "Return a lambda expression implementing the behavior of EXPRESSION.

    GRAMMAR specifies the calling convention used by the returned
    lambda expression and may influence the compilation of EXPRESSION.

    PARAMETERS is a list of names of parameters the resulting rule
    function should accept.

    ENVIRONMENT may specify the calling convention and is the
    environment for compiling EXPRESSION in.

    EXPRESSION describes the behavior of the rule."))

(defgeneric make-rule-lambda (grammar environment parameters body)
  (:documentation
   "Return a lambda expression for ENVIRONMENT, PARAMETERS and BODY."))

;;; Default behavior

(defvar *rule-name*) ; TODO hack

(defvar *fatal-cont*) ; TODO hack

(defconstant +context-var+ 'context)

(defmethod compile-rule-using-environment ((grammar     t)
                                           (name        t)
                                           (parameters  list)
                                           (environment t)
                                           (expression  t))
  ;; TODO the variable write stuff is too simplistic
  (let+ ((environment        (apply #'env:environment-binding environment
                                    (mappend (lambda (parameter)
                                               (list parameter (cons parameter :parameter)))
                                             parameters)))
         (expression         (prepare-expression grammar environment expression))

         ((&flet references-with-mode (mode) ; TODO turn this into a function in exp module
            (exp:variable-references
             expression :filter (lambda (node)
                                  (and (not (env:lookup (exp:variable node) environment)) ; TODO hack
                                       (eq (exp:mode node) mode))))))
         (writes             (references-with-mode :write))
         (assigned-variables (remove-duplicates writes :key #'exp:variable))
         (assigned-names     (mapcar #'exp:variable assigned-variables))

         ((&flet make-return-cont (kind)
            (lambda (environment &optional message)
              (let ((value (ecase kind
                             ((t)       ; TODO hack
                              (env::value* environment))
                             ((nil) message)
                             (:fatal message))))
                `(values ,kind
                         ,@(env:position-variables environment)
                         ,@(when value `(,value)))))))
         (form (maybe-let assigned-names
                 (let ((*rule-name*  name)
                       (*fatal-cont* (make-return-cont :fatal)))
                   (compile-expression
                    grammar environment expression
                    (make-return-cont t) (make-return-cont nil))))))
    (make-rule-lambda grammar environment parameters (list form))))

(defmethod make-rule-lambda ((grammar     t)
                             (environment t)
                             (parameters  t)
                             (body        t))
  `(lambda (,+context-var+ ,@(env:state-variables environment) ,@parameters)
     ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
     (declare (optimize (speed 1) (debug 1) (safety 1)))
     (declare (ignorable ,+context-var+))
     ,@body))
