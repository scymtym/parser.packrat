(cl:in-package #:parser.packrat.grammar.base)

(defmethod compile-test ((grammar      base-grammar)
                         (environment  t)
                         (expression   t)
                         (predicate    t)
                         (value        t)
                         (arguments    t)
                         (success-cont function)
                         (failure-cont function))
  `(if (,predicate ,value ,@arguments)
       ,(funcall success-cont environment)
       ,(funcall failure-cont environment)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  env:value-environment)
                               (expression   predicate-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o sub-expression predicate) expression)
         ((predicate &rest arguments) (ensure-list predicate))) ; TODO should be done earlier
    (compile-expression
     grammar environment sub-expression
     (lambda (new-environment)
       (compile-test grammar new-environment expression
                     predicate (env:value new-environment) arguments
                     success-cont failure-cont))
     failure-cont)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  env:value-environment)
                               (expression   anything-expression)
                               (success-cont function)
                               (failure-cont function))
  (funcall success-cont environment))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  env:value-environment)
                               (expression   terminal-expression)
                               (success-cont function)
                               (failure-cont function))
  (let* ((value     (env:value environment))
         (expected  (exp:value expression))
         (predicate (typecase expected
                      (string 'equal)
                      (t      'eql))))
    (compile-test grammar environment expression
                  predicate value (list `',expected)
                  success-cont failure-cont)))

;;; Combinators

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t) ; TODO value-environment ?
                               (expression   not-expression)
                               (success-cont function)
                               (failure-cont function))
  (compile-expression
   grammar environment (sub-expression expression)
   (lambda (new-environment)
     (declare (ignore new-environment))
     (funcall failure-cont environment))
   (lambda (new-environment)
     (declare (ignore new-environment))
     (funcall success-cont environment))))

(macrolet ((define-pre-condition-check (expression-class)
             `(defmethod compile-expression :before  ((grammar      t)
                                                      (environment  t)
                                                      (expression   ,expression-class)
                                                      (success-cont function)
                                                      (failure-cont function))
                (unless (> (length (exp:sub-expressions expression)) 1)
                  (error "~@<Expression ~A must have at least two ~
                          sub-expressions.~@:>"
                         expression)))))
  (define-pre-condition-check or-expression)
  (define-pre-condition-check and-expression))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   or-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ ((expressions (sub-expressions expression))
         (names       (map-into (make-list (length expressions)) #'gensym))
         ((&flet assignments (expression)
            (let+ (((&flet references-with-mode (mode) ; TODO duplicated in compile-rule-using-environment
                      (exp:variable-references
                       expression :filter (lambda (node)
                                            (and (not (env:lookup (exp:variable node) environment)) ; TODO hack
                                                 (eq (exp:mode node) mode))))))
                   (writes             (references-with-mode :write))
                   (assigned-variables (remove-duplicates writes :key #'exp:variable))
                   (assigned-names     (mapcar #'exp:variable assigned-variables))
                   (temporary-names    (mapcar (compose #'gensym #'string) assigned-names)))
              (when assigned-names
                (values
                 (apply #'env:environment-binding environment
                        (mappend (lambda (variable-name temporary-name)
                                   (let ((kind (cdr (env:lookup variable-name environment))))
                                     (unless (eq kind :parameter)
                                       ;; (cons temporary-name nil) is required for e.g.
                                       ;; (or (:guard name symbolp)
                                       ;;     (:transform thing (error ...)))
                                       ;; but (cons temporary-name t) is required for other cases, e.g.
                                       ;; TODO add an example
                                       (list variable-name (cons temporary-name kind)))))
                                 assigned-names temporary-names))
                 assigned-names
                 temporary-names)))))
         ((&flet alternative (expression name next-name) ; TODO pass failing environment to next alternative?
            (let+ (((&values assignment-environment assigned-names temporary-names)
                    (assignments expression))
                   (environment (or assignment-environment environment)))
              `(,name ()
                  ,(maybe-let
                    (mapcar #'list temporary-names assigned-names)
                    (compile-expression
                     grammar environment expression
                     (lambda (success-environment)
                       (maybe-progn
                        (when assigned-names
                          `(setf ,@(mappend #'list assigned-names temporary-names)))
                        (let ((continue-environment
                                (apply #'env:environment-binding
                                       success-environment
                                       (mappend (lambda (name)
                                                  (list name nil)) ; TODO use old value, not nil
                                                assigned-names))))
                         (funcall success-cont continue-environment))))
                     (lambda (failure-environment)
                       (declare (ignore failure-environment))
                       (if next-name
                           `(,next-name)
                           (funcall failure-cont environment))))))))))

    `(labels ,(map 'list #'alternative
                   expressions names (append (rest names) '(nil)))
       (,(first names)))))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   and-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&labels+ sub ((&optional first &rest rest) environment)
            (compile-expression
             grammar environment first
             (lambda (new-environment)
               (if rest
                   (sub rest (env:environment-at environment (env:state-variables/plist environment)
                                                 :parent new-environment))
                   (funcall success-cont new-environment)))
             failure-cont))))
    (sub (sub-expressions expression) environment)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   compose-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&labels+ sub ((&optional first &rest rest) environment first?)
            (let ((environment* (cond (first?
                                       environment)
                                      ((typep environment 'env:value-environment)
                                       environment)
                                      (t
                                       (assert (env::value* environment))
                                       (env:environment-at environment (list :value (env::value* environment))
                                                           :class 'env:value-environment
                                                           :state nil)))))
              (compile-expression
               grammar environment* first
               (lambda (new-environment)
                 (if rest
                     #+test-case-for-this-change (defrule test ()
                                                   (or (:guard symbolp)
                                                       (:compose (list a b)
                                                                 (:transform :any b)
                                                                 (test))))
                     (sub rest new-environment nil)
                     (funcall success-cont (add-value (env:environment-at environment (env:position-variables/plist environment)
                                                                          :parent new-environment)
                                                      (value* new-environment)))))
               failure-cont)))))
    (sub (sub-expressions expression) environment t)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   must-expression)
                               (success-cont function)
                               (failure-cont function))
  (compile-expression
   grammar environment (sub-expression expression)
   success-cont
   (lambda (new-environment)
     (funcall parser.packrat.compiler::*fatal-cont* new-environment (message expression)))))

;;; Variables

(defun value* (environment)
  (env::value* environment))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t ; env:value-environment
                                )
                               (expression   set-expression) ; TODO have two kinds of expressions for set and unify and rely on preprocessing?
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (variable exp:variable) sub-expression) expression)
         ((&optional (name variable) . already-bound?)
          (env:lookup variable environment)))
    (compile-expression
     grammar environment sub-expression
     (if already-bound?
         (lambda (new-environment)
           (compile-test grammar new-environment expression
                         'equal (value* new-environment) (list name)
                         success-cont
                         (lambda (new-environment)
                           (declare (ignore new-environment))
                           (funcall failure-cont environment))))
         (lambda (new-environment)
           (setf (env:lookup variable new-environment)
                 (cons name t)) ; TODO hack. maybe make a new environment instead?
           (maybe-progn
            `(setf ,name ,(value* new-environment))
            (funcall success-cont new-environment))))
     failure-cont)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t ; env:value-environment
                                )
                               (expression   push-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (variable exp:variable) sub-expression) expression)
         ((&optional (name variable) . already-bound?)
          (env:lookup variable environment)))
    (compile-expression
     grammar environment sub-expression
     (lambda (new-environment)
       (unless already-bound?
         (setf (env:lookup variable new-environment)
               (cons name t))) ; TODO hack. maybe make a new environment instead?)
       (maybe-progn
        `(push ,(value* new-environment) ,name)
        (funcall success-cont new-environment)))
     failure-cont)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   variable-reference-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ ((variable (exp:variable expression))
         ((&optional (name variable) . already-bound?)
          (env:lookup variable environment)))
    (unless already-bound?
      (error "~@<Unbound variable ~S.~@:>" variable))
    (funcall success-cont
             (env:environment-at environment (list :value name)
                                 :class 'env:value-environment
                                 :state '()))))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   position-expression)
                               (success-cont function)
                               (failure-cont function))
  (let ((environment (add-value environment (first (env:position-variables environment)))))
    (funcall success-cont environment)))

;;; Transform

(defclass value-environment-mixin (env:value-environment)
  ((actual-class :initarg :actual-class
                 :reader  actual-class)))

(defmethod env:environment-at ((environment value-environment-mixin)
                               (position    t)
                               &rest args &key
                               (class (actual-class environment))
                               parent
                               state)
  (declare (ignore parent state))
  (apply #'call-next-method environment position :class class
         (remove-from-plist args :class)))

;;; TODO if we only use this for rule-invocation in the end, simplify accordingly
(defun add-value (environment value) ; TODO should be in environment package
  (cond ((not (typep environment 'env:value-environment))
         (let* ((actual-class (class-of environment))
                (superclasses (list actual-class
                                    (find-class 'value-environment-mixin)))
                (class        (make-instance 'standard-class
                                             :direct-superclasses superclasses
                                             :direct-slots        '())))
           (change-class environment class
                         :actual-class actual-class
                         :value        value)))
        ((eq (env:value environment) value)
         environment)
        (t
         (env:environment-at environment (list :value value)))))

(defun remove-value (environment)
  (cond ((or (not (value* environment))
             (typep environment '(and env:value-environment
                                  (not value-environment-mixin))))
         environment)
        (t
         (let* ((actual-class (class-of environment))
                (superclasses (list actual-class
                                    (find-class 'remove-value-mixin)))
                (class        (make-instance 'standard-class
                                             :direct-superclasses superclasses
                                             :direct-slots        '())))
           (change-class environment class :actual-class actual-class)))))

(defclass remove-value-mixin ()
  ((actual-class :initarg :actual-class
                 :reader  actual-class)))

(defmethod env:value ((environment remove-value-mixin))
  nil)

(defmethod env:environment-at ((environment remove-value-mixin)
                               (position    t)
                               &rest args &key
                               (class (actual-class environment))
                               parent
                               state)
  (declare (ignore parent state))
  (apply #'call-next-method environment position :class class
         (remove-from-plist args :class)))

#+later (add-value (make-instance 'parser.packrat.grammar.sequence:vector-environment
                          :position 'pos
                          :sequence 'seq
                          :end      'endp)
           'value)

(defun %uses-fail? (expression)
  (catch 'used
    (sb-cltl2:macroexpand-all
     `(macrolet ((:fail ()
                   (throw 'used t)))
        ,expression))
    nil))

(defun %uses-position? (expression)
  (catch 'used
    (sb-cltl2:macroexpand-all
     `(macrolet ((%used () (throw 'used t)))
        (symbol-macrolet ((cl-user::position (%used)))
          ,expression)))
    nil))

;; TODO syntactically, EXPRESSION contains a sub-expression. However,
;; we do not use the value produced by that sub-expression.
(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   transform-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o code sub-expression) expression)
         (expression (apply #'maybe-progn code))
         ((&with-gensyms transform-name value-var aborted-var block-name))
         #+no ((&flet make-value-environment (parent ; &optional path-suffix
                                              )
                 (env:environment-at parent (list :value value-var)
                                     :class   'env:value-environment
                                        ; :parent2 environment
                                     :state '()
                                     #+later (when path-suffix
                                               (list :path-suffix (list path-suffix))))))
         (transform-environment (add-value (env:environment-at environment :fresh)
                                           value-var))


         ((&flet restore-bindings (environment)
            (loop :for (variable . (name . already-bound?)) :in (env:bindings environment)
                  :unless (eq variable name)
                  :collect `(,variable ,name))))
         ((&flet run-code (environment)
            (parser.packrat.compiler::maybe-symbol-macrolet
             (append (when (%uses-position? expression)
                       (list (list 'cl-user::position (first (env:position-variables environment)))))
                     (restore-bindings environment))
             expression)))


         )
    `(labels ((,transform-name ,(env:position-variables transform-environment)
                (declare (ignorable ,@(env:position-variables transform-environment))) ; TODO hack
                ,(if (%uses-fail? expression)
                     `(let ((,value-var   nil)
                            (,aborted-var nil))
                        (block ,block-name
                          (flet ((:fail ()
                                   (setf ,aborted-var t)
                                   (return-from ,block-name)))
                            (declare (inline :fail)
                                     (ignorable #':fail))
                            (setf ,value-var ,(run-code transform-environment))))
                        (if ,aborted-var
                            ,(funcall failure-cont environment)
                            ,(funcall success-cont transform-environment)))
                     ;; CODE does not cause the match to fail.
                     `(let ((,value-var ,(run-code transform-environment)))
                        ,(funcall success-cont transform-environment)))))
       ,(compile-expression
         grammar environment sub-expression
         (lambda (new-environment)
           `(,transform-name ,@(env:position-variables new-environment)))
         failure-cont))))

;;;

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   constant-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&with-gensyms value-var)))
    `(let ((,value-var ',(exp:value expression))) ; TODO can we just make a value environment with the constant value?
       ,(funcall success-cont (env:environment-at
                               environment (list :value value-var)
                               :class 'env:value-environment
                               :state '())))))

;;; Rule invocation

(defmethod validate-invocation ((grammar     t)
                                (environment t)
                                (invocation  rule-invocation-expression))
  (let+ (((&accessors-r/o (grammar-name grammar) (rule-name rule)) invocation)
         (grammar            (if grammar-name
                                 (grammar:find-grammar grammar-name)
                                 grammar))
         (target-environment (if-let ((rule (grammar:find-rule rule-name grammar
                                                               :if-does-not-exist nil)))
                               (or (grammar::environment rule) ; TODO rule should always have a calling convention
                                   (grammar:default-environment grammar invocation))
                               (grammar:default-environment grammar invocation))))
    (typep target-environment (class-of environment))))

(defun cached (cache rule &rest state)
  (gethash (list rule state) cache))

(defun (setf cached) (new-value cache rule &rest state)
  (setf (gethash (list rule state) cache) new-value))

(defun cached/arguments (cache rule arguments &rest state)
  (gethash (list rule state arguments) cache))

(defun (setf cached/arguments) (new-value cache rule arguments &rest state)
  (setf (gethash (list rule state arguments) cache) new-value))

(defun emit-find-grammar-and-rule (grammar grammar-name rule-name
                                   context-var grammar-var rule-var)
  (flet ((make-load-time-lookup (grammar-name)
           `(load-time-value
             (grammar:find-rule ',rule-name (grammar:find-grammar ',grammar-name)
                                :if-does-not-exist :forward-reference))))
    (if grammar-name
        `((,rule-var ,(make-load-time-lookup grammar-name)))
        (let+ ((grammar-name (grammar:name grammar))
               ((&with-gensyms context-grammar-var)))
          `((,grammar-var         (load-time-value
                                   (grammar:find-grammar ',grammar-name)))
            (,context-grammar-var (context-grammar ,context-var))
            (,rule-var            (if (eq ,context-grammar-var ,grammar-var)
                                      ,(make-load-time-lookup grammar-name)
                                      (grammar:find-rule ',rule-name ,context-grammar-var))))))))

;; TODO make lookup-and-call/single-argument so we don't have
;; cons and copy-list for a single argument, rename {with ->
;; multiple}-arguments
;; TODO include grammar name in cache key?
(defun emit-call (rule-var context state-variables arguments-var)
  `(,(if arguments-var 'apply 'funcall)
    ,rule-var ,context ,@state-variables
    ,@(when arguments-var `(,arguments-var))))

(defun emit-lookup-and-call
    (rule-name rule-var position-variables state-variables arguments-var
     &key grammar-name grammar)
  (let* ((rule-key    (if grammar-name
                          `(,rule-name . ,grammar-name)
                          `,rule-name))
         (context     parser.packrat.compiler::+context-var+)
         (cache-place #+eventually `(cache:cached
                                     ',rule-name ,@position-variables ,cache-var)
                      `(cached
                        (context-cache ,parser.packrat.compiler::+context-var+) ',rule-key
                        ,@position-variables
                        ,@(when arguments-var `(,arguments-var)))))
    `(values-list
      (or ,cache-place
          ,(maybe-let (when arguments-var `((,arguments-var (copy-list ,arguments-var))))
             (emit-d-call rule-key context position-variables state-variables arguments-var :grammar grammar)
             `(let ((*depth* (+ *depth* 2))
                    (*old-state* (list ,@ (remove-if (rcurry #'member position-variables) state-variables))))
                (setf ,cache-place
                      (multiple-value-list
                       ,(emit-call rule-var context state-variables arguments-var)))))))))

(defun emit-call/no-cache
    (rule-name rule-var position-variables state-variables arguments-var
     &key grammar-name grammar)
  (let ((rule-key (if grammar-name
                      `(,rule-name . ,grammar-name)
                      `,rule-name))
        (context  parser.packrat.compiler::+context-var+))
    `(progn
       ,(emit-d-call rule-key context position-variables state-variables arguments-var :grammar grammar)
       (let ((*depth*     (+ *depth* 2))
             (*old-state* (list ,@(remove-if (rcurry #'member position-variables) state-variables))))
         ,(emit-call rule-var context state-variables arguments-var)))))

(defmethod compile-expression ((grammar      base-grammar) ; TODO (grammar t)?
                               (environment  t)
                               (expression   rule-invocation-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o cached?) grammar)
         ((&accessors-r/o (grammar-name grammar) (rule-name rule) arguments) expression)
         (state-variables    (env:state-variables environment))
         (position-variables (env:position-variables environment))
         ((&with-gensyms grammar-var rule-var arguments-var success?-var value-var))
         ((&labels+ argument ((&optional first &rest rest) #+no arguments environment)
            (values
             (if first
                 (let ((new-e ))
                   (list*
                    (compile-expression
                     grammar environment first
                     (lambda (new-environment)
                       (setf new-e new-environment)
                       (env:value new-environment)
                       #+no `(let ((,arguments-var (cons ,(env:value new-environment)
                                                         ,arguments)))
                               (declare (dynamic-extent ,arguments-var))
                               ))
                     failure-cont)
                    (argument rest #+no arguments-var new-e)))
                                        ; (values (lookup-and-call arguments) environment)

                 '())
             environment)))
         ((&values argument-forms call-environment)
          (argument arguments #+no nil environment)) ; TODO ENVIRONMENT is just passed trough, then bound to CALL-ENVIRONMENT
         (continue-environment (add-value (env:environment-at call-environment :fresh) value-var)))
    (maybe-let* `(,@(emit-find-grammar-and-rule
                     grammar grammar-name rule-name
                     parser.packrat.compiler::+context-var+ grammar-var rule-var)
                  ,@(when arguments
                      `((,arguments-var (list ,@argument-forms)))))
      (when arguments `(declare (dynamic-extent ,arguments-var)))
      ;; TODO tail calls do not need the receiving part
      `(multiple-value-bind (,success?-var
                             ,@(env:position-variables continue-environment)
                             ,@(if (member value-var (env:position-variables continue-environment))
                                   '(message)
                                   `(,value-var)))
           ,(funcall (if cached?
                         'emit-lookup-and-call
                         'emit-call/no-cache)
                     rule-name rule-var position-variables state-variables
                     (when arguments arguments-var) :grammar-name grammar-name :grammar grammar)
         ,(emit-d-return rule-name parser.packrat.compiler::+context-var+
                         position-variables state-variables (when arguments arguments-var)
                         success?-var (env:position-variables continue-environment) value-var)
         (case ,success?-var
           ((t)    ,(funcall success-cont continue-environment))
           ((nil)  ,(funcall failure-cont continue-environment))
           (:fatal ,(funcall parser.packrat.compiler::*fatal-cont* continue-environment (if (member value-var (env:position-variables continue-environment))
                                                                                            'message
                                                                                            value-var))))))))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   next-rule-invocation-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o cached? (grammar-name grammar:name)) grammar)
         ((&accessors-r/o arguments) expression)
         (rule-name          parser.packrat.compiler::*rule-name*)
         (state-variables    (env:state-variables environment))
         (position-variables (env:position-variables environment))
         ((&with-gensyms rule-var arguments-var success?-var value-var))
         ((&labels+ argument ((&optional first &rest rest) #+no arguments environment)
            (values
             (if first
                 (let ((new-e ))
                   (list*
                    (compile-expression
                     grammar environment first
                     (lambda (new-environment)
                       (setf new-e new-environment)
                       (env:value new-environment)
                       #+no `(let ((,arguments-var (cons ,(env:value new-environment)
                                                         ,arguments)))
                               (declare (dynamic-extent ,arguments-var))
                               ))
                     failure-cont)
                    (argument rest #+no arguments-var new-e)))
                                        ; (values (lookup-and-call arguments) environment)

                 '())
             environment)))
         ((&values argument-forms call-environment)
          (argument arguments #+no nil environment)) ; TODO ENVIRONMENT is just passed trough, then bound to CALL-ENVIRONMENT
         (continue-environment (add-value (env:environment-at call-environment :fresh) value-var)))

    (maybe-let `((,rule-var #+no (load-time-value
                             (grammar:find-rule ',rule-name (grammar:find-grammar ',grammar-name)
                                                :if-does-not-exist :forward-reference))
                            (grammar:find-rule ',rule-name (first (grammar:dependencies (context-grammar ,parser.packrat.compiler::+context-var+)))
                                               ))
                 ,@(when arguments `((,arguments-var (list ,@argument-forms)))))
      ;; TODO wrong (when arguments `(declare (dynamic-extent ,arguments-var)))
      ;;            At least SBCL stack-allocated the arguments of the `list' application, not just the list itself
      ;; TODO tail calls do not need the receiving part
      `(multiple-value-bind (,success?-var
                             ,@(env:position-variables continue-environment)
                             ,@(if (member value-var (env:position-variables continue-environment))
                                   '(message)
                                   `(,value-var)))
           ,(funcall (if cached?
                         'emit-lookup-and-call
                         'emit-call/no-cache)
                     rule-name rule-var position-variables state-variables
                     (when arguments arguments-var)
                     :grammar grammar)
         ,(emit-d-return rule-name parser.packrat.compiler::+context-var+
                         position-variables state-variables (when arguments arguments-var)
                         success?-var (env:position-variables continue-environment) value-var)
         (case ,success?-var
           ((t)    ,(funcall success-cont continue-environment))
           ((nil)  ,(funcall failure-cont continue-environment))
           (:fatal ,(funcall parser.packrat.compiler::*fatal-cont* continue-environment (if (member value-var (env:position-variables continue-environment))
                                                                                            'message
                                                                                            value-var))))))))

;;; Rule

(defmethod compile-rule :around ((grammar    base-grammar)
                                 (name       t)
                                 (parameters list)
                                 (expression t)
                                 &key environment)
  (declare (ignore name environment))
  (let ((*gensym-counter* 0))
    (call-next-method)))
