(cl:in-package #:parser.packrat.grammar.base)

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  env:value-environment)
                               (expression   predicate-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (sub-expression exp:sub-expression) predicate) expression))
    (compile-expression grammar environment sub-expression
     (lambda (new-environment)
       `(if (,predicate ,(env:value new-environment))
            ,(funcall success-cont new-environment)
            ,(funcall failure-cont new-environment)))
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
    `(if (,predicate ,value ',expected)
         ,(funcall success-cont environment) ; (producing (value environment) environment)
         ,(funcall failure-cont environment))))

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
   success-cont))

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
         ((&flet alternative (expression name next-name) ; TODO pass failing environment to next alternative
            `(,name ()
                ,(compile-expression
                  grammar environment expression
                  success-cont
                  (lambda (failure-environment)
                    (declare (ignore failure-environment))
                    (if next-name
                        `(,next-name)
                        (funcall failure-cont environment))))))))

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
                   (sub rest (env:environment-at new-environment (env:state-variables/plist environment)))
                   (funcall success-cont new-environment)))
             failure-cont))))
    (sub (sub-expressions expression) environment)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   compose-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&labels+ sub ((&optional first &rest rest) environment)
            (compile-expression
             grammar environment first
             (lambda (new-environment)
               (if rest
                   (sub rest new-environment)
                   (funcall success-cont new-environment)))
             failure-cont))))
    (sub (sub-expressions expression) environment)))

;;; Variables

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  env:value-environment)
                               (expression   set-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (variable exp:variable) sub-expression) expression))
    (if (env:lookup variable environment) ; TODO hack. this should not be a set expression in that case

        (compile-expression
         grammar environment sub-expression
         (lambda (new-environment)
           `(if (equal ,variable ,(env:value new-environment))
                ,(funcall success-cont new-environment)
                ,(funcall failure-cont environment)))
         failure-cont)

        (compile-expression
         grammar environment sub-expression
         (lambda (new-environment)
           (setf (env:lookup variable new-environment) t) ; TODO hack
           `(progn
              (setf ,variable ,(env:value new-environment))
              ,(funcall success-cont new-environment)))
         failure-cont)))
  #+old (let+ ((variable (exp::variable expression))
         ((&with-gensyms success-fun value-var))
         (success-body)
         (sub-body
          (compile-expression
           grammar environment (sub-expression expression)
           (lambda (new-environment)
             (setf success-body (funcall success-cont new-environment))
             `(,success-fun ,(env:value new-environment)))
           failure-cont)))
    `(labels ((,success-fun (,value-var)
                (setf ,variable ,value-var)
                ,success-body))
       ,sub-body)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  env:value-environment)
                               (expression   push-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (variable exp:variable) sub-expression) expression))
    (compile-expression
     grammar environment sub-expression
     (lambda (new-environment)
       `(progn
          (push ,(env:value new-environment) ,variable)
          ,(funcall success-cont new-environment)))
     failure-cont))
  #+old (let+ ((variable (exp::variable expression))
         ((&with-gensyms success-fun value-var))
         (success-body)
         (sub-body
          (compile-expression
           grammar environment (exp:sub-expression expression)
           (lambda (new-environment)
             (setf success-body (funcall success-cont new-environment))
             `(,success-fun ,(env:value new-environment)))
           failure-cont)))
    `(labels ((,success-fun (,value-var)
                (push ,value-var ,variable)
                ,success-body))
       ,sub-body)))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   variable-reference)
                               (success-cont function)
                               (failure-cont function))
  (let* ((variable (exp:variable expression))
         (binding  (env:lookup variable environment)))
    ;; TODO check environment
    #+old (funcall success-cont (print (env:environment-carrying
                                        environment variable)))
    (funcall success-cont
             (env:environment-at environment (list :value variable)
                                 :class 'env:value-environment
                                 :state '()))))

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   ignored-expression)
                               (success-cont function)
                               (failure-cont function))
  (funcall success-cont environment))

;;; Transform

;; TODO syntactically, EXPRESSION contains a sub-expression. However,
;; we do not use the value produced by that sub-expression.
(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   transform-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o code sub-expression) expression)
         ((&with-gensyms value-var aborted-var block-name))
         ((&flet make-value-environment (parent ; &optional path-suffix
                                         )
            (env:environment-at parent (list :value value-var)
                                :class   'env:value-environment
                                        ; :parent2 environment
                                :state '()
                                #+later (when path-suffix
                                          (list :path-suffix (list path-suffix)))))))
    (compile-expression
     grammar environment sub-expression
     (lambda (new-environment)
       (if t                            ; can-fail?
           `(let ((,value-var   nil)
                  (,aborted-var nil))
              (block ,block-name
                (flet ((:fail ()
                         (setf ,aborted-var t)
                         (return-from ,block-name)))
                  (declare (inline :fail)
                           (ignorable #':fail))
                  (setf ,value-var ,(maybe-progn code))))
              (if ,aborted-var
                  ,(funcall failure-cont environment)
                  ,(funcall success-cont (make-value-environment
                                          new-environment))))
           ;; CODE does not cause the match to fail.
           `(let ((,value-var ,(maybe-progn code)))
              ,(funcall success-cont (make-value-environment
                                      new-environment)))))
     failure-cont)))

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

(defun cached (cache rule &rest state)
  (gethash (list rule state) cache))

(defun (setf cached) (new-value cache rule &rest state)
  (setf (gethash (list rule state) cache) new-value))

(defun cached/arguments (cache rule arguments &rest state)
  (gethash (list rule state arguments) cache))

(defun (setf cached/arguments) (new-value cache rule arguments &rest state)
  (setf (gethash (list rule state arguments) cache) new-value))

(defvar *depth* 0)

(defconstant parser.packrat.compiler::+context-var+ 'context)

(defmethod compile-expression ((grammar      base-grammar)
                               (environment  t)
                               (expression   rule-invocation-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o #+todo (grammar-name exp:grammar) (rule-name rule #+maybe exp:rule) (arguments arguments #+maybe exp:arguments)) expression)
         (rule               `(load-time-value
                               (find-rule ',rule-name (find-grammar ',(name grammar))
                                          :if-does-not-exist :forward-reference)))
         (state-variables    (env:state-variables environment))
         (position-variables (env:position-variables environment))
         ((&with-gensyms arguments-var success?-var))
         ((&flet lookup-and-call/no-arguments ()
            (let ((cache-place #+eventually `(cache:cached
                                              ',rule-name ,@position-variables ,cache-var)
                               `(cached
                                 ,parser.packrat.compiler::+context-var+ ',rule-name ,@position-variables)))
              `(values-list (or ,cache-place
                                (progn
                                  (format t "~&~V@T~A@~S~%" *depth* ',rule-name (list ,@state-variables))
                                  (let ((*depth* (+ *depth* 2)))
                                    (setf ,cache-place (multiple-value-list (funcall ,rule ,parser.packrat.compiler::+context-var+ ,@state-variables))))))))))
         ;; TODO make lookup-and-call/single-argument so we don't have
         ;; cons and copy-list for a single argument, rename {with ->
         ;; multiple}-arguments
         ((&flet lookup-and-call/with-arguments (arguments)
            (let ((cache-place #+eventually `(cache:cached/arguments
                                              ',rule-name ,@position-variables ,arguments
                                              ,cache-var)
                               `(cached/arguments
                                 ,parser.packrat.compiler::+context-var+
                                 ',rule-name
                                 ,arguments
                                 ,@position-variables)))
              `(or ,cache-place
                   (let ((,arguments (copy-list ,arguments)))
                     (format t "~&~V@T~A@~S ~S~%" *depth* ',rule-name (list ,@state-variables) ,arguments)
                     (let ((*depth* (+ *depth* 2)))
                       (apply ,rule ,parser.packrat.compiler::+context-var+ ,@state-variables ,arguments)))))))
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
         (continue-environment #+no (make-instance 'parser.packrat.grammar.sequence::vector-environment
                                                   :parent   call-environment
                                                   :sequence (parser.packrat.grammar.sequence:sequence* environment)
                                                   :position 'k-p
                                                   :end      (parser.packrat.grammar.sequence:end environment))
                               #+no (env:environment-carrying call-environment 'v)
                               (env:environment-at call-environment :fresh)))
    `(let (,@(when arguments `((,arguments-var (list ,@argument-forms)))))
       ,@(when arguments `((declare (dynamic-extent ,arguments-var))))
       ;; TODO tail calls do not need the receiving part
       (multiple-value-bind (,success?-var
                             ,@(env:position-variables continue-environment))
           ,(if arguments
                (lookup-and-call/with-arguments arguments-var)
                (lookup-and-call/no-arguments))
         (format t "~&~V@T~A@~S -> ~S ~{~S~^ ~}~%" *depth* ',rule-name (list ,@state-variables)
                 ,success?-var (list ,@(env:position-variables continue-environment)))
         (if ,success?-var
             ,(funcall success-cont continue-environment)
             ,(funcall failure-cont continue-environment))))))

;;; Rule

(defmethod compile-rule :before ((grammar    base-grammar)
                                 (parameters list)
                                 (expression t)
                                 &key environment)
  (declare (ignore environment))
  #+no (let+ (((&flet references-with-mode (mode)
            (exp:variable-references grammar expression
                                     :filter (lambda (node)
                                               (eq (exp::mode node) mode)))))
         #+not-needed? (reads  (references-with-mode :read))
         (writes (references-with-mode :write)))
    (when-let ((offenders (remove-if
                           (lambda (expression)
                             (not (find (exp:variable expression) parameters
                                        :test #'eq)))
                           writes)))
      (error "~@<The assignment~P ~{~A~^, ~} would overwrite rule ~
              parameter.~@:>"
             (length offenders) offenders))))

(defmethod compile-rule :around ((grammar    base-grammar)
                                 (parameters list)
                                 (expression t)
                                 &key environment)
  (declare (ignore environment))
  (let ((*gensym-counter* 0))
    (call-next-method)))
