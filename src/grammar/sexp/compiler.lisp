(cl:in-package #:parser.packrat.grammar.sexp)

(defmethod compile-expression ((grammar      t)
                               (environment  env:environment)
                               (expression   structure-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (type type*) readers sub-expressions) expression)
         (readers   (mapcar #'ensure-list readers)) ; TODO should not happen here
         (slot-vars (map 'list (compose #'gensym #'string #'first) readers))
         (value     (env:value environment))
         ((&labels+ slot ((&optional ((first-reader &rest first-args) '(nil)) &rest rest-readers)
                          (&optional first-expression                         &rest rest-expressions)
                          (&optional first-var                                &rest rest-vars)
                          slot-environment)
            (let+ (((&flet make-reader-args ()
                      (cond
                        ((not first-args)
                         (list value))
                        ((find :x first-args) ; TODO temp hack
                         (substitute value :x first-args))
                        (t
                         (list* value first-args)))))
                   ((&flet make-value-environment (value)
                      (env:environment-at
                             slot-environment (list :value value)
                             :class 'env:value-environment
                             :state '()))))
              (if first-reader
                  `(let ((,first-var (,first-reader ,@(make-reader-args))))
                     ,(compile-expression
                       grammar
                       #+old (env:environment-carrying environment first-var)
                       (make-value-environment first-var)
                       first-expression
                       (curry #'slot rest-readers rest-expressions rest-vars)
                       failure-cont))
                  (funcall success-cont (make-value-environment value)))))))
    (compile-expression
     grammar environment type
     (lambda (new-environment)
       `(if (typep ,value ,(env:value new-environment))
            ,(slot readers sub-expressions slot-vars environment)
            ,(funcall failure-cont environment)))
     failure-cont)))

;;; Casts

(defvar *just-test-bounds*
  (make-instance 'seq::bounds-test-expression
                 :sub-expression (make-instance 'seq:sequence-expression
                                                :sub-expressions '())))

(defun compile-cast-expression
    (grammar environment expression success-cont failure-cont
     environment-class state &optional bindings)
  (let+ ((value (env:value environment))
         ((&flet call-with-value-environment (cont parent-environment)
            (funcall cont (env:environment-at
                           parent-environment (list :value value)
                           :class 'env:value-environment
                           :state '()))))
         (sequence-environment (env:environment-at
                                environment state
                                :class environment-class
                                :state '())))
    `(if (typep ,value ',(target-type expression))
         ,(maybe-let bindings
            (compile-expression
             grammar sequence-environment (sub-expression expression)
             ;; If the sub-expression succeeds, ensure that the entire
             ;; sequence has been consumed by compiling a
             ;; `bounds-test-expression' that is expected to fail. In
             ;; any case, continue with a new `value-environment' for
             ;; VALUE.
             (lambda (new-environment)
               (compile-expression
                grammar new-environment *just-test-bounds*
                (curry #'call-with-value-environment failure-cont)
                (curry #'call-with-value-environment success-cont)))
             (curry #'call-with-value-environment failure-cont)))
         ,(funcall failure-cont environment))))

(defmethod compile-expression ((grammar      t)
                               (environment  env:environment)
                               (expression   as-list-expression)
                               (success-cont function)
                               (failure-cont function))
  (compile-cast-expression
   grammar environment expression success-cont failure-cont
   'seq:list-environment (list :tail (env:value environment))))

(defmethod compile-expression ((grammar      sexp-grammar)
                               (environment  seq:list-environment)
                               (expression   rest-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&with-gensyms tail-var end-var))
         (rest-environment (env:environment-at
                            environment (list :value tail-var)
                            :class 'env:value-environment
                            :state '())))
    `(let ((,tail-var ,(seq:tail environment)))
       ,(compile-expression
         grammar rest-environment (exp:sub-expression expression)
         (lambda (new-environment)
           (let ((end-environment (env:environment-at
                                   new-environment (list :tail end-var)
                                   :class 'seq:list-environment
                                   :state '())))
             `(let ((,end-var nil))
                ,(funcall success-cont end-environment))))
         (lambda (new-environment)
           (declare (ignore new-environment))
           (funcall failure-cont environment))))))

(defmethod compile-expression ((grammar      sexp-grammar)
                               (environment  env:environment)
                               (expression   as-vector-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ ((value (env:value environment))
         ((&with-gensyms end-var)))
    (compile-cast-expression
     grammar environment expression success-cont failure-cont
     'seq:vector-environment
     (list :sequence value
           :position 0
           :end      end-var)
     `((,end-var (length ,value))))))
