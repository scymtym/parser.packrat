(cl:in-package #:parser.packrat.grammar.tree)

(defmethod compile-expression ((grammar      t)
                               (environment  generator-environment)
                               (expression   seq::bounds-test-expression)
                               (success-cont function)
                               (failure-cont function))
  `(if (null (cdr ,(generator environment)))
       ,(funcall failure-cont environment)
       ,(compile-expression
         grammar environment (exp:sub-expression expression)
         success-cont failure-cont)))

(defmethod compile-expression ((grammar      t)
                               (environment  generator-environment)
                               (expression   seq::advance-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (amount seq::amount)) expression))
    (assert (eql amount 1))
    (compile-expression
     grammar environment (exp:sub-expression expression)
     (lambda (element-environment)
       (let ((new-environment (parser.packrat.grammar.base::add-value
                               (env:environment-at environment :fresh
                                                   :parent element-environment)
                               (env:value element-environment))))
         `(let ((,(generator new-environment) (cons nil nil)))
            (setf (values (car ,(generator new-environment))
                          (cdr ,(generator new-environment)))
                  (funcall (cdr ,(generator environment))))
            (print (list :generator ,(generator new-environment)))
            ,(funcall success-cont new-environment))))
     (lambda (element-environment)
       (declare (ignore element-environment))
       (funcall failure-cont environment)))))

(defmethod compile-expression ((grammar      t)
                               (environment  generator-environment)
                               (expression   seq::element-access-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&with-gensyms element))
         (new-environment (env:environment-at environment (list :value element)
                                              :class 'env:value-environment
                                              :state '())))
    `(let ((,(env:value new-environment) (car ,(generator environment))))
       (print (list :value ,(env:value new-environment)))
       ,(compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))

(defmethod compile-expression ((grammar      t)
                               (environment  env:value-environment)
                               (expression   ancestors-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (sub-expression exp:sub-expression)) expression)
         ((&with-gensyms generator-var))
         (new-environment (env:environment-at environment (list :generator generator-var)
                                              :class 'generator-environment
                                              :state '())))
    `(let ((,(generator new-environment) (cons nil nil)))
       (setf (cdr ,(generator new-environment))
             (depth-first-traverse ,(env:value environment)) ; TODO almost same for children- vs. ancestors-expression -> ancestor-expression-mixin)
             )
       ;; TODO later (declare (dynamic-extent ,position))
       ,(compile-expression
         grammar new-environment sub-expression
         (lambda (new-environment)
           (funcall success-cont new-environment))
         (lambda (failure-environment)
           (declare (ignore failure-environment))
           (funcall failure-cont environment))))))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  ancestor-expression-mixin))
  (let ((sub-expression (prepare-expression grammar
                                            (c2mop:class-prototype
                                             (c2mop:ensure-finalized
                                              (find-class 'generator-environment)))
                                            (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))

;;; Runtime support

(unless (assoc 'ancestors parser.packrat.bootstrap::*primitives*)
  (push (cons 'ancestors (lambda (expression recurse)
                           (let+ (((&ign sub-expression) expression)
                                  (sub-expression (funcall recurse sub-expression)))
                             (make-instance 'ancestors-expression
                                            :sub-expression sub-expression))))
        parser.packrat.bootstrap::*primitives*))
