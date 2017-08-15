(cl:in-package #:parser.packrat.grammar.stream)

(defmethod compile-expression ((grammar      t)
                               (environment  stream-environment)
                               (expression   seq::bounds-test-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o (position seq:position*) check-bounds-function) environment))
   `(if (funcall ,check-bounds-function ,position)
        ,(compile-expression
          grammar environment (exp:sub-expression expression)
          success-cont failure-cont)
        ,(funcall failure-cont environment))))

(defmethod compile-expression ((grammar      t)
                               (environment  stream-environment)
                               (expression   seq::element-access-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&with-gensyms element))
         ((&accessors-r/o (position seq:position*) access-function) environment)
         (new-environment (env:environment-at environment (list :value element)
                                              :class 'env:value-environment
                                              :state '())))
    `(let ((,element (funcall ,access-function ,position)))
       ,(compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))
