(defmethod compile-expression ((grammar      parser.packrat.grammar.sexp::sexp-grammar)
                               (environment  value-environment)
                               (expression   ancestor-expression)
                               (success-cont function)
                               (fail-cont    function))
  (let+ (((&accessors-r/o sub-expression) expression)
         ((&flet compile-with-node (node expression success-cont fail-cont)
            (let ((environment (make-instance 'value-environment
                                              :parent environment
                                              :value  node)))
              (compile-expression
               grammar environment expression
               success-cont fail-cont))))
         ((&with-gensyms visit-fun visit-node)))
    `(labels ((,visit-fun (,visit-node)
                ,(compile-with-node
                  visit-node sub-expression
                  success-cont
                  (lambda (fail-environment)
                    (declare (ignore fail-environment))
                    `(map nil #',visit-fun (children ,visit-node))))))
       (,visit-fun ,(value environment))
       ,(funcall fail-cont environment))))
