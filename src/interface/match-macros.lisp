(cl:in-package #:parser.packrat)

(defmacro define-rule-macro ())

;;;

#+no (map-matches 'list (lambda (pattern body)
                          )
                  '(* (list* (<- pattern any) (<- body (* any))))
                  clauses)

#+later (defmacro match (value-and-options &body clauses)
          (let+ (((value &key (grammar (find-symbol (string '#:sexp-grammar)
                                                    (find-package '#:parser.packrat.grammar.sexp))))
                  (typecase value-and-options
                    ((cons (eql values))
                     (list value-and-options))
                    (list
                     value-and-options)
                    (t
                     (list value-and-options))))
                 (grammar    (make-instance grammar))
                 (expression `(or ,@(map 'list (lambda+ ((pattern body))
                                                 `(:transform ,pattern ,body))
                                         clauses)))
                 (parsed     (parser.packrat.bootstrap:parse
                              expression))
                 ((&with-gensyms (block-name #:match) (value-var #:value)))
                 ((&values environment variables values)
                  (typecase value
                    ((cons (eql values))
                     (let* ((values    (rest value))
                            (variables (map-into (make-list (length values)) #'gensym)))
                       (values
                        (make-instance 'parser.packrat.grammar.values::values-environment
                                       :values variables)
                        variables
                        values)))
                    (t
                     (let+ (((&with-gensyms value-var)))
                       (values
                        (make-instance 'parser.packrat.environment:value-environment
                                       :value value-var)
                        (list value-var)
                        (list value))))))
                 (form (parser.packrat.compiler:compile-expression
                        grammar environment parsed
                        #+no #'parser.packrat.environment:value
                        (constantly :match)
                        (lambda (failure-environment)
                          (declare (ignore failure-environment))
                          'nil))))
            (print `(block ,block-name
                      (multiple-value-bind ,variables (values ,@values)
                        ,form)))))

(defun parse-match-value-and-options (value-and-options)
  (let+ (((value &key (grammar (find-symbol (string '#:sexp-grammar)
                                            (find-package '#:parser.packrat.grammar.sexp))))
          (ensure-list value-and-options)))
    (values value (make-instance grammar :name :temp))))

(defmacro single-value-match (value-and-options &body clauses)
  (let+ (((&values value grammar)
          (parse-match-value-and-options value-and-options))
         ((&with-gensyms value-var))
         (environment (make-instance 'parser.packrat.environment:value-environment
                                     :value value-var))
         (expression  `(or ,@(map 'list (lambda+ ((pattern body))
                                          `(:transform ,pattern ,body))
                                  clauses)))
         (parsed      (parser.packrat.bootstrap:parse
                       expression))

         ;; TODO this should be exposed by the compiler
         ((&flet references-with-mode (mode)
            (parser.packrat.expression:variable-references
             grammar parsed
             :filter (lambda (node)
                       (eq (parser.packrat.expression:mode node) mode)))))
         (writes             (references-with-mode :write))
         (assigned-variables (remove-duplicates writes :key #'parser.packrat.expression:variable))
         (assigned-names     (mapcar #'parser.packrat.expression:variable assigned-variables)))

    `(let ((,value-var ,value)
           ,@assigned-names)
       ,(parser.packrat.compiler:compile-expression
         grammar environment parsed
         #'parser.packrat.environment:value
         (lambda (failure-environment)
           (declare (ignore failure-environment))
           'nil)))))

#+later (defmacro multiple-value-match (value-and-options &body multiclauses)
          (let+ (((&values value grammar)
                  (parse-match-value-and-options value-and-options))
                 ((&values values-max clauses)
                  (loop :for (patterns . body) :in multiclauses
                        :collect `((:seq ,@patterns) ,@body) :into clauses
                        :maximize (length patterns) :into values-max
                        :finally (return (values values-max clauses))))
                 (value-vars  (map-into (make-list values-max) #'gensym))
                 (environment (make-instance 'parser.packrat.grammar.values::values-environment
                                             :values value-vars))
                 (expression  `(or ,@(map 'list (lambda+ ((pattern body))
                                                  `(:transform ,pattern ,body))
                                          clauses)))
                 (parsed      (parser.packrat.bootstrap:parse
                               expression)))
            `(multiple-value-bind ,value-vars ,value
               ,(parser.packrat.compiler:compile-expression
                 grammar environment parsed
                 #+no #'parser.packrat.environment:value
                 (constantly :match)
                 (lambda (failure-environment)
                   (declare (ignore failure-environment))
                   'nil)))))


#+test (multiple-value-match ((values 1 2 3))
         ((1 2 3) :match))

#+test (sb-disassem:disassemble-code-component
        (compile nil '(lambda (foo)
                       (declare (optimize (speed 3) (debug 0) (safety 0)))
                       (single-value-match foo
                         ((or 1 (vector 1 2)) 2)
                         (3                   4)))))

#+test (sb-disassem:disassemble-code-component
        (compile nil '(lambda (a b c)
                       (declare (optimize (speed 3) (debug 0) (safety 0)))
                       (multiple-value-match ((values a b c))
                         ((1 (list 2 x) 3) 2)
                         ((3)              4)))))
#+test (:ce '(:seq 1 (list 2 x) 3)
        :grammar :sexp
        :environment  (make-instance 'parser.packrat.grammar.values::values-environment
                                     :values '(a b c))
        :success-cont (constantly :success))
