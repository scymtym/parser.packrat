(cl:in-package #:parser.packrat)

(defmacro defgrammar (name &body options)
  "Define a new grammar named NAME.

   syntactically similar to CL:DEFPACKAGE. OPTIONS can be any of the
   following:

   * (:CLASS CLASS-NAME)

     TODO

   * (:USE GRAMMAR-DESIGNATOR+)

     Allow using rules defined in the grammar(s) designated by
     GRAMMAR-DESIGNATOR+ in the grammar being defined.

   * (:DOCUMENTATION STRING)

     Install STRING as the documentation string of the grammar being
     defined.

   The :USE option can be supplied multiple times.

   When used grammars are cannot be found at compile time, a full warning
   is signaled. When used grammars are cannot be found at runtime, an
   error is signaled.

   Depending on the grammar class (see :CLASS option), additional
   options may be accepted. For example, sequence-based grammars
   accept the following additional options:

   * (:SEQUENCE-TYPE TYPE)

     TODO

   * (:ELEMENT-TYPE TYPE)

     TODO

   Grammars can be redefined in a similar way packages can: if the
   redefinition would introduce an error (e.g. a used grammar cannot
   be found), the previous definition is kept. When the redefinition
   succeeds, existing rules are retained but may behave differently
   since added/removed used grammars can cause nonterminals to become
   defined or undefined."
  (check-type name grammar-designator)

  (let ((element-type (singleton-option
                       'defgrammar :element-type '(not (or null keyword))))
        (class (singleton-option 'defgrammar :class '(not null)))
        (use '())
        (documentation (singleton-option 'defgrammar :documentation 'string)))
    ;; Build and check use list. If used grammars cannot be found,
    ;; still expand, but signal full warnings.
    (dolist (option options)
      (destructuring-bind (keyword &rest value) option
        (ecase keyword
          (:element-type (apply element-type value))
          (:class (apply class value))
          (:use
           (dolist (used value)
             (coerce-to-grammar used :if-does-not-exist #'warn))
           (appendf use value))
          (:documentation
           (apply #'documentation value)))))

    ;; MAKE-GRAMMAR signals an error at runtime if a used grammar
    ;; cannot be found.
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (make-grammar ,(string name)
                     ,@(when (funcall element-type)
                         `(:element-type ',(funcall element-type)))
                     ,@(when (funcall class)
                         `(:class ',(funcall class)))
                     :use ',use
                     :documentation ,(funcall documentation)))))

(defvar *grammar*) ; TODO move this somewhere

(defmacro in-grammar (grammar-name)
  ; (check-type grammar grammar-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *grammar* (parser.packrat.grammar:find-grammar ',grammar-name))))

(defmacro defrule (name-and-options (&rest parameters)
                                       expression &optional production)
  (let+ (((name &key
                ((:grammar grammar-name) :test)
                environment)
          (ensure-list name-and-options))
         (grammar (parser.packrat.grammar:find-grammar grammar-name)) ; TODO grammar-designator
         (ast     (parser.packrat.bootstrap::bootstrap-parse
                   (if production
                       `(:transform ,expression ,production)
                       expression))))
    (:tree ast)
    `(setf (parser.packrat.grammar:find-rule ',name (parser.packrat.grammar:find-grammar :test))
           ,(apply #'parser.packrat.compiler:compile-rule
                   grammar parameters ast
                   (when environment (list :environment (eval environment)))))))

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
         (parsed     (parser.packrat.bootstrap::bootstrap-parse
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
    (values value (make-instance grammar))))

(defmacro single-value-match (value-and-options &body clauses)
  (let+ (((&values value grammar)
          (parse-match-value-and-options value-and-options))
         ((&with-gensyms value-var))
         (environment (make-instance 'parser.packrat.environment:value-environment
                                     :value value-var))
         (expression  `(or ,@(map 'list (lambda+ ((pattern body))
                                          `(:transform ,pattern ,body))
                                  clauses)))
         (parsed      (parser.packrat.bootstrap::bootstrap-parse
                       expression)))
    `(let ((,value-var ,value))
       ,(parser.packrat.compiler:compile-expression
         grammar environment parsed
         #+no #'parser.packrat.environment:value
         (constantly :match)
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
         (parsed      (parser.packrat.bootstrap::bootstrap-parse
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
