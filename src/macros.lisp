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

  (let+ (((&flet singleton-option (context name type)
            (let ((value*))
              (lambda (&optional (value nil value?))
                (if value?
                    (setf value* value)
                    value*)))))
         (element-type (singleton-option
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
          #+no (:use
                (dolist (used value)
                  (coerce-to-grammar used :if-does-not-exist #'warn))
                (appendf use value))
          (:documentation
           (apply #'documentation value)))))

    ;; MAKE-GRAMMAR signals an error at runtime if a used grammar
    ;; cannot be found.
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (grammar:ensure-grammar
        ',name
        #+no ,@(when (funcall element-type)
                 `(:element-type ',(funcall element-type)))
        ,@(when (funcall class)
            `(:grammar-class ',(funcall class)))
                                        ; :use ',use
                                        ; :documentation ,(funcall documentation)
        ))))

(defvar *grammar*) ; TODO move this somewhere

(defmacro in-grammar (grammar-name)
  ; (check-type grammar grammar-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *grammar* (grammar:find-grammar ',grammar-name))))

(defmacro defrule (name-and-options (&rest parameters)
                   expression &optional production)
  (let+ (((name &key
                ((:grammar grammar-name) nil grammar-supplied?)
                environment)
          (ensure-list name-and-options))
         (grammar      (if grammar-supplied?
                           (grammar:find-grammar grammar-name) ; TODO grammar-designator
                           *grammar*))
         (grammar-name (grammar:name grammar))
         (ast          (parser.packrat.bootstrap::bootstrap-parse ; TODO this will be (grammar:parse-expression grammar expression)
                        (if production
                            `(:transform ,expression ,production)
                            expression))))
    (:tree ast)

    `(grammar:ensure-rule
      ',name
      (grammar:find-grammar ',grammar-name)
      :rule-class 'grammar::rule
      :expression ',expression
      :function   ,(apply #'parser.packrat.compiler:compile-rule
                          grammar parameters ast
                          (when environment (list :environment (eval environment)))))))
