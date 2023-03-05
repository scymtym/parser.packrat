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
         #+no (element-type (singleton-option
                        'defgrammar :element-type '(not (or null keyword))))
         (class         (singleton-option 'defgrammar :class '(not null)))
         (use           '())
         (cached?       (singleton-option 'defgrammar :cached? 'boolean))
         (documentation (singleton-option 'defgrammar :documentation 'string)))
    ;; Build and check use list. If used grammars cannot be found,
    ;; still expand, but signal full warnings.
    (dolist (option options)
      (destructuring-bind (keyword &rest value) option
        (ecase keyword
          ; (:element-type (apply element-type value))
          (:class (apply class value))
          (:use
           (dolist (used value)
             (grammar:find-grammar used :if-does-not-exist #'warn)) ; TODO ensure?
           (appendf use value))
          (:cached? (apply cached? value))
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
        :use     ',use
        :cached? ',(funcall cached?)
        ; :documentation ,(funcall documentation)
        ))))

(defmacro in-grammar (grammar-name)
  ; TODO (check-type grammar grammar-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *grammar* (grammar:find-grammar ',grammar-name))))

;;; `defrule'

(defun expand-expression (grammar name parameters expression environment)
  (let* ((ast      (handler-bind
                       ((grammar:expression-syntax-error
                          (lambda (condition)
                            (with-current-source-form
                                ((grammar:expression condition))
                              (error condition)))))
                     (grammar:parse-expression grammar expression)))
         (function (handler-bind ((parser.packrat.compiler::compilation-error
                                    (lambda (condition)
                                      (with-current-source-form
                                          ((parser.packrat.expression::source
                                            (parser.packrat.compiler::node condition))) ; TODO do this properly
                                        (error condition)))))
                     (apply #'parser.packrat.compiler:compile-rule
                            grammar name parameters ast
                            (when environment (list :environment environment))))))
    function))

(defmacro defrule (name-and-options (&rest parameters)
                   expression &rest production)
  (let+ (((name &key
                ((:grammar grammar-name) nil grammar-supplied?) ; TODO call this :in?
                environment)
          (ensure-list name-and-options))
         (grammar       (if grammar-supplied?
                            (grammar:find-grammar grammar-name) ; TODO grammar-designator
                            *grammar*))
         (grammar-name  (grammar:name grammar))
         (expression    (if production
                            `(:transform ,expression ,@production)
                            expression))
         (function-form (expand-expression
                         grammar name parameters expression
                         (when environment (eval environment)))))
    `(progn
       (eval-when (:compile-toplevel)
         (grammar:find-rule ',name (grammar:find-grammar ',grammar-name)
                            :if-does-not-exist (list :forward-reference
                                                     ,@(when environment
                                                         `(:environment ,environment)))))
       (grammar:ensure-rule ',name (grammar:find-grammar ',grammar-name)
                            :rule-class 'grammar::rule
                            ,@(when environment `(:environment ,environment))
                            :expression ',expression
                            :function   ,function-form))))
