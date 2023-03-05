;;;; macros.lisp --- Macros provided by the interface module.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat)

(defun singleton-option (context name type)
  (let (seen-value
        (value-seen? nil))
    (lambda (&optional (value nil value?))
      (cond (value?
             (when value-seen?
               (error "~@<The ~A option ~A can be supplied only once.~@:>"
                      context name))
             (unless (typep value type)
               (error "~@<The value ~S supplied for the ~A option ~A is ~
                       not of the required type ~S.~@:>"
                      value context name type))
             (setf seen-value value value-seen? t))
            (t
             seen-value)))))

(defmacro defgrammar (name &body options)
  "Define a new grammar named NAME.

Neither NAME nor any of the value expressions in OPTIONS are
evaluated.

OPTIONS can be any of the following:

* (:class CLASS-NAME)

  If supplied, the new grammar will be an instance of the class named
  CLASS-NAME.

  If a grammar named NAME exists and is not an instance of the class
  named CLASS-NAME, the class of the existing grammar will be changed.

* (:use GRAMMAR-DESIGNATOR+)

  Allow using rules defined in the grammar(s) designated by
  GRAMMAR-DESIGNATOR+ in the grammar being defined.

  The :USE option can be supplied multiple times.

  When used grammars are cannot be found at compile time, a full
  warning is signaled. When used grammars are cannot be found at
  runtime, an error is signaled.

* (:documentation STRING)

  Install STRING as the documentation string of the grammar being
  defined.

Depending on the grammar class (see `:class' option), additional
options may be accepted. For example, sequence-based grammars accept
the following additional options:

* (:sequence-type TYPE)

  TODO

* (:element-type TYPE)

  TODO

Grammars can be redefined in a similar way packages can: if the
redefinition would introduce an error (e.g. a used grammar cannot be
found), the previous definition is retained. When the redefinition
succeeds, existing rules are retained but may behave differently since
added/removed used grammars can cause nonterminals to become defined
or undefined."
  (check-type name grammar-designator)

  (let (#+no (element-type (singleton-option
                             'defgrammar :element-type '(not (or null keyword))))
        (class         (singleton-option 'defgrammar :class '(and symbol (not null))))
        (use           '())
        (cached?       (singleton-option 'defgrammar :cached? 'boolean))
        (documentation (singleton-option 'defgrammar :documentation 'string)))
    ;; Build and check use list. If used grammars cannot be found,
    ;; still expand, but signal full warnings.
    (dolist (option options)
      (destructuring-bind (keyword &rest value) option
        (with-current-source-form (value option)
          (ecase keyword
            ;; (:element-type (apply element-type value))
            (:class (apply class value))
            (:use
             (dolist (used value)
               (with-current-source-form (used value option)
                 (grammar:find-grammar used :if-does-not-exist #'warn))) ; TODO ensure?
             (appendf use value))
            (:cached? (apply cached? value))
            (:documentation (apply #'documentation value))))))

    ;; ENSURE-GRAMMAR signals an error at load time or runtime if a
    ;; used grammar cannot be found.
    (flet ((expansion (&rest extra-options)
             `(grammar:ensure-grammar
               ',name
               ,@(when-let ((grammar-class (funcall class)))
                   `(:grammar-class ',grammar-class))
               :cached? ',(funcall cached?)
               :use     ',use
               ,@extra-options)))
      `(progn
         (eval-when (:compile-toplevel)
           ,(expansion :if-used-does-not-exist nil))
         ,(expansion
           ;; ,@(when (funcall element-type)
           ;;    `(:element-type ',(funcall element-type)))
           ;; :documentation ,(funcall documentation)
           )))))

(defmacro in-grammar (grammar-name)
  "Set the current grammar to the grammar designated by GRAMMAR-NAME.

GRAMMAR-NAME is not evaluated."
  ;; TODO (check-type grammar grammar-designator)
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
