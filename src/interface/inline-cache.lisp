(cl:in-package #:parser.packrat)

(defclass inline-cache (c2mop:funcallable-standard-object)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance inline-cache) &key)
  (c2mop:set-funcallable-instance-function
   instance (curry #'initial-function instance)))

(defun initial-function (instance grammar expression input)
  ;; Compile EXPRESSION into a function and replace the instance
  ;; function (which is currently this function).
  (c2mop:set-funcallable-instance-function
   instance (compile nil (make-inline-cache-form grammar expression)))
  ;; Call INSTANCE again which will use the new instance function.
  (funcall instance grammar expression input))

;;; Utilities

(declaim (inline expressions-compatible?))
(defun expressions-compatible? (old new invocation?)
  (or (eq old new)
      (equal old new)
      (and invocation? (equal (first old) (first new)))))

(defun make-inline-rule-lambda (grammar expression)
  (let* ((environment       (grammar:default-environment
                             grammar expression))
         (state-variables   (env:state-variables environment))
         (free-variables    (map 'list #'exp:variable
                                 (exp:variable-references
                                  expression :filter (of-type 'parser.packrat.grammar.base:variable-reference-expression)))))
    (values (parser.packrat.compiler:compile-rule
             grammar free-variables expression)
            state-variables
            free-variables)))

(defun make-inline-cache-form (grammar expression ; free-variables
                                )
  (let+ ((expression/parsed (grammar:parse-expression grammar expression))
         (invocation?       (typep expression/parsed 'parser.packrat.grammar.base:rule-invocation-expression))
         ((&values rule-lambda state-variables ; free-variables
                   )
          (make-inline-rule-lambda grammar expression/parsed)))
    `(lambda (grammar* expression* input*)
       (if (and (eq                      grammar*    ,grammar)
                (expressions-compatible? expression* ',expression ,invocation?))
           (progn (format *trace-output* "fast path ~S~%" expression*)
                  (grammar:parse ,grammar ,rule-lambda input* ; ,@state-variables ; ,@free-variables
                                 ))
           ;; Slow path
           (progn (format *trace-output* "slow path ~S~%" expression*)
            (grammar:parse grammar* expression* input*))))))
