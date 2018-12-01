(cl:in-package #:parser.packrat)

;;; Recognize forms of the form
;;;
;;;   (list 'RULE-NAME                ARG₁ …)
;;;   (list '(RULE-NAME GRAMMAR-NAME) ARG₁ …) [not yet]
;;;
;;; and transform them into
;;;
;;;   (RULE-NAME ARG₁ …)
;;;
;;; . Note that the resulting expression may have free variables.
(defun maybe-partially-evaluate-rule-invocation (expression)
  (when (typep expression '(cons (eql list)
                                 (cons (cons (eql quote) (cons symbol null))
                                       list)))
    (destructuring-bind (list rule &rest arguments)
        expression
      (declare (ignore list))
      (list* (second rule) arguments))))

(define-compiler-macro parse (expression input
                              &key (grammar nil grammar-supplied?))
  (let+ ((grammar-constant    (cond
                                ((not grammar-supplied?)
                                 nil #+no (when (boundp '*grammar*) ; TODO wrong
                                            *grammar*))
                                ((constantp grammar)
                                 (eval grammar))))
         (grammar/resolved    (typecase grammar-constant
                                (null)
                                (symbol ; TODO grammar-designator
                                 (grammar:find-grammar
                                  grammar-constant :if-does-not-exist nil)) ; TODO warning
                                (t
                                 grammar-constant)))
         (expression-constant (cond ((constantp expression)
                                     (eval expression))
                                    ((maybe-partially-evaluate-rule-invocation
                                      expression))))
         (expression/parsed   (when (and grammar/resolved expression-constant)
                                (grammar:parse-expression
                                 grammar/resolved expression-constant)))
         (free-variables      (when (and grammar/resolved expression/parsed)
                                (map 'list #'exp:variable
                                     (exp:variable-references
                                      expression/parsed :filter (of-type 'parser.packrat.grammar.base:variable-reference-expression)))))
         ((&flet make-find-grammar-form ()
            `(load-time-value (grammar:find-grammar ',grammar-constant)))))
    (cond ((and expression/parsed grammar/resolved (not free-variables))
           `(grammar:parse
             ,(make-find-grammar-form)
             ,(make-inline-rule-lambda grammar/resolved expression/parsed)
             ,input))

          ((not grammar-supplied?)
           `(funcall (load-time-value (make-instance 'inline-cache))
                     *grammar* ,expression ,input))

          (grammar-constant
           `(funcall (load-time-value (make-instance 'inline-cache))
                     ,(make-find-grammar-form)
                     ,expression ,input))

          (t
           `(funcall (load-time-value (make-instance 'inline-cache))
                     (grammar:find-grammar ,grammar) ; TODO inline cache could get grammar symbol and cache lokup
                     ,expression ,input)))))
