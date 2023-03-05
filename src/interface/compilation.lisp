;;;; compilation.lisp --- Compile time optimizations for the interface module.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

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
    (destructuring-bind (list rule &rest arguments) expression
      (declare (ignore list))
      (list* (second rule) arguments))))

;;; Analyze and maybe transform `parse' calls, possibly performing
;;; some of the following optimizations:
;;; * Resolve the GRAMMAR at load time
;;; * Parse and compile the EXPRESSION at compile-time
;;; * Insert an inline cache that will cache expression parsing and
;;;   rule compilation at runtime
(define-compiler-macro parse (expression input
                              &key (grammar nil grammar-supplied?))
  (let* ((grammar-constant    (cond ((not grammar-supplied?)
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
                                      expression/parsed :filter (of-type 'base:variable-reference-expression))))))
    (flet ((make-find-grammar-form ()
             `(load-time-value (grammar:find-grammar ',grammar-constant)))
           (make-inline-cache-form ()
             `(load-time-value (make-instance 'inline-cache))))
      (cond ((and expression/parsed grammar/resolved (not free-variables))
             ;; GRAMMAR and EXPRESSION are constant. Reference GRAMMAR
             ;; at load time and compile EXPRESSION now.
             `(grammar:parse
               ,(make-find-grammar-form)
               ,(make-rule-lambda grammar/resolved expression/parsed)
               ,input))

            ((not grammar-supplied?)
             ;; GRAMMAR has not been supplied which means that the
             ;; runtime value of *GRAMMAR* determines the grammar. The
             ;; best we can do is inserting an inline cache.
             `(funcall ,(make-inline-cache-form) *grammar* ,expression ,input))

            (grammar-constant
             ;; GRAMMAR is constant but EXPRESSION is not. Insert an
             ;; inline cache but at least perform the grammar lookup
             ;; at load time.
             `(funcall ,(make-inline-cache-form)
                       ,(make-find-grammar-form) ,expression ,input))

            (t
             ;; GRAMMAR has been supplied but is not constant. Insert
             ;; an inline cache and perform he grammar lookup at
             ;; runtime.
             `(funcall ,(make-inline-cache-form)
                       (grammar:find-grammar ,grammar) ; TODO inline cache could get grammar symbol and cache lookup
                       ,expression ,input))))))
