;;;; compiler.lisp --- Expression compilation for grammar.stream module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.stream)

;;; Expressions

(defmethod c:compile-expression ((grammar      t)
                                 (environment  stream-environment)
                                 (expression   seq::bounds-test-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ (((&accessors-r/o (position seq:position*) check-bounds-function) environment))
    `(if (funcall ,check-bounds-function ,position)
         ,(c:compile-expression
           grammar environment (exp:sub-expression expression)
           success-cont failure-cont)
         ,(funcall failure-cont environment))))

(defmethod c:compile-expression ((grammar      t)
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
       (declare (type  #+TODO (element-type environment) character ,element))
       ,(c:compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))

;;; Rules

(defmethod c:make-rule-lambda ((grammar     t)
                               (environment stream-environment)
                               (parameters  t)
                               (body        t))
  (let+ (((&accessors-r/o (state-variables env:state-variables)
                          (position-var    seq:position*)
                          (stream-var      stream*)
                          check-bounds-function access-function)
          environment))
   `(lambda (,c::+context-var+ ,@state-variables ,@parameters)
      (declare ;; (optimize (speed 3) (debug 0) (safety 0))
               (ignorable ,c::+context-var+ ,stream-var)
               (type alexandria:array-index ,position-var)
               (type function ,check-bounds-function ,access-function))
      ,@body)))
