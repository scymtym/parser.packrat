;;;; compiler.lisp --- Expression compilation in the grammar.values module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.values)

;;; Expression

(defmethod c:compile-expression ((grammar      t)
                                 (environment  values-environment)
                                 (expression   seq::bounds-test-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (c:compile-expression
   grammar environment (exp:sub-expression expression)
   success-cont failure-cont))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  values-environment)
                                 (expression   seq::element-access-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let* ((value           (value environment))
         (new-environment (env:environment-at environment (list :value value)
                                              :class 'env:value-environment
                                              :state '())))
    (c:compile-expression
     grammar new-environment (exp:sub-expression expression)
     success-cont failure-cont)))

(defmethod c:compile-expression ((grammar      t)
                                 (environment  values-environment)
                                 (expression   seq::advance-expression)
                                 (success-cont function)
                                 (failure-cont function))
  (let+ (((&accessors-r/o (amount seq::amount)) expression))
    (assert (eql 1 amount))
    (c:compile-expression
     grammar environment (exp:sub-expression expression)
     (lambda (element-environment)
       (let ((new-environment (env:environment-at environment :next-value
                                                  :parent element-environment)))
         (funcall success-cont new-environment)))
     failure-cont)))

;;; Rule

(defmethod c:make-rule-lambda ((grammar     t)
                               (environment values-environment)
                               (parameters  t)
                               (body        t))
  (let ((parameters (append (values* environment) parameters)))
    (call-next-method grammar environment parameters body)))
