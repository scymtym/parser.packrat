;;;; conditions.lisp --- Conditions signaled by the grammar module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar)

;;; TODO Do we want to go with foo-missing-{error,warning}?

(define-condition grammar-condition (condition)
  ((grammar :initarg :grammar
            :reader  grammar))
  (:default-initargs
   :grammar (more-conditions:missing-required-initarg 'grammar-condition :grammar)))

(define-condition rule-condition (condition)
  ((rule :initarg :rule
         :reader  rule))
  (:default-initargs
   :rule (more-conditions:missing-required-initarg 'rule-condition :rule)))

(define-condition grammar-missing-problem (grammar-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<A grammar named ~S does not exist.~@:>"
             (grammar condition)))))

(define-condition grammar-missing-warning (grammar-missing-problem
                                           warning)
  ())

(define-condition grammar-missing-error (grammar-missing-problem
                                         error)
  ())

(define-condition rule-missing-problem (grammar-condition
                                        rule-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<A rule named ~S does not exist in grammar ~
                     ~A.~@:>"
             (rule condition) (grammar condition)))))

(define-condition rule-missing-warning (rule-missing-problem
                                        warning)
  ())

(define-condition rule-missing-error (rule-missing-problem
                                      error)
  ())
