;;;; rule.lisp --- Rule and forward referenced rule classes.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar)

;;; `rule-base' class

(defclass rule-base (named-mixin
                     print-items:print-items-mixin
                     c2mop:funcallable-standard-object)
  ()
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation
   "Superclass for rule classes."))

#+todo (defmethod rule-function ((rule rule-base))
  )

(defmethod (setf rule-function) ((new-value function) (rule rule-base))
  (c2mop:set-funcallable-instance-function rule new-value))

;;; `forward-referenced-rule' class

(defclass forward-referenced-rule (rule-base)
  ()
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation
   "Instances represented undefined rules created by forward references."))

(defmethod initialize-instance :after ((instance forward-referenced-rule)
                                       &key)
  (setf (rule-function instance)
        (lambda (&rest args)
          (declare (ignore args))
          (error "~@<Forward referenced rule ~A invoked.~@:>"
                 instance))))

;;; `rule'

(defclass rule (rule-base)
  ((expression :initarg :expression
               :reader  expression))
  (:metaclass c2mop:funcallable-standard-class)
  (:default-initargs
   :expression (more-conditions:missing-required-initarg 'rule :expression))
  (:documentation
   "Instances represent defined rules."))

(defmethod shared-initialize :after ((instance   rule)
                                     (slot-names t)
                                     &key
                                     (function nil function-supplied?))
  (when function-supplied?
    (setf (rule-function instance) function)))
