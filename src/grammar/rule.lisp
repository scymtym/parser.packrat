;;;; rule.lisp --- .
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar)

;;;

(defclass rule-base (c2mop:funcallable-standard-object
                     named-mixin)
  ()
  (:metaclass c2mop:funcallable-standard-class))

#+todo (defmethod rule-function ((rule rule-base))
  )

(defmethod (setf rule-function) ((new-value function) (rule rule-base))
  (c2mop:set-funcallable-instance-function rule new-value))

;;;

(defclass forward-referenced-rule (rule-base)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance forward-referenced-rule)
                                       &key)
  (setf (rule-function instance)
        (lambda (&rest args)
          (declare (ignore args))
          (error "~@<Forward referenced rule ~A invoked.~@:>"
                 instance))))

;;;

(defclass rule (rule-base)
  ((expression :initarg :expression
               :reader  expression))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod shared-initialize :after ((instance   rule)
                                     (slot-names t)
                                     &key
                                     (function nil function-supplied?))
  (when function-supplied?
    (setf (rule-function instance) function)))
