;;;; environment.lisp --- Values environment provided by the grammar.values module.
;;;;
;;;; Copyright (C) 2017-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.values)

(defclass values-environment (env:environment
                              seq:sequential-environment-mixin
                              print-items:print-items-mixin)
  ((%values :initarg :values
            :reader  values*
            :writer  (setf %values))
   (%value  :initarg :value
            :reader  value
            :writer  (setf %value))))

(defmethod shared-initialize :after ((instance   values-environment)
                                     (slot-names t)
                                     &key (values nil            values-supplied?)
                                          (value  (first values)))
  (when values-supplied?
    (setf (%values instance) values
          (%value  instance) value)))

;; TODO invariant: value ∈ values

(defmethod print-items:print-items append ((object values-environment))
  (let+ (((&accessors-r/o (values values*) value) object)
         (index (position value values)))
    (if index
        `((:prefix                      "~{~A ~}" ,(subseq values 0 index))
          ((:current (:after  :prefix)) "!~A"     ,value)
          ((:suffix (:after  :current) (:before :binding-count))
           "~{ ~A~} " ,(subseq values (1+ index)) ()))
        `((:values "~{~A ~}" ,values)
          ((:end (:after :values) (:before :binding-count))
           "! ")))))

(env:define-state-methods values-environment (value) ((values* :values)))

;;; no runtime state
(defmethod env:position-variables ((environment values-environment))
  '())

(defmethod env:state-variables ((environment values-environment))
  '())

(defmethod env:environment-at ((environment values-environment)
                               (position    (eql :fresh))
                               &rest args &key parent state class)
  (declare (ignore parent state class))
  (apply #'env:environment-at environment (list :value (value environment))
         args))

(defmethod env:environment-at ((environment values-environment)
                               (position    (eql :next-value))
                               &rest args &key parent state class)
  (declare (ignore parent state class))
  (let+ (((&accessors-r/o (values values*) value) environment)
         (next-value (second (member value values))))
    (apply #'env:environment-at environment (list :value next-value)
           args)))
