(cl:in-package #:parser.packrat.environment)

;;; `environment'

(defclass environment (print-items:print-items-mixin)
  ((parent   :initarg  :parent
             :reader   parent
             :initform nil)
   (bindings :reader   %bindings
             :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object environment))
  `((:binding-count ,(hash-table-count (%bindings object)) "(~D)")
    (:depth         ,(depth object)                        " @~D"
     ((:after :binding-count)))))

(defmethod lookup ((name t) (environment environment))
  (let+ (((&values value value?) (gethash name (%bindings environment))))
    (if value?
        value
        (when-let ((parent (parent environment)))
          (lookup name parent)))))

(defmethod (setf lookup) ((new-value t) (name t) (environment environment))
  (setf (gethash name environment) new-value))

;;; `value-environment'

(defclass value-environment (environment
                             print-items:print-items-mixin)
  ((value :initarg :value
          :type    symbol
          :reader  value))
  (:default-initargs
   :value (more-conditions:missing-required-initarg 'value-environment :value)))

(defmethod print-items:print-items append ((object value-environment))
  `((:value ,(value object))))

(define-state-methods value-environment (value) ())