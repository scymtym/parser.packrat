(cl:in-package #:parser.packrat.environment)

;;; `path-mixin'

(defclass path-mixin ()
  ((%path-suffix :initarg  :path-suffix
                 :reader   path-suffix
                 :initform '())))

(defmethod path ((environment path-mixin))
  (append (path-suffix environment)
          (when-let ((parent (parent environment)))
            (path parent))))

;;; `environment'

(defclass environment (path-mixin
                       print-items:print-items-mixin)
  ((%parent   :initarg  :parent
              :reader   parent
              :initform nil)
   (%bindings :reader   %bindings
              :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object environment))
  (let ((count (hash-table-count (%bindings object)))
        (depth (depth object)))
    `((:binding-count                          "(~D)" ,count)
      ((:depth        (:after :binding-count)) " @~D" ,depth))))

(defmethod direct-bindings ((environment environment))
  (when-let ((bindings (%bindings environment)))
    (hash-table-alist bindings)))

(defmethod lookup ((name t) (environment environment))
  (let+ (((&values value value?) (gethash name (%bindings environment))))
    (if value?
        value
        (when-let ((parent (parent environment)))
          (lookup name parent)))))

(defmethod (setf lookup) ((new-value t) (name t) (environment environment))
  (when (null new-value) (break))
  (setf (gethash name (%bindings environment)) new-value))

;;; `value-environment'

(defclass value-environment (environment
                             print-items:print-items-mixin)
  ((%value :initarg :value
           :type    symbol
           :reader  value))
  (:default-initargs
   :value (more-conditions:missing-required-initarg 'value-environment :value)))

(defmethod print-items:print-items append ((object value-environment))
  `(((:value (:after :depth)) " ~A" ,(value object))))

(define-state-methods value-environment (value) ())
