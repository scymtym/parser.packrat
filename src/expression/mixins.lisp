(cl:in-package #:parser.packrat.expression)

;;; `expression'

(defclass expression () ((%source :initarg :source :reader source :initform nil))) ; TODO hack

;;; `sub-expression-mixin'

(defclass sub-expression-mixin ()
  ((sub-expressions :initarg  :sub-expressions
                    :type     list
                    :reader   sub-expressions
                    :initform '())))

(defmethod print-items:print-items append ((object sub-expression-mixin))
  `((:sub-expression-count ,(length (sub-expressions object)) "(~D)")))

(defmethod bp:node-relations ((builder t)
                              (node    sub-expression-mixin))
  '((:sub-expression . *)))

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :sub-expression))
                             (node     sub-expression-mixin))
  (values (sub-expressions node)
          (load-time-value (circular-list '(:evaluated? t)))))

(defmethod bp:relate ((builder  t)
                      (relation (eql :sub-expression))
                      (left     sub-expression-mixin)
                      (right    t)
                      &key)
  (reinitialize-instance
   left :sub-expressions (append (sub-expressions left) (list right))))

;;; `single-sub-expression-mixin'

(defclass single-sub-expression-mixin ()
  ((sub-expression :initarg :sub-expression
                   :reader  sub-expression)))

(defmethod print-items:print-items append ((object single-sub-expression-mixin))
  (let ((sub-items (print-items:print-items (sub-expression object))))
    `((:sub-expression ,sub-items "~/print-items:format-print-items/"))))

(defmethod bp:node-relations ((builder t)
                              (node    single-sub-expression-mixin))
  '((:sub-expression . 1)))

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :sub-expression))
                             (node     single-sub-expression-mixin))
  (values (sub-expression node) '(:evaluated? t)))

(defmethod bp:relate ((builder  t)
                      (relation (eql :sub-expression))
                      (left     single-sub-expression-mixin)
                      (right    t)
                      &key)
  (reinitialize-instance left :sub-expression right))

;;; `value-environment-needing-mixin'

(defclass value-environment-needing-mixin ()
  ())
