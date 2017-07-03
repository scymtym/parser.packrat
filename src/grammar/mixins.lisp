(cl:in-package #:parser.packrat.grammar)

;;; `named-mixin'

(defclass named-mixin ()
  ((name :initarg :name
         :type    symbol
         :reader  name))
  (:default-initargs
   :name (more-conditions:missing-required-initarg 'named-mixin :name)))

(defmethod print-items:print-items append ((object named-mixin))
  `((:name ,(name object))))

;;; `rule-storage-mixin'

(defclass rule-storage-mixin ()
  ((rules :reader   %rules
          :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object rule-storage-mixin))
  `((:rule-count ,(hash-table-count (%rules object)) " ~D rule~:P" ((:after :name)))))

(defmethod find-rule ((name symbol) (grammar rule-storage-mixin)
                      &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (gethash name (%rules grammar)))

(defmethod (setf find-rule) ((new-value t) (name symbol) (grammar rule-storage-mixin)
                             &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (gethash name (%rules grammar)) new-value))
