(cl:in-package #:parser.packrat.grammar)

;;; `named-mixin'

(defclass named-mixin ()
  ((%name :initarg :name
          :type    symbol
          :reader  name))
  (:default-initargs
   :name (more-conditions:missing-required-initarg 'named-mixin :name)))

(defmethod print-items:print-items append ((object named-mixin))
  `((:name ,(name object))))

;;; `dependencies-mixin'

(defclass dependencies-mixin ()
  ((dependencies :reader   dependencies
                 :initform '())))

;;; `rule-storage-mixin'

(defclass rule-storage-mixin ()
  ((%rules :reader   %rules
           :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object rule-storage-mixin))
  `((:rule-count ,(hash-table-count (%rules object)) " ~D rule~:P" ((:after :name)))))

(defmethod rules ((grammar rule-storage-mixin))
  (hash-table-values (%rules grammar)))

(defmethod rules/alist ((grammar rule-storage-mixin))
  (hash-table-alist (%rules grammar)))

(defmethod find-rule ((name symbol) (grammar rule-storage-mixin)
                      &key recursive? if-does-not-exist)
  (declare (ignore recursive? if-does-not-exist))
  (gethash name (%rules grammar)))

(defmethod (setf find-rule) ((new-value t) (name symbol) (grammar rule-storage-mixin)
                             &key recursive? if-does-not-exist)
  (declare (ignore recursive? if-does-not-exist))
  (setf (gethash name (%rules grammar)) new-value))

;;; `meta-grammar-mixin'

(defun make-meta-expression (meta-grammar-name meta-start-rule-name)
  (let ((invoke-expression-class
          (find-symbol (string '#:rule-invocation-expression)
                       (find-package '#:parser.packrat.grammar.base))))
    (make-instance invoke-expression-class
                   :grammar meta-grammar-name
                   :rule    meta-start-rule-name)))

(defclass meta-grammar-mixin ()
  ;; TODO could just a use a meta-grammar-expression. that would
  ;; allow, among other things a qualified rule invocation, subsuming
  ;; this.
  ((%meta-grammar    :initarg :meta-grammar
                     :reader  meta-grammar
                     :documentation
                     "Stores the grammar according to which
                      expressions for this grammar should be parsed.")
   (%meta-start-rule :initarg :meta-start-rule
                     :reader  meta-start-rule
                     :documentation
                     "Stores the name of the rule according which
                      expressions for this grammar should be
                      parsed.")))

(defvar *bootstrapping* t)

(defmethod parse-expression ((grammar meta-grammar-mixin) (expression t))
  (if *bootstrapping*
      (call-next-method)
      (let+ (((&accessors-r/o meta-grammar meta-start-rule) grammar)
             (meta-expression (make-meta-expression
                               meta-grammar meta-start-rule))
             ((&values success? &ign result)
              (parse meta-grammar meta-expression expression)))
        (if success?
            result
            (error "Failed to parse expression")))))
