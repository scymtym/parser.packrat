(cl:in-package #:parser.packrat.grammar.sexp)

;;; `structure'

(defclass structure-expression (exp:sub-expression-mixin
                                exp::value-environment-needing-mixin
                                exp:expression
                                print-items:print-items-mixin)
  ((type    :initarg  :type
            :reader   type*)
   (readers :initarg  :readers
            :type     list
            :reader   readers
            :initform '()))
  (:default-initargs
   :type (more-conditions:missing-required-initarg 'structure-expression :type)))

(defmethod shared-initialize :after ((instance   structure-expression)
                                     (slot-names t)
                                     &key)
  #+later (let+ (((&accessors-r/o readers (sub-expressions exp:sub-expressions)) instance))
    (unless (length= readers sub-expressions)
      (more-conditions:incompatible-initargs
       'structure-expression :readers readers :sub-expressions sub-expressions)
      #+no ("~@<Number of readers (~D) does not match number of ~
            sub-expressions (~D).~@:>"
            (length readers) (length expressions)))))

(defmethod print-items:print-items append ((object structure-expression)) ; TODO or unparse like optima?
  (let+ (((&accessors-r/o type* readers (sub-expressions exp:sub-expressions)) object)
         (sub-items (map 'list (lambda (reader sub-expression)
                                 (let ((sub-items (print-items:print-items
                                                   sub-expression)))
                                   (list reader (if (length= 1 sub-items)
                                                    sub-items
                                                    '((:abbrev ".."))))))
                         readers sub-expressions)))
    `((:type                 ,type*      "~A ")
      (:sub-structures       ,sub-items  "~{~{~A:~/print-items:format-print-items/~}~^ ~}"
                             ((:after :type)))
      (:sub-expression-count nil         ""))))

(defmethod bp:node-kind ((builder t) (node structure-expression))
  :structure)

(defmethod bp:node-initargs ((builder t) (node structure-expression))
  (list :readers (readers node)))

(defmethod bp:node-relations ((builder t)
                              (node    structure-expression))
  (list* '(:type . 1) (call-next-method)))

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :type))
                             (node     structure-expression))
  (values (type* node) '(:key type :evaluated? t)))

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :sub-expression))
                             (node     structure-expression))
  (values (exp:sub-expressions node)
          (map 'list (lambda (reader)
                       (list* :key reader '(:evaluated? t)))
               (readers node))))

(defmethod bp:make-node ((builder t) (kind (eql :structure)) &key readers)
  (make-instance 'structure-expression
                 :readers readers
                 :type    (make-instance 'base:constant-expression :value 'dummy))) ; TODO hack

(defmethod bp:relate ((builder  t)
                      (relation (eql :type))
                      (left     structure-expression)
                      (right    t)
                      &key)
  (reinitialize-instance left :type right))

(defmethod bp:relate ((builder  t)
                      (relation (eql :reader))
                      (left     structure-expression)
                      (right    t)
                      &key)
  (reinitialize-instance left :readers (append (readers left) (list right))))

;;; `cast-expression-mixin'

(defclass cast-expression-mixin (exp:single-sub-expression-mixin)
  ((target-type :reader  target-type
                :allocation :class)))

;;; `as-list-expression'

(exp:define-expression-class as-list (cast-expression-mixin
                                      exp::value-environment-needing-mixin)
  ((target-type :allocation :class
                :initform 'list))
  (:documentation
   "Interprets the current value as a list.

    Doing so allows processing its elements using sequence-specific
    combinators such as `seq' and `*'.

    TODO"))

;;; `rest-expression'

(exp:define-expression-class rest (exp:single-sub-expression-mixin)
  ())

;;; `as-vector-expression'

(exp:define-expression-class as-vector (cast-expression-mixin
                                        exp::value-environment-needing-mixin)
  ((target-type :allocation :class
                :initform 'vector))
  (:documentation
   "Interprets the current value as a `vector'.

    Doing so allows processing its elements using sequence-specific
    combinators such as `seq' and `*'.

    TODO"))
