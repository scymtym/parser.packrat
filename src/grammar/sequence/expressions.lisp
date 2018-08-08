(cl:in-package #:parser.packrat.grammar.sequence)

;;; Internal concepts

(exp:define-expression-class bounds-test (exp:single-sub-expression-mixin)
  ())

(exp:define-expression-class element-access (exp:single-sub-expression-mixin)
  ())

(exp:define-expression-class advance (exp:single-sub-expression-mixin)
  ((amount :initarg  :amount
           :type     positive-integer
           :reader   amount
           :initform 1)))

;;; repetition

(defclass repetition-expression (exp:single-sub-expression-mixin
                                 print-items:print-items-mixin)
  ((min-repetitions :initarg  :min-repetitions
                    :reader   min-repetitions
                    :initform nil)
   (max-repetitions :initarg  :max-repetitions
                    :reader   max-repetitions
                    :initform nil)))

(defmethod print-items:print-items append ((object repetition-expression))
  `((:min-repetitions ,(min-repetitions object) "~:[0~;~:*~D~]")
    (:max-repetitions ,(max-repetitions object) "..~:[*~;~:*~D~]"
                      ((:after :min-repetitions)))))

(defmethod bp:node-kind ((builder t)
                         (node    repetition-expression))
  :repetition)

(defmethod bp:node-relations ((builder t)
                              (node    repetition-expression))
  (append '((:min-repetitions . bp:?) (:max-repetitions . bp:?))
          (call-next-method)))

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :min-repetitions))
                             (node     repetition-expression))
  (values (min-repetitions node) '(:evaluated? t)))

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :max-repetitions))
                             (node     repetition-expression))
  (values (max-repetitions node) '(:evaluated? t)))

(defmethod bp:make-node ((builder t) (kind (eql :repetition)) &key)
  (make-instance 'repetition-expression))

(defmethod bp:relate ((builder t)
                      (relation (eql :min-repetitions))
                      (left     repetition-expression)
                      (right    t)
                      &key)
  (reinitialize-instance left :min-repetitions right))

(defmethod bp:relate ((builder t)
                      (relation (eql :max-repetitions))
                      (left     repetition-expression)
                      (right    t)
                      &key)
  (reinitialize-instance left :max-repetitions right))

;;; sequence

(exp:define-expression-class sequence (exp:sub-expression-mixin
                                       print-items:print-items-mixin)
  ())

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :sub-expression))
                             (node     sequence-expression))
  (let+ (((&values nodes relation-args) (call-next-method)))
    (values nodes (loop :for args :in relation-args
                        :for i :from 0 :below (length nodes)
                        :collect (list* :key i args)))))
