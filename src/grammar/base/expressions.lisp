(cl:in-package #:parser.packrat.grammar.base)

;; TODO print-items
(exp:define-expression-class predicate (exp:single-sub-expression-mixin
                                        exp::value-environment-needing-mixin
                                        print-items:print-items-mixin)
  (predicate))

(exp:define-expression-class anything (exp::value-environment-needing-mixin)
  ())

(exp:define-expression-class terminal (exp::value-environment-needing-mixin
                                       print-items:print-items-mixin)
  (exp:value))

(defmethod print-items:print-items append ((object terminal-expression))
  `((:value ,(exp:value object) "~S")))

;;; Combinators

(exp:define-expression-class not (exp:single-sub-expression-mixin
                                  print-items:print-items-mixin)
  ())

(exp:define-expression-class and (exp:sub-expression-mixin
                                  print-items:print-items-mixin)
  ())

(exp:define-expression-class or (exp:sub-expression-mixin
                                 print-items:print-items-mixin)
  ())

;;; Variables

(defclass variable-reference-mixin ()
  ((variable :initarg :variable
             :type    symbol
             :reader  exp:variable)
   (mode     :reader  exp:mode
             :allocation :class))
  (:default-initargs
   :variable (more-conditions:missing-required-initarg 'variable-reference-mixin :variable)))

(defmethod print-items:print-items append ((object variable-reference-mixin))
  `((:variable ,(exp:variable object))))

(defmethod bp:node-initargs ((builder t) (node variable-reference-mixin))
  (list :variable (exp:variable node) :mode (mode node)))

(defmethod exp:direct-variable-references ((grammar    t)
                                           (expression variable-reference-mixin)
                                           &key filter)
  (when (or (not filter) (funcall filter expression))
    (list expression)))

(defclass variable-write-mixin (variable-reference-mixin
                                exp::value-environment-needing-mixin)
  ((mode :allocation :class
         :initform :write)))

(exp:define-expression-class set (variable-write-mixin
                                  exp:single-sub-expression-mixin
                                  print-items:print-items-mixin)
  ())

(defmethod print-items:print-items append ((object set-expression))
  `((:arrow nil " ← " ((:after :variable)))))

(exp:define-expression-class push (variable-write-mixin
                                   exp:single-sub-expression-mixin
                                   print-items:print-items-mixin)
  ())

(defmethod print-items:print-items append ((object push-expression))
  `((:arrow nil " ←+ " ((:after :variable)))))

(defclass variable-reference (expression
                              variable-reference-mixin
                              print-items:print-items-mixin)
  ((mode :allocation :class
         :initform :read)))

;;; Transform

(exp:define-expression-class transform (exp:single-sub-expression-mixin)
  (code))

;;; Constant

(exp:define-expression-class constant (print-items:print-items-mixin)
  (exp:value))

(defmethod print-items:print-items append ((object constant-expression))
  `((:value ,(exp:value object))))

;;; `rule-invocation'

(defclass rule-invocation (exp:expression
                           print-items:print-items-mixin)
  ((rule      :initarg  :rule
              :reader   rule)
   (arguments :initarg  :arguments
              :type     list
              :accessor arguments
              :initform '()))
  (:default-initargs
   :rule (more-conditions:missing-required-initarg 'rule-invocation :rule)))

(defmethod print-items:print-items append ((object rule-invocation))
  (let ((rule      (rule object))
        (arguments (map 'list #'print-items:print-items (arguments object))))
    `((:open      nil        "(")
      (:rule      ,rule      "~A"                                     ((:after :open)))
      (:arguments ,arguments "~{ ~/print-items:format-print-items/~}" ((:after :rule)))
      (:close     nil        ")"                                      ((:after :arguments))))))

(defmethod bp:node-initargs ((builder t) (node rule-invocation))
  (list :rule (rule node)))

(defmethod bp:node-relations ((builder t) (node rule-invocation))
  '((:argument . *)))

(defmethod bp:node-relation ((builder  t)
                             (relation (eql :argument))
                             (node     rule-invocation))
  (values (arguments node)
          (load-time-value (circular-list '(:evaluated? t)))))

(defmethod bp:relate ((builder t)
                      (relation (eql :argument))
                      (left     rule-invocation)
                      (right    t)
                      &key)
  (appendf (arguments left) (list right)))
