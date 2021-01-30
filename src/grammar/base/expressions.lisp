(cl:in-package #:parser.packrat.grammar.base)

(exp:define-expression-class predicate (exp:single-sub-expression-mixin
                                        exp::value-environment-needing-mixin
                                        print-items:print-items-mixin)
  (predicate))

(defmethod print-items:print-items append ((object predicate-expression))
  (let ((predicate (ensure-list (predicate object))))
    `((:open "(")
      ((:predicate (:after :open) (:before :sub-expression))
       "~{~A~^ ~} " ,predicate)
      ((:close (:after  :sub-expression)) ")"))))

(exp:define-expression-class anything (exp::value-environment-needing-mixin)
  ())

(exp:define-expression-class terminal (exp::value-environment-needing-mixin
                                       print-items:print-items-mixin)
  (exp:value))

(defmethod print-items:print-items append ((object terminal-expression))
  `((:value "~S" ,(exp:value object))))

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

(exp:define-expression-class compose (exp:sub-expression-mixin
                                      print-items:print-items-mixin)
  ())

(exp:define-expression-class must (exp:single-sub-expression-mixin
                                   print-items:print-items-mixin)
  (message))

;;; Constant

(exp:define-expression-class constant (print-items:print-items-mixin)
  (exp:value))

(defmethod print-items:print-items append ((object constant-expression))
  `((:value "~A" ,(exp:value object))))

;;; Variables

(defclass variable-reference-mixin ()
  ((%variable :initarg :variable
              :type    symbol
              :reader  exp:variable)
   (%mode     :reader  exp:mode
              :allocation :class))
  (:default-initargs
   :variable (more-conditions:missing-required-initarg 'variable-reference-mixin :variable)))

(defmethod print-items:print-items append ((object variable-reference-mixin))
  `((:variable ,(exp:variable object))))

(defmethod bp:node-initargs ((builder t) (node variable-reference-mixin))
  (list :variable (exp:variable node) :mode (exp:mode node)))

(defmethod exp:direct-variable-references ((expression variable-reference-mixin)
                                           &key filter)
  (when (or (not filter) (funcall filter expression))
    (list expression)))

(defclass variable-write-mixin (variable-reference-mixin
                                exp:single-sub-expression-mixin
                                ; exp::value-environment-needing-mixin
                                )
  ((%mode :allocation :class
          :initform :write)))

(exp:define-expression-class set (variable-write-mixin
                                  print-items:print-items-mixin)
  ())

(defmethod print-items:print-items append ((object set-expression))
  `(((:arrow (:after :variable) (:before :sub-expression)) " ← ")))

(exp:define-expression-class push (variable-write-mixin
                                   print-items:print-items-mixin)
  ())

(defmethod print-items:print-items append ((object push-expression))
  `(((:arrow (:after :variable) (:before :sub-expression)) " ←/push ")))

(exp:define-expression-class append (variable-write-mixin
                                     print-items:print-items-mixin)
  ((%tail-reference :accessor   %tail-reference
                    :initform nil)))

(defmethod exp:direct-variable-references ((expression append-expression)
                                           &key filter)
  (append
   (let+ (((&flet make-tail-reference ()
             (let ((variable (exp:variable expression)))
               (make-instance 'variable-write-mixin
                              :variable (let ((*package* (symbol-package variable)))
                                          (symbolicate variable '#:-tail))))))
          (tail-reference (or (%tail-reference expression)
                              (setf (%tail-reference expression)
                                    (make-tail-reference)))))
     (exp:direct-variable-references tail-reference :filter filter))
   (call-next-method)))

(defmethod print-items:print-items append ((object append-expression))
  `(((:arrow (:after :variable) (:before :sub-expression)) " ←/append ")))

(exp:define-expression-class variable-reference (variable-reference-mixin
                                                 print-items:print-items-mixin)
  ((mode :allocation :class
         :initform :read)))

(exp:define-expression-class variable-same (variable-write-mixin ; TODO is this a write?
                                            print-items:print-items-mixin)
  ((mode :allocation :class
         :initform :same)))

(defmethod print-items:print-items append ((object variable-same-expression))
  `(((:relation (:after :variable) (:before :sub-expression)) " ≡")))

;;; Position

(exp:define-expression-class position ()
  ())

;;; Transform

(exp:define-expression-class transform (exp:single-sub-expression-mixin)
  (code))

;;; `rule-invocation'

(defclass rule-invocation-base (exp:sub-expression-mixin
                                print-items:print-items-mixin)
  ((sub-expressions :initarg  :arguments
                    :accessor arguments)))

(defmethod print-items:print-items append ((object rule-invocation-base))
  (let ((arguments (map 'list #'print-items:print-items (arguments object))))
    `((:sub-expression-count            "") ; suppress inherited
      (:open                            "(")
      ((:arguments (:after :open))      "~{ ~/print-items:format-print-items/~}" ,arguments)
      ((:close     (:after :arguments)) ")"))))

(exp:define-expression-class rule-invocation (rule-invocation-base)
  ((grammar           :initform nil
                      :documentation
                      "Name of the grammar ")
   (rule              :documentation
                      "Name of the rule")
   (switch-to-grammar :initform nil ; TODO we could switch by default if GRAMMAR is not used?
                      :documentation
                      "Name of grammar to switch to")))

(defmethod print-items:print-items append ((object rule-invocation-expression))
  `(((:rule (:after :open) (:before :arguments)) "~A" ,(rule object))))

(exp:define-expression-class next-rule-invocation (rule-invocation-base)
  ())

(defmethod print-items:print-items append ((object next-rule-invocation-expression))
  `(((:rule (:after :open) (:before :arguments)) "next-rule")))
