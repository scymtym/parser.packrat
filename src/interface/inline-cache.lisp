(cl:in-package #:parser.packrat)

(defclass inline-cache (print-items:print-items-mixin
                        c2mop:funcallable-standard-object)
  ((%expressions :accessor expressions
                 :initform '()
                 :documentation
                 "A two-level alist structure of the form

                    ((GRAMMAR . ((EXPRESSION . (GUARD-FORM . FUNCTION)) …) …)

                  which maps (GRAMMAR EXPRESSION) pairs to already
                  compiled functions and the corresponding guard
                  forms."))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance inline-cache) &key)
  (c2mop:set-funcallable-instance-function
   instance (curry #'initial-function instance)))

(defmethod find-entry ((grammar t) (expression t) (instance inline-cache))
  (assoc-value (assoc-value (expressions instance) grammar :test #'eq)
               expression :test #'equal))

(defmethod (setf find-entry) ((new-value  t)
                              (grammar    t)
                              (expression t)
                              (instance   inline-cache))
  (setf (assoc-value (assoc-value (expressions instance) grammar :test #'eq)
                     expression :test #'equal)
        new-value))

(defmethod entry-count ((instance inline-cache))
  (reduce #'+ (expressions instance) :key (compose #'length #'cdr)))

(defmethod print-items:print-items append ((object inline-cache))
  `((:entry-count ,(entry-count object) "(~:D)")))

(defun initial-function (instance grammar expression input) ;; TODO locking
  (let+ (((&values key guard function) (make-rule-function grammar expression)))
    (setf (find-entry grammar key instance) (cons guard function)))

  ;; Compile EXPRESSION into a function and replace the instance
  ;; function (which is currently this function).
  (c2mop:set-funcallable-instance-function
   instance (make-inline-cache-function instance (expressions instance) t))
  ;; Call INSTANCE again which will use the new instance function.
  (funcall instance grammar expression input))

;;; Dispatch function

(defun make-inline-cache-form (instance rule-functions extend?)
  (let+ (((&flet+ make-rule-clause ((&ign . (guard . function)))
            `(,guard
              (funcall ,function expression input))))
         ((&flet+ make-grammar-clause ((grammar . rule-functions))
            `((eq grammar ,grammar)
              (cond ,@(map 'list #'make-rule-clause rule-functions)
                    (t (miss)))))))
    `(lambda (grammar expression input)
       (flet ((miss ()
                ,(if extend?
                     ;; Extend
                     `(initial-function ,instance grammar expression input)
                     ;; Slow path
                     `(grammar:parse grammar expression input))))
         (cond ,@(map 'list #'make-grammar-clause rule-functions)
               (t (miss)))))))

(defun make-inline-cache-function (instance expressions extend?)
  (compile nil (make-inline-cache-form instance expressions extend?)))

;;; Rule functions

(defun make-rule-lambda (grammar expression)
  ;; TODO we don't do anything with free variable at the moment
  (let* ((environment    (grammar:default-environment grammar expression))
         (free-variables (map 'list #'exp:variable
                              (exp:variable-references
                               expression :filter (of-type 'base:variable-reference-expression)))))
    (values (parser.packrat.compiler:compile-rule
             grammar free-variables expression)
            free-variables)))

(defun make-rule-form/expression (grammar expression/parsed expression/raw)
  (let ((rule-lambda (make-rule-lambda grammar expression/parsed)))
    (values expression/raw
            `(expressions-compatible? expression ',expression/raw)
            `(lambda (expression input)
               (declare (ignore expression))
               (grammar:parse ,grammar ,rule-lambda input)))))

(defun make-bindings (arguments-var variables)
  (loop :for tail = `(tail ,arguments-var) :then `(tail (rest tail))
        :for variable :in variables
        :collect tail
        :collect `(,variable (first tail))
        :collect `(,variable (if (and (consp ,variable) (eq (first ,variable) 'quote))
                                 (second ,variable)
                                 ,variable))))

(defun make-rule-form/invocation (grammar expression/parsed expression/raw)
  (declare (ignore expression/raw))
  (let+ ((rule           (base:rule expression/parsed))
         (count          (length (base:arguments expression/parsed)))
         (names          (map-into (make-list count) #'gensym))
         (new-expression (make-instance 'base:rule-invocation-expression
                                        :rule      rule
                                        :arguments (map 'list (lambda (variable)
                                                                (make-instance 'base:variable-reference-expression
                                                                               :variable variable))
                                                        names)))
         ((&ign rule-lambda-list &rest rule-body)
          (make-rule-lambda grammar new-expression)))
    (values
     rule
     `(expression-is-invocation? expression ',rule)
     `(lambda (expression input)
        ,@(unless names `((declare (ignore expression))))
        (let* (,@(when names
                   `((arguments (rest expression))))
               ,@(make-bindings 'arguments names))
          (grammar:parse ,grammar (lambda ,(remove-if (rcurry #'member names) rule-lambda-list) ,@rule-body) input))))))

(defun simple-invocation? (expression)
  (and (typep expression 'base:rule-invocation-expression)
       (every (of-type '(or base:variable-reference-expression
                         base:constant-expression))
              (base:arguments expression))))

(defun make-rule-form (grammar expression)
  (let ((expression/parsed (grammar:parse-expression grammar expression)))
    (if (simple-invocation? expression/parsed)
        (make-rule-form/invocation grammar expression/parsed expression)
        (make-rule-form/expression grammar expression/parsed expression))))

(defun make-rule-function (grammar expression)
  (let+ (((&values key guard lambda-expression)
          (make-rule-form grammar expression)))
    (values key guard (compile nil lambda-expression))))

;;; Runtime

(declaim (inline expressions-compatible?
                 expression-is-invocation?))

(defun expressions-compatible? (new-expression old-expression)
  (equal old-expression new-expression))

(defun expression-is-invocation? (new-expression rule)
  (and (consp new-expression)
       (let ((new-rule (first new-expression)))
         (etypecase new-rule
           (symbol (eq new-rule rule))
           (cons   (eq (first new-rule) rule))))))
