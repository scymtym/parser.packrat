(cl:in-package #:parser.packrat.grammar.sequence)

;;; TODO these methods should not be in the sequence grammar

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  exp:expression))
  expression)

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  exp:single-sub-expression-mixin))
  ;; TODO use builder protocol to copy
  (let ((sub-expression (prepare-expression
                         grammar environment (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  exp:sub-expression-mixin))
  (let ((sub-expressions (map 'list (curry #'prepare-expression
                                           grammar environment)
                              (exp:sub-expressions expression))))
    (reinitialize-instance expression
                           :sub-expressions (remove nil sub-expressions))))

;; TODO this is basically a different phase, optimizing repetitions
(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  repetition-expression))
  (let+ (((&flet maybe-value (expression) ; TODO generalize and factor out
            (when (typep expression 'base:constant-expression)
              (exp:value expression))))
         (min (maybe-value (min-repetitions expression)))
         (max (maybe-value (max-repetitions expression))))
    (cond
      #+no ((equal min 0)
            (error "~@<In ~A, minimum repetition count is 0.~@:>"
                   expression))
      ((and (equal min 1) (equal max 1))
       (prepare-expression grammar environment (exp:sub-expression expression)))
      (t
       (call-next-method)))))

(defmethod prepare-expression :around ((grammar     t)
                                       (environment sequential-environment-mixin)
                                       (expression  base:rule-invocation-expression))
  ;; If the rule invocation needs a value environment, establish it.
  (if (parser.packrat.compiler:validate-invocation
       grammar environment expression)
      (call-next-method)
      (wrap-value-needing-expression grammar expression)))

(defmethod prepare-expression :around ((grammar     t)
                                       (environment sequential-environment-mixin)
                                       (expression  exp::value-environment-needing-mixin))
                                        ; (format t "Adding element access for ~A in ~A~%" expression environment)
  (wrap-value-needing-expression grammar expression))

;;; TODO this is wrong. Instead of wrapping the direct sub-expression,
;;; we should traverse all ancestors and wrap those as appropriate.
;;; "As appropriate" probably means we
#+no (defmethod prepare-expression :around ((grammar     t)
                                       (environment sequential-environment-mixin)
                                       (expression  parser.packrat.grammar.base::variable-write-mixin))
  ; (format t "Adding element access for ~A in ~A~%" expression environment)
  (call-next-method)
  #+maybe (let ((sub-expression (exp:sub-expression expression)))
    (if (typep sub-expression 'base:rule-invocation-expression)
        (call-next-value)
        (reinitialize-instance expression
                               :sub-expression (wrap-value-needing-expression
                                                grammar sub-expression)))))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  base::compose-expression))
  (let ((sub-expressions   (exp:sub-expressions expression))
        (value-environment (c2mop:class-prototype
                            (find-class 'env:value-environment))))
    (reinitialize-instance
     expression
     :sub-expressions (list* (prepare-expression grammar environment (first sub-expressions))
                             (map 'list (curry #'prepare-expression grammar value-environment)
                                  (rest sub-expressions))))))

(defun wrap-value-needing-expression (grammar expression)
  (let ((environment (c2mop:class-prototype
                      (find-class 'env:value-environment))))
    (reduce (lambda (wrap-expression-class expression)
              (make-instance wrap-expression-class :sub-expression expression))
            '(bounds-test-expression advance-expression element-access-expression)
            :initial-value (prepare-expression grammar environment expression)
            :from-end      t)))
