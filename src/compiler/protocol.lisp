(cl:in-package #:parser.packrat.compiler)

;;; Expression compilation protocol

(defgeneric prepare-expression (grammar environment expression)
  (:documentation
   "A \"source transform\" that prepares EXPRESSION for compilation."))

(defgeneric compile-expression (grammar environment expression
                                success-cont failure-cont)
  (:documentation
   "TODO"))

;;; Rule compilation protocol

(defgeneric compile-rule (grammar parameters expression &key environment)
  (:documentation
   "TODO"))

(defgeneric compile-rule-using-environment
    (grammar parameters environment expression)
  (:documentation
   "TODO"))

;;; Default behavior

(defconstant +context-var+ 'context)

(defmethod compile-rule-using-environment ((grammar     t)
                                           (parameters  list)
                                           (environment t)
                                           (expression  t))
  ;; TODO the variable write stuff is too simplistic
  (let+ ((environment        (apply #'env:environment-binding environment
                                    (mappend (lambda (parameter)
                                               (list parameter (cons parameter :parameter)))
                                             parameters)))
         (expression (prepare-expression grammar environment expression))

         ((&flet references-with-mode (mode)
            (exp:variable-references grammar expression
                                     :filter (lambda (node)
                                               (and (not (env:lookup (exp:variable node) environment)) ; TODO hack
                                                    (eq (exp:mode node) mode))))))
         (writes             (references-with-mode :write))
         (assigned-variables (remove-duplicates writes :key #'exp:variable))
         (assigned-names     (mapcar #'exp:variable assigned-variables))

         ((&flet make-return-cont (success)
            (lambda (environment)
              `(values ,success ,@(env:position-variables environment))))))
    `(lambda (,+context-var+ ,@(env:state-variables environment) ,@parameters)
       ;; TODO type declarations
       ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
       (declare (optimize (speed 1) (debug 1) (safety 1)))
       (declare (ignorable ,+context-var+))
       (let (,@assigned-names)
         ,(compile-expression
           grammar environment expression
           (make-return-cont t) (make-return-cont nil))))))
