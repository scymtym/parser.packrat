(cl:in-package #:parser.packrat.grammar.string)

;;; Expressions

#+no (defmethod compile-expression ((grammar      simple-string-grammar)
                               (environment  env:value-environment)
                               (expression   string-terminal-expression)
                               (success-cont function)
                               (failure-cont function))
  ())

#+no (defmethod compile-expression ((grammar      simple-string-grammar)
                               (environment  env:value-environment)
                               (expression   base:terminal-expression)
                               (success-cont function)
                               (failure-cont function))
  (let ((value (exp:value expression)))
    (typecase value
      (string
       (let+ ((length (length value))
              ((&accessors-r/o (sequence seq:sequence*) (start seq:position*))
               environment)
              (new-environment-1 (env:environment-at environment :fresh))
              (end-var-1         (seq:position* new-environment-1)))
         `(let ((,end-var-1 (+ ,start ,length -1)))
            ,(compile-expression
              grammar new-environment-1
              (lambda (new-environment)
                (let* ((position         (seq:position* new-environment))
                       (new-environment  (env:environment-at new-environment :fresh))
                       (end-var          (seq:position* new-environment)))
                  `(let ((,end-var (+ 1 ,position)))
                     (if (%string= ,sequence ,value ,start ,end-var)
                         ,(funcall success-cont new-environment)
                         ,(funcall failure-cont environment)))))
              failure-cont))))
      (t
       (call-next-method)))))

(defmethod compile-expression ((grammar      simple-string-grammar)
                               (environment  seq:vector-environment)
                               (expression   string-terminal-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ ((value (exp:value expression))
         (value-length (length value))
         ((&accessors-r/o (sequence seq:sequence*) (start seq:position*))
          environment)
         (end-1-environment (env:environment-at environment :fresh))
         (end-1-var         (seq:position* end-1-environment)))
    `(let ((,end-1-var (+ ,start ,value-length -1)))
       ,(compile-expression
         grammar end-1-environment (make-instance 'seq::bounds-test-expression ; TODO make an exclusive slot or separate bounds test to avoid -1 shenanigans
                                                  :sub-expression (make-instance 'base::ignored-expression))
         (lambda (end-1-environment)
           (let* ((end-1-var       (seq:position* end-1-environment))
                  (end-environment (env:environment-at end-1-environment :fresh))
                  (end-var         (seq:position* end-environment)))
             `(let ((,end-var (+ 1 ,end-1-var)))
                (if (%string= ,sequence ,value ,start ,end-var)
                    ,(funcall success-cont end-environment)
                    ,(funcall failure-cont environment)))))
         failure-cont))))

;;; Rules

;; TODO sequence module could define this method for `sequence-environment'?
(defmethod make-rule-lambda ((grammar     simple-string-grammar)
                             (environment seq:vector-environment)
                             (parameters  t)
                             (body        t))
  `(lambda (,parser.packrat.compiler::+context-var+ ,@(env:state-variables environment) ,@parameters)
     (declare ; (optimize (speed 3) (debug 0) (safety 0))
              (ignorable ,parser.packrat.compiler::+context-var+)
              (type ,(seq:sequence-type grammar) ,(seq:sequence* environment))
              (type ,(seq:index-type grammar)    ,(seq:position* environment) ,(seq:end environment)))
     ,@body))
