(cl:in-package #:parser.packrat.grammar.string)

;;; Expressions

(defvar *just-test-bounds*
  (make-instance 'seq::bounds-test-expression
                 :sub-expression (make-instance 'seq:sequence-expression
                                                :sub-expressions '())))

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
         grammar end-1-environment *just-test-bounds* ; TODO make an exclusive slot or separate bounds test to avoid -1 shenanigans
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
