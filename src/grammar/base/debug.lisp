(cl:in-package #:parser.packrat.grammar.base)

(defvar *depth* 0)

(defvar *old-state* nil)

(defvar *debug-stream* nil)

(defvar *debug-print-level* 2)

(defun d (format-control &rest format-arguments)
  (when-let ((stream *debug-stream*))
    (format stream "~&~V@T" *depth*)
    (let ((*print-level* *debug-print-level*))
      (apply #'format stream format-control format-arguments))))

(defun d-call2 (grammar-name rule-name context position state arguments)
  (let ((state (unless (equalp state *old-state*)
                 state)))
    (d "~A::~A {~A} ~
       [~{~S~^ ~}] ~@[(~{~S~^ ~})~]~%"
       grammar-name rule-name context
       (append position state) arguments)))

(defun emit-d-call (rule-name context
                    position-variables state-variables arguments-var
                    &key grammar)
  (let ((state-variables (remove-if (rcurry #'member position-variables)
                                    state-variables)))
    `(d-call2 ',(grammar:name grammar) ',rule-name ,context
             (list ,@position-variables) (list ,@state-variables)
             ,(or arguments-var ''()))))

(defun d-return2 (rule-name context old-position old-state arguments
                 success? new-position value)
  (let ((old-state (unless (equalp old-state *old-state*)
                     old-state)))
    (d "~A {~A} ~
        [~{~S~^ ~}] ~@[(~{~S~^ ~})~] ~
        -> ~[SUCC~;FAIL~;FATAL~] [~{~S~^ ~}] ~S~%"
       rule-name context
       (append old-position old-state) arguments
       (ecase success? ((t) 0) ((nil) 1) (:fatal 2))
       new-position value)))

(defun emit-d-return (rule-name context
                      position-variables state-variables arguments-var
                      success?-var new-position-variables value-var)
  (let ((state-variables (remove-if (rcurry #'member position-variables)
                                    state-variables)))
    `(d-return2 ',rule-name ,context
               (list ,@position-variables) (list ,@state-variables)
               ,(or arguments-var ''())
               ,success?-var (list ,@new-position-variables) ,value-var)))
