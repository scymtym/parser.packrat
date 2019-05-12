(cl:in-package #:parser.packrat.grammar.base)

(defvar *depth* 0)

(defvar *old-state* nil)

(defvar *debug-stream* nil)

(defvar *debug-print-level* 2)

(defun d (format-control &rest format-arguments)
  (when-let ((stream *debug-stream*))
            (apply #'format stream format-control format-arguments)))

(defun d-call (rule-name position state arguments)
  (let ((state (unless (equalp state *old-state*)
                 state))
        (*print-level* *debug-print-level*))
    (d "~&~V@T~A [~{~S~^ ~}] ~@[(~{~S~^ ~})~]~%"
       *depth* rule-name (append position state) arguments)))

(defun emit-d-call (rule-name position-variables state-variables arguments-var)
  (let ((state-variables (remove-if (rcurry #'member position-variables)
                                    state-variables)))
    `(d-call ',rule-name (list ,@position-variables) (list ,@state-variables)
             ,(or arguments-var ''()))))

(defun d-return (rule-name old-position old-state arguments
                 success? new-position value)
  (let ((old-state (unless (equalp old-state *old-state*)
                     old-state))
        (*print-level* *debug-print-level*))
    (d "~&~V@T~A [~{~S~^ ~}] ~@[(~{~S~^ ~})~] -> ~[SUCC~;FAIL~;FATAL~] [~{~S~^ ~}] ~S~%"
       *depth* rule-name (append old-position old-state)
       arguments
       (ecase success? ((t) 0) ((nil) 1) (:fatal 2))
       new-position value)))

(defun emit-d-return (rule-name position-variables state-variables arguments-var
                      success?-var new-position-variables value-var)
  (let ((state-variables (remove-if (rcurry #'member position-variables)
                                    state-variables)))
    `(d-return ',rule-name (list ,@position-variables) (list ,@state-variables)
               ,(or arguments-var ''())
               ,success?-var (list ,@new-position-variables) ,value-var)))
