(cl:in-package #:parser.packrat.grammar.base)

(defvar *depth* 0)

(defvar *old-state* nil)

(defvar *debug-stream* nil)

(defvar *debug-print-level* 2)

(defun debug-format (format-control &rest format-arguments)
  (when-let ((stream *debug-stream*))
    (format stream "~&~V@T" *depth*)
    (let ((*print-level* *debug-print-level*))
      (apply #'format stream format-control format-arguments))))

(defun debug-call (grammar-name rule-name context position state arguments)
  (let ((context-grammar-name (grammar:name (context-grammar context)))
        (state                (unless (equalp state *old-state*)
                                state)))
    (debug-format "~@[~A::~]~A~@[ {~A}~] ~
                   [~{~S~^ ~}]~@[ (~{~S~^ ~})~]~%"
                  grammar-name rule-name
                  (unless (eq context-grammar-name grammar-name)
                    context-grammar-name)
                  (append position state) arguments)))

(defun emit-debug-call (grammar-name rule-name context
                        position-variables state-variables arguments-var)
  (let ((state-variables (remove-if (rcurry #'member position-variables)
                                    state-variables)))
    `(debug-call ',grammar-name ',rule-name ,context
                 (list ,@position-variables) (list ,@state-variables)
                 ,(or arguments-var ''()))))

(defun debug-return (grammar-name rule-name context old-position old-state arguments
                     success? new-position value)
  (let ((context-grammar-name (grammar:name (context-grammar context)))
        (old-state            (unless (equalp old-state *old-state*)
                                old-state)))
    (debug-format "~@[~A::~]~A~@[ {~A}~] ~
                   [~{~S~^ ~}]~@[ (~{~S~^ ~})~] ~
                   -> ~[SUCC~;FAIL~;FATAL~] [~{~S~^ ~}] ~S~%"
                  grammar-name rule-name
                  (unless (eq context-grammar-name grammar-name)
                    context-grammar-name)
                  (append old-position old-state) arguments
                  (ecase success? ((t) 0) ((nil) 1) (:fatal 2))
                  new-position value)))

(defun emit-debug-return (grammar-name rule-name context
                          position-variables state-variables arguments-var
                          success?-var new-position-variables value-var)
  (let ((state-variables (remove-if (rcurry #'member position-variables)
                                    state-variables)))
    `(debug-return ',grammar-name ',rule-name ,context
                   (list ,@position-variables) (list ,@state-variables)
                   ,(or arguments-var ''())
                   ,success?-var (list ,@new-position-variables) ,value-var)))
