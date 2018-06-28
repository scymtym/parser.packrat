(cl:in-package #:parser.packrat) ; TODO may want to turn this into protocol.lisp or similar

;;; Goal:
;;;
;;; (defun f (x y input)
;;;   (parse `(foo ,x ,y) input)
;;;
;;; (defun f (x y input)
;;;   ((lambda (#:input)
;;;      ...)
;;;    input))

(defclass inline-cache (c2mop:funcallable-standard-object)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance inline-cache) &key)
  (c2mop:set-funcallable-instance-function
   instance (curry #'initial-function instance)))

(defun initial-function (instance expression input)
  (c2mop:set-funcallable-instance-function
   instance (compile nil `(lambda (expression input)
                            (parser.packrat.compiler:compile-expression
                             ))))
  (funcall instance expression input))

(defun parse (expression input &key (grammar *grammar*))
  (parser.packrat.grammar:parse grammar expression input))

(define-compiler-macro parse (&whole whole
                              expression input
                              &key (grammar nil grammar-supplied?))
  (let* ((grammar-constant    (cond
                                ((not grammar-supplied?)
                                 (when (boundp '*grammar*) ; TODO wrong
                                   *grammar*))
                                ((constantp grammar)
                                 (eval grammar))))
         (grammar             (typecase grammar-constant
                                (null)
                                (symbol ; TODO grammar-designator
                                 (parser.packrat.grammar:find-grammar
                                  grammar-constant :if-does-not-exist nil)) ; TODO warning
                                (t
                                 grammar-constant)))
         (expression-constant (when (constantp expression)
                                (eval expression)))
         (expression/parsed   (when (and grammar expression-constant)
                                (parser.packrat.grammar:parse-expression
                                 grammar expression-constant)))
         (free-variables      (when (and grammar expression/parsed)
                                (parser.packrat.expression:variable-references
                                 grammar expression/parsed))))
    (print (list grammar expression-constant expression/parsed free-variables))
    whole))
