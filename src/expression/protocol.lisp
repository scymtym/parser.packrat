(cl:in-package #:parser.packrat.expression)

;;; Traversal protocol

(defun walk-expression (function expression)
  (let ((function (ensure-function function)))
    (flet ((visit (recurse relation relation-args node &rest args)
             (declare (ignore args))
             (when (or (not relation) (getf relation-args :evaluated?))
               (funcall function recurse node))))
      (bp:with-builder ('list)
        (bp:walk-nodes* #'visit expression)))))

(defun walk-expression/path (function expression)
  (let ((function (ensure-function function)))
    (labels ((visit (path recurse relation relation-args node &rest args)
               (declare (ignore args))
               (when (or (not relation) (getf relation-args :evaluated?))
                 (let* ((path    (if-let ((key (getf relation-args :key)))
                                   (list* key path)
                                   path))
                        (recurse (rcurry recurse :function (curry #'visit path))))
                   (funcall function path recurse node)))))
      (bp:with-builder ('list)
        (bp:walk-nodes* (curry #'visit ()) expression)))))

;;; Value protocol

(defgeneric value (expression))

;;; Variables protocol

(defgeneric direct-variable-references (expression &key filter)
  (:documentation
   "Return a list of variable references in EXPRESSION.

    If EXPRESSION is a variable reference expression, a one-element
    list consisting of EXPRESSION is returned. Otherwise the empty
    list is returned."))

(defgeneric variable-references (expression &key filter)
  (:documentation
   "Return a list of variable references in ancestors of EXPRESSION.

    Elements of the returned list are variable reference expressions.

    If EXPRESSION is a variable reference expression, a one-element
    list consisting of EXPRESSION is returned."))

;;; Default behavior

(defmethod direct-variable-references ((expression t) &key filter)
  (declare (ignore filter))
  '())

(defmethod variable-references ((expression t) &key filter)
  (let+ ((references '())
         ((&flet collect (recurse expression)
            (appendf references
                     (direct-variable-references expression :filter filter))
            (funcall recurse))))
    (walk-expression #'collect expression)
    references))
