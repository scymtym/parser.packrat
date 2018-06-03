(cl:in-package #:parser.packrat.expression)

;;; Traversal protocol

(defun walk-expression (function expression)
  (flet ((visit (recurse relation relation-args node &rest args)
           (declare (ignore args))
           (when (or (not relation) (getf relation-args :evaluated?))
             (funcall function recurse node))))
    (bp:with-builder ('list)
      (bp:walk-nodes* #'visit expression))))

(defun walk-expression/path (function expression)
  (labels ((visit (path recurse relation relation-args node &rest args)
             (declare (ignore args))
             (when (or (not relation) (getf relation-args :evaluated?))
               (let* ((path    (if-let ((key (getf relation-args :key)))
                                 (list* key path)
                                 path))
                      (recurse (rcurry recurse :function (curry #'visit path))))
                (funcall function path recurse node)))))
    (bp:with-builder ('list)
      (bp:walk-nodes* (curry #'visit ()) expression))))

;;; Value protocol

(defgeneric value (expression))

;;; Variables protocol

;; TODO remove grammar parameter
(defgeneric direct-variable-references (grammar expression &key filter)
  (:documentation
   "TODO"))

(defgeneric variable-references (grammar expression &key filter)
  (:documentation
   "TODO"))

;;; Default behavior

(defmethod direct-variable-references ((grammar t) (expression t) &key filter)
  (declare (ignore filter))
  '())

(defmethod variable-references ((grammar t) (expression t) &key filter)
  (let+ ((references '())
         ((&flet collect (recurse expression)
            (appendf references
                     (direct-variable-references
                      grammar expression :filter filter))
            (funcall recurse))))
    (walk-expression #'collect expression)
    references))
