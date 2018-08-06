(cl:in-package #:parser.packrat.grammar.tree)

#+old (defun depth-first-traverse (tree)
  (labels ((visit-node (sibling-cont node cont)
             (funcall cont
                      node
                      (lambda (cont)
                        (visit-children sibling-cont (children node) cont))))
           (visit-children (sibling-cont children cont)
             (if-let ((first (first children)))
               (visit-node (lambda (cont)
                             (visit-children sibling-cont (rest children) cont))
                           first cont)
               (funcall sibling-cont cont))))
    (curry #'visit-node (lambda (cont) (declare (ignore cont))) tree)))

(defun depth-first-traverse (tree)
  (labels ((visit-node (sibling-cont node)
             (values node
                     (lambda ()
                       (visit-children sibling-cont (children node)))))
           (visit-children (sibling-cont children)
             (if-let ((first (first children)))
               (visit-node (lambda ()
                             (visit-children sibling-cont (rest children)))
                           first)
               (funcall sibling-cont))))
    (curry #'visit-node (lambda () nil) tree)))

(defmethod children ((node t))
  '())

(defmethod children ((node cons))
  (list (car node) (cdr node)))

(loop :for (node generator) = (list nil (depth-first-traverse '(1 (3 (4) (5 (6) (7))) (2))))
      :then (multiple-value-list (funcall generator))
      :while generator
      :do (print node))
