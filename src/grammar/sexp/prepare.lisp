(cl:in-package #:parser.packrat.grammar.sexp)

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  structure-expression))
  (let* ((expression (call-next-method))
         (type       (prepare-expression grammar expression (type* expression))))
    (reinitialize-instance expression :type type)))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  as-list-expression))
  (let* ((environment    (c2mop:class-prototype
                          (c2mop:ensure-finalized
                           (find-class 'seq:list-environment))))
         (sub-expression (prepare-expression
                          grammar environment (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  rest-expression))
  (let* ((environment    (c2mop:class-prototype
                          (c2mop:ensure-finalized
                           (find-class 'env:value-environment))))
         (sub-expression (prepare-expression
                          grammar environment (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  as-vector-expression))
  (let* ((environment    (c2mop:class-prototype
                          (c2mop:ensure-finalized
                           (find-class 'seq:vector-environment))))
         (sub-expression (prepare-expression
                          grammar environment (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))
