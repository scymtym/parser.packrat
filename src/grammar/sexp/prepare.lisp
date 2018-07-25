(cl:in-package #:parser.packrat.grammar.sexp)

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  as-list-expression))
  (let ((sub-expression (prepare-expression grammar
                                            (c2mop:class-prototype
                                             (c2mop:ensure-finalized
                                              (find-class 'seq:list-environment)))
                                            (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  rest-expression))
  (let ((sub-expression (prepare-expression grammar
                                            (c2mop:class-prototype
                                             (c2mop:ensure-finalized
                                              (find-class 'env:value-environment)))
                                            (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))

(defmethod prepare-expression ((grammar     t)
                               (environment t)
                               (expression  as-vector-expression))
  (let ((sub-expression (prepare-expression grammar
                                            (c2mop:class-prototype
                                             (c2mop:ensure-finalized
                                              (find-class 'seq:vector-environment)))
                                            (exp:sub-expression expression))))
    (reinitialize-instance expression :sub-expression sub-expression)))
