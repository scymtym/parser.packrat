;;;; prepare.lisp --- Expression preparation provided by the grammar.string module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.string)

(defmethod c:prepare-expression :around ((grammar     simple-string-grammar)
                                         (environment seq:sequential-environment-mixin)
                                         (expression  base:terminal-expression))
  (typecase (exp:value expression)
    (string
     (change-class expression 'string-terminal-expression))
    (t
     (call-next-method))))
