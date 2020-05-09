;;;; conditions.lisp --- Conditions signaled by the compiler module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.compiler)

;;; Compilation

(define-condition compilation-error (more-conditions:chainable-condition
                                     error)
  ((%node :initarg :node
          :reader  node))
  (:report
   (lambda (condition stream)
     (format stream "~@<Error compiling expression ~
                     ~A~/more-conditions:maybe-print-cause/~@:>"
             (node condition)
             condition))))
