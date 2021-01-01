;;;; environment.lisp --- Environment provided by grammar.stream module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.stream)

(defclass stream-environment (env:environment
                              seq:sequential-environment-mixin
                              print-items:print-items-mixin)
  ((%position              :initarg :position
                           :type    (or symbol array-index)
                           :reader  seq:position*)
   (%stream                :initarg :stream
                           :type    symbol
                           :reader  stream*)
   (%check-bounds-function :initarg :check-bounds-function
                           :type    symbol
                           :reader  check-bounds-function)
   (%access-function       :initarg :access-function
                           :type    symbol
                           :reader  access-function)))

(defmethod print-items:print-items append ((object stream-environment))
  `((:position ,(seq:position* object) "~A")
    (:stream   ,(stream* object)       " in ~A" ((:after :value)))))

(env:define-state-methods stream-environment
  ((seq:position* :position))
  ((stream* :stream) check-bounds-function access-function))
