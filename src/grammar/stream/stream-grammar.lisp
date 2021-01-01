;;;; stream-grammar.lisp --- Grammar class provided by grammar.stream module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.stream)

(defclass stream-grammar (base:base-grammar)
  ())

(defmethod grammar:default-environment ((grammar    stream-grammar)
                                        (expression t))
  (make-instance 'stream-environment :position              'position
                                     :stream                'stream
                                     :access-function       'access
                                     :check-bounds-function 'check-bounds))

(defmethod grammar:parse ((grammar    stream-grammar)
                          (expression function)
                          (input      stream))
  (let+ ((element-type (stream-element-type input))
         ;; TODO make sub-classes for element-types instead of this? A
         ;; grammar that can handle binary and character streams seems
         ;; far fetched
         (stream-cache (cond ((subtypep element-type '(unsigned-byte 8)) ; TODO slow
                              (parser.packrat.cache:make-octet-vector-stream-cache input))
                             ((subtypep element-type 'character) ; TODO slow
                              (parser.packrat.cache:make-string-stream-cache input))
                             (t
                              (error "~@<Cannot parse stream with element type ~S~@:>"
                                     element-type))))
         ((&values bounds-check access)
          (parser.packrat.cache::make-functions stream-cache))
         (context (grammar::make-context grammar expression input)))
    (funcall expression context 0 input bounds-check access)))
