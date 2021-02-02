;;;; base-grammar.lisp --- Grammar class provided by the grammar.base module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.base)

(defclass base-grammar (grammar:named-mixin
                        grammar::dependencies-mixin
                        grammar:rule-storage-mixin
                        grammar::meta-grammar-mixin
                        print-items:print-items-mixin)
  ((%cached? :initarg  :cached?
             :reader   cached?
             :initform t)))

(defmethod grammar:find-rule ((name symbol) (grammar base-grammar)
                              &key recursive? if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (or (call-next-method name grammar :if-does-not-exist nil)
      (when recursive?
        (some (lambda (used)
                (grammar:find-rule name used :recursive?        t
                                             :if-does-not-exist nil))
              (grammar:dependencies grammar)))))

(declaim (inline %make-context))
(defstruct (context
            (:constructor %make-context (grammar)))
  (cache   (make-hash-table :test #'equal) :read-only t)
  (grammar nil                             :read-only t))

(defmethod grammar::make-context ((grammar    base-grammar)
                                  (expression t)
                                  (input      t))
  (%make-context grammar))
