(cl:in-package #:parser.packrat.grammar.base)

(defclass base-grammar (named-mixin
                        rule-storage-mixin
                        print-items:print-items-mixin)
  ((use :initarg :use
        :reader  use))) ; TODO mixin for dependencies

(defmethod find-rule ((name symbol) (grammar base-grammar)
                      &key recursive? if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (or (call-next-method name grammar :if-does-not-exist nil)
      (when recursive?
        (some (lambda (used)
                (find-rule name used :if-does-not-exist nil))
              (use grammar)))))

(declaim (inline %make-context))
(defstruct (context
            (:constructor %make-context (grammar)))
  (cache   (make-hash-table :test #'equal) :read-only t)
  (grammar nil                             :read-only t))

(defmethod parser.packrat.grammar::make-context ((grammar    base-grammar)
                                  (expression t)
                                  (input      t))
  (%make-context grammar))
