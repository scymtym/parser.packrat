(cl:in-package #:parser.packrat.grammar.sequence)

;;; `sequential-environment-mixin'

(defclass sequential-environment-mixin ()
  ()
  (:documentation
   "Environment for all sequential input contexts.

    Not restricted to sequences in the sense of the `cl:sequence'
    type."))

;;; `sequence-environment'

(defclass sequence-environment (parser.packrat.environment:environment
                                sequential-environment-mixin
                                print-items:print-items-mixin)
  ((sequence :initarg  :sequence
             :type     symbol
             :reader   sequence*     ; TODO
             :initform (gensym #.(string '#:sequence)))
   (position :initarg  :position
             :type     (or array-index symbol)
             :reader   position*     ; TODO
             :initform (gensym #.(string '#:position)))
   (end      :initarg  :end
             :type     symbol
             :reader   end
             :initform (gensym #.(string '#:end))))
  (:documentation
   "Environment for sequences in the sense of the `cl:sequence' type."))

(defmethod print-items:print-items append ((object sequence-environment))
  `((:position "~A" ,(position* object))
    ((:sequence (:after  :position)) " in ~A" ,(sequence* object))
    ((:end (:after :sequence) (:before :binding-count))
     ":~A " ,(end object))))

(env:define-state-methods sequence-environment
  ((position* :position))
  ((sequence* :sequence) end))

;;; `vector-environment'

(defclass vector-environment (sequence-environment)
  ()
  (:documentation
   "Environment for `vector' inputs."))

;;; `list-environment'

(defclass list-environment (env:environment
                            sequential-environment-mixin)
  ((tail :initarg  :tail
         :type     (or list symbol)
         :reader   tail
         :initform (gensym #.(string '#:tail))))
  (:documentation
   "Environment for `list' inputs."))

(defmethod print-items:print-items append ((object list-environment))
  `((:tail "~A" ,(tail object))))

(env:define-state-methods list-environment (tail) ())
