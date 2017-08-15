(cl:in-package #:parser.packrat.expression)

(defmacro define-expression-class (name-and-options superclasses
                                   slots
                                   &rest options)
  (let+ (((name
           &key
           (class-name (symbolicate name '#:-expression))
           (kind       (make-keyword name)))
          (ensure-list name-and-options))
         ((&flet make-non-relation-slot (name)
            (let ((initarg (make-keyword name)))
              `(,name :initarg ,initarg :reader ,name))))
         ((&flet make-default-initarg (name)
            (let ((initarg (make-keyword name)))
              `(,initarg (more-conditions:missing-required-initarg ',class-name ,initarg)))))
         ((&flet make-node-initarg (name)
            (let ((initarg (make-keyword name)))
              `(,initarg (,name node)))))
         ((&flet make-required-keyword-argument (name)
            `(,name (more-conditions:missing-required-initarg
                     ',class-name ,(make-keyword name))))))
    `(progn
       (defclass ,class-name (expression
                              ,@superclasses)
         ,(map 'list #'make-non-relation-slot slots)
         (:default-initargs
          ,@(mappend #'make-default-initarg slots))
         ,@options)

       (defmethod bp:node-kind ((builder t)
                                (node    ,class-name))
         ,kind)

       ,@(when slots
           `((defmethod bp:node-initargs ((builder t) (node ,class-name))
               (list ,@(mappend #'make-node-initarg slots)))))

       (defmethod bp:make-node
           ((builder t)
            (kind    (eql ,kind))
            &key
            ,@(mapcar #'make-required-keyword-argument slots))
         (make-instance ',class-name ,@(mappend (lambda (name) `(,(make-keyword name) ,name)) slots))))))
