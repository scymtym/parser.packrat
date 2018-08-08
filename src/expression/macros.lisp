(cl:in-package #:parser.packrat.expression)

(defmacro define-expression-class (name-and-options superclasses
                                   slots
                                   &rest options)
  (let+ (((name
           &key
           (class-name (symbolicate name '#:-expression))
           (kind       (make-keyword name)))
          (ensure-list name-and-options))
         (slots (map 'list #'ensure-list slots))
         ((&flet+ make-non-relation-slot ((name &rest initargs &key
                                                (initarg (make-keyword name))
                                                (reader  name)
                                                allocation
                                                &allow-other-keys))
            `((,name ,@(unless (eq allocation :class) `(:initarg ,initarg))
                     :reader ,reader
                     ,@(remove-from-plist initargs :initarg :reader)))))
         ((&flet+ make-default-initarg ((name &key
                                              (initarg   (make-keyword name))
                                              (initform nil iniform-supplied?)
                                              allocation
                                              &allow-other-keys))
            (declare (ignore initform))
            (unless (or iniform-supplied? (eq allocation :class))
              `(,initarg (more-conditions:missing-required-initarg
                          ',class-name ,initarg)))))
         ((&flet+ make-node-initarg ((name &key
                                           (initarg   (make-keyword name))
                                           allocation
                                           &allow-other-keys))
            (unless (eq allocation :class)
              `(,initarg (,name node)))))
         ((&flet+ make-required-keyword-argument
              ((name &key
                     (initarg   (make-keyword name))
                     (initform  nil                 initform-supplied?)
                     allocation
                     &allow-other-keys))
            (declare (ignore initform))
            (unless (or initform-supplied? (eq allocation :class))
              `((,name (more-conditions:missing-required-initarg
                        ',class-name ,initarg))))))
         ((&flet+ make-ignore ((name &key
                                        ; (initarg   (make-keyword name))
                                     (initform  nil initform-supplied?)
                                     allocation
                                     &allow-other-keys))
            (declare (ignore initform))
            (unless (or initform-supplied? (eq allocation :class))
              `(,name)))))
    `(progn
       (defclass ,class-name (,@superclasses
                              expression)
         ,(mappend #'make-non-relation-slot slots)
         (:default-initargs
          ,@(mappend #'make-default-initarg slots))
         ,@options)

       (defmethod bp:node-kind ((builder t) (node ,class-name))
         ,kind)

       ,@(when-let ((initargs (mappend #'make-node-initarg slots)))
           `((defmethod bp:node-initargs ((builder t) (node ,class-name))
               (list* ,@initargs (when (next-method-p) (call-next-method))))))

       (defmethod bp:make-node
           ((builder t)
            (kind    (eql ,kind))
            &rest initargs &key
                           ,@(mappend #'make-required-keyword-argument slots))
         (declare (ignore ,@(mappend #'make-ignore slots)))
         (apply #'make-instance ',class-name initargs ; ,@(mappend #'make-initarg slots)
                )))))
