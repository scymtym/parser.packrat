(cl:in-package #:parser.packrat.environment)

(defmacro define-state-methods (class position-specs state-specs)
  (let+ ((all-specs (append position-specs state-specs))
         ((&flet role (spec)
            (if (consp spec)
                (second spec)
                (make-keyword spec))))
         ((&flet accessor (spec)
            (if (consp spec)
                (first spec)
                spec)))
         ((&flet make-list-elements (object-var specs)
            (loop :for spec :in specs
               :collect `(,(accessor spec) ,object-var))))
         ((&flet make-plist-elements (object-var specs)
            (loop :for spec :in specs
               :collect (role spec)
               :collect `(,(accessor spec) ,object-var)))))
    `(progn
       (defmethod state-variables ((environment ,class))
         (list ,@(make-list-elements 'environment all-specs)))

       (defmethod state-variables/plist ((environment ,class))
         (list ,@(make-plist-elements 'environment all-specs)))

       (defmethod position-variables ((environment ,class))
         (list ,@(make-list-elements 'environment position-specs)))

       (defmethod position-variables/plist ((environment ,class))
         (list ,@(make-plist-elements 'environment position-specs))))))
