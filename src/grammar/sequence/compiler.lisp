(cl:in-package #:parser.packrat.grammar.sequence)

;;; Default for `sequential-environment-mixin'

(defmethod compile-expression ((grammar      t)
                               (environment  sequential-environment-mixin)
                               (expression   advance-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&accessors-r/o amount) expression)
         (position (position* environment)))
    (compile-expression
     grammar environment (exp:sub-expression expression)
     (lambda (element-environment)
       (declare (ignore element-environment))
       (typecase position
         #+no (array-index
               (let ((new-environment (env:environment-at
                                       environment (list :position (+ position amount)))))
                 (funcall success-cont new-environment)))
         (t
          (let* ((new-environment (env:environment-at environment :fresh))
                 (new-position    (position* new-environment)))
            `(let ((,new-position ,(or nil #+no to `(+ ,position ,amount))))
               ,(funcall success-cont new-environment))))))
     failure-cont)))

;;; Bounds test, element access and advance rules for `sequence-environment'

(defmethod compile-expression ((grammar      t)
                               (environment  sequence-environment)
                               (expression   bounds-test-expression)
                               (success-cont function)
                               (failure-cont function))
  `(if (< ,(position* environment) ,(end environment))
       ,(compile-expression
         grammar environment (exp:sub-expression expression)
         success-cont failure-cont)
       ,(funcall failure-cont environment)))

(defmethod compile-expression ((grammar      t)
                               (environment  sequence-environment)
                               (expression   element-access-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&with-gensyms element))
         (new-environment (env:environment-at environment (list :value element)
                                              :class 'env:value-environment
                                              :state '())))
    `(let ((,element (elt ,(sequence* environment) ,(position* environment))))
       ,(compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))

;;; Bounds test, element access and advance rules for `list-environment'

(defmethod compile-expression ((grammar      t)
                               (environment  list-environment)
                               (expression   bounds-test-expression)
                               (success-cont function)
                               (failure-cont function))
  `(if (null ,(tail environment))
       ,(funcall failure-cont environment)
       ,(compile-expression
         grammar environment (exp:sub-expression expression)
         success-cont failure-cont)))

(defmethod compile-expression ((grammar      t)
                               (environment  list-environment)
                               (expression   element-access-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&with-gensyms element))
         (new-environment (env:environment-at environment (list :value element)
                                              :class 'env:value-environment
                                              :state '())))
    `(let ((,element (car ,(tail environment))))
       ,(compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))

(defmethod compile-expression ((grammar      t)
                               (environment  list-environment)
                               (expression   advance-expression)
                               (success-cont function)
                               (failure-cont function))
  (let* ((amount          (amount expression))
         (tail            (tail environment))
         (new-environment (env:environment-at environment :fresh))
         (new-tail        (tail new-environment)))
    (compile-expression
     grammar environment (exp:sub-expression expression)
     (lambda (element-environment)
       (declare (ignore element-environment))
       `(let ((,new-tail ,(or nil #+maybe-later to
                                  (case amount
                                    (1 `(cdr ,tail))
                                    (2 `(cddr ,tail))
                                    (t `(nthcdr ,amount ,tail))))))
          ,(funcall success-cont new-environment)))
     failure-cont)))

;;; Bounds test, element access and advance rules for `vector-environment'
;;;
;;; Only element access is different compared to the superclass
;;; `sequence-environment'.

(defmethod compile-expression ((grammar      t)
                               (environment  vector-environment)
                               (expression   element-access-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ (((&with-gensyms element))
         (new-environment (env:environment-at environment (list :value element)
                                              :class 'env:value-environment
                                              :state '())))
    `(let ((,element
             (locally
                 #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                 (aref ,(sequence* environment) ,(position* environment)))))
       ,(compile-expression
         grammar new-environment (exp:sub-expression expression)
         success-cont failure-cont))))

;;; Repetition and sequence expressions

;; TODO (* EXPRESSION 0 1) -> (if C(EXPRESSION) SUCCESS SUCCESS), no labels
(defmethod compile-expression ((grammar      sequential-grammar-mixin)
                               (environment  sequential-environment-mixin)
                               (expression   repetition-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ ((min    (min-repetitions expression)) ; TODO &accessors-r/o (exp:sub-expression expression)
         (max    (max-repetitions expression))
         (max=1? (and (typep max 'parser.packrat.grammar.base:constant-expression) ; TODO
                      (equal 1 (exp:value max))))
         (count? (or min (and max (not max=1?))))
         ((&flet compile-constraint (expression environment success-cont)
            (if expression
                (compile-expression
                 grammar environment expression
                 success-cont failure-cont)
                (funcall success-cont environment))))
         ((&with-gensyms repeat done/check done/no-check)))
    (compile-constraint
     min environment
     (lambda (min-new-environment)
       (compile-constraint
        max min-new-environment
        (lambda (max-new-environment)
          (let+ ((recursion-environment (env:environment-at environment :fresh)) ; TODO should somehow use max-new-environment as parent
                 ((&flet recurse (environment)
                    `(,repeat
                      ,@(env:state-variables environment) ,@(when count? `((1+ count))))))
                 ((&flet done (environment check?)
                    `(,(if (and min check?) done/check done/no-check)
                       ,@(env:state-variables environment)
                       ,@(when (and min check?) `(count))))))
            `(labels ((,repeat (,@(env:state-variables recursion-environment)
                                ,@(when count? '(count)))
                        ; (declare (type array-index position)) ; TODO depends on the sequence
                        ,@(when count? `((declare (type array-index count))))
                        ,(compile-expression
                          grammar recursion-environment (exp:sub-expression expression)
                          (cond
                            ((not max)
                             #'recurse)
                            (max=1?     ; TODO
                             (rcurry #'done nil))
                            (t
                             (lambda (new-environment)
                               `(if (< count ,(env:value max-new-environment)) ; TODO emit a local function for this as well?
                                    ,(recurse new-environment)
                                    ,(done new-environment nil)))))
                          (lambda (failure-environment)
                            (declare (ignore failure-environment))
                            (done recursion-environment t))))
                      (,done/no-check (,@(env:state-variables recursion-environment))
                        ,(funcall success-cont recursion-environment))
                      ,@(when min
                          `((,done/check (,@(env:state-variables recursion-environment)
                                          count)
                              ; (declare (type array-index position))
                              (declare (type array-index count))
                              (if (>= count ,(env:value min-new-environment))
                                  (,done/no-check ,@(env:state-variables recursion-environment))
                                  ,(funcall failure-cont recursion-environment))))))
               (,repeat ,@(env:state-variables environment  #+TODO max-new-environment) ,@(when count? '(0)))))))))))

(defmethod compile-expression ((grammar      sequential-grammar-mixin)
                               (environment  sequential-environment-mixin)
                               (expression   sequence-expression)
                               (success-cont function)
                               (failure-cont function))
  (let+ ((expressions (exp:sub-expressions expression))
         (names       (map-into (make-list (length expressions)) #'gensym))
         ((&labels+ element ((&optional expression &rest rest-expressions)
                             (&optional name next-name &rest rest-names)
                             environment)
            (when expression
              (let ((environment      (env:environment-at environment :fresh))
                    (next-environment))
                (list*
                 `(,name (,@(env:position-variables environment))
                     ,(compile-expression
                       grammar (print environment) (print expression)
                       (lambda (new-environment)
                         (setf next-environment new-environment)
                         (if next-name
                             `(,next-name ,@(env:position-variables new-environment))
                             (funcall success-cont new-environment)))
                       failure-cont))
                 (element rest-expressions (list* next-name rest-names) next-environment)))))))
    (if expressions
        `(labels ,(element expressions names environment)
           (,(first names) ,@(env:position-variables environment)))
        (funcall success-cont environment))))

;;; Rules

#+no (defmethod compile-rule ((grammar    sequence-grammar)
                         (parameters list)
                         (expression t)
                         &key
                         (environment ))
  (compile-rule-using-environment grammar parameters environment expression))
