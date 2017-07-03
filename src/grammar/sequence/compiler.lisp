(cl:in-package #:parser.packrat.grammar.sequence)

;;;

(defmethod compile-bounds-test ((grammar     sequential-grammar-mixin)
                                (environment sequence-environment)
                                (in-cont     function)
                                (out-cont    function))
  `(if (< ,(position* environment) ,(end environment))
       ,(funcall in-cont)
       ,(funcall out-cont)))

(defmethod compile-access ((grammar     sequential-grammar-mixin)
                           (environment sequence-environment)
                           (cont        function))
  (let+ (((&with-gensyms element)))
    `(let ((,element
            (locally
                #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                (elt ,(sequence* environment) ,(position* environment)))))
       ,(funcall cont element))))

(defmethod compile-advance ((grammar     sequential-grammar-mixin)
                            (environment sequence-environment)
                            (cont        function)
                            &key
                            (amount 1)
                            to)
  (let ((position (position* environment)))
    (typecase position
      (array-index
       (funcall cont (env:environment-at
                      environment (list :position (+ position amount)))))
      (t
       (let* ((new-environment (env:environment-at environment :fresh))
              (new-position    (position* new-environment)))
         `(let ((,new-position ,(or to `(+ ,position ,amount))))
            ,(funcall cont new-environment)))))))

;;; TODO these are for sequential-grammar-mixin and sequential-environment-mixin and should be separate from the sequence-{grammar,environment}

(defmethod compile-expression ((grammar      sequential-grammar-mixin)
                               (environment  sequential-environment-mixin)
                               (expression   exp::value-environment-needing-mixin)
                               (success-cont function)
                               (failure-cont function))
  (compile-bounds-test
   grammar environment
   (lambda ()
     (compile-access
      grammar environment
      (lambda (value)
        (compile-expression
         grammar
         #+old (env:environment-carrying environment value)
         (env:environment-at environment (list :value value)
                             :class 'env:value-environment
                             :state '())
         expression
         (lambda (new-environment)
           (declare (ignore new-environment))
           (compile-advance grammar environment success-cont)) ; TODO should make sequence-environment with new-environment as parent
         (lambda (new-environment)
           (declare (ignore new-environment))
           (funcall failure-cont environment))))))
   (curry failure-cont environment)))

(defmethod compile-expression :before  ((grammar      t)
                                        (environment  t)
                                        (expression   repetition-expression)
                                        (success-cont function)
                                        (failure-cont function))
  (let ((min (min-repetitions expression))
        (max (max-repetitions expression)))
    (cond
      ((and (equal min 1) (equal max 1))
       (error "~@<In ~A, minimum and maximum repetition count are both
               1.~@:>"
              expression))
      ((equal min 0)
       (error "~@<In ~A, minimum repetition count is 0.~@:>"
              expression)))))

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
                            (max=1?     ;TODO
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
  #+old (let+ (((&labels+ sub ((&optional first &rest rest) environment)
                  (compile-expression
                   grammar environment first
                   (lambda (new-environment)
                     (if rest
                         (sub rest new-environment)
                         (funcall success-cont new-environment)))
                   failure-cont))))
          (sub (exp:sub-expressions expression) environment))
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

    `(labels ,(element expressions names environment)
       (,(first names) ,@(env:position-variables environment)))))

;;; Rules

(defmethod compile-rule ((grammar    sequence-grammar)
                         (parameters list)
                         (expression t)
                         &key
                         (environment (make-instance 'sequence-environment)))
  (compile-rule-using-environment grammar parameters environment expression))
