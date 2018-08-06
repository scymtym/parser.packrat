(cl:in-package #:parser.packrat.bootstrap)

(defparameter *macros*
  `((?       . ,(lambda (expression)
                  (assert (length= 2 expression))
                  `(* ,(second expression) nil (const 1))))

    (list    . ,(lambda (expression)
                  `(list-elements (seq ,@(rest expression)))))

    (list*   . ,(lambda (expression)
                  (let ((rest (rest expression)))
                    `(list-elements (seq ,@(butlast rest) (rest ,(lastcar rest)))))))

    (vector  . ,(lambda (expression)
                  `(vector-elements (seq ,@(rest expression)))))

    (vector* . ,(lambda (expression)
                  (break "vector* is not implemented")
                  `(vector-elements (seq ,@(rest expression)))))

    (cons    . ,(lambda (expression)
                  (let+ (((car cdr) (rest expression)))
                    `(structure 'cons (car ,car) (cdr ,cdr)))))

    (bounds  . ,(lambda (expression)
                  (let+ ((((start end) &rest expressions) (rest expression)))
                    `(:seq (<- ,start :position) ,@expressions (<- ,end :position)))))))

(defparameter *primitives*
  '())

(defun bootstrap-parse (expression)
  (let+ (((&labels map-rec (expressions &optional context)
            (map 'list (rcurry #'rec context) expressions)))
         ((&labels combinator (class sub-expressions)
            (make-instance class :sub-expressions (map-rec sub-expressions))))
         ((&labels maybe-combinator (class sub-expressions)
            (case (length sub-expressions)
              (0 (error "not implemented"))
              (1 (rec (first sub-expressions)))
              (t (combinator class sub-expressions)))))
         ((&labels symbol (expression context)
            (case context
              ((:invoke :repetition-constraint :structure-type)
               (make-instance 'base:variable-reference :variable expression))
              (t
               (make-instance 'base:set-expression
                              :sub-expression (make-instance 'base:anything-expression)
                              :variable       expression)))))
         ((&labels constant-ish (expression context)
            (case context
              ((:invoke :repetition-constraint :structure-type)
               (make-instance 'base:constant-expression :value expression))
              (t
               (make-instance 'base:terminal-expression :value expression)))))
         ((&labels rec (expression &optional context)
            (when (consp expression)
              ;; "macros"
              (when-let ((expander (assoc-value *macros* (car expression) :test #'eq)))
                (return-from rec (rec (funcall expander expression))))
              ;; "primitives"
              (when-let ((expander (assoc-value *primitives* (car expression) :test #'eq)))
                (return-from rec (funcall expander expression (rcurry #'rec context)))))

            (etypecase expression
              ;; ((cons (member ? :?) (cons t null))
              ;;  (rec `(* ,(second expression) nil (const 1))))
              ;;
              ;; ((cons (eql list))
              ;;  (rec `(list-elements (seq ,@(rest expression)))))
              ;;
              ;; ((cons (eql list*))
              ;;  (let ((rest (rest expression)))
              ;;    (rec `(list-elements (seq ,@(butlast rest) (rest ,(lastcar rest)))))))
              ;;
              ;; ((cons (eql vector))
              ;;  (rec `(vector-elements (seq ,@(rest expression)))))
              ;; ((cons (eql vector*))
              ;;  (break "vector* is not implemented")
              ;;  (rec `(vector-elements (seq ,@(rest expression)))))
              ;;
              ;; ((cons (eql cons))
              ;;  (let+ (((car cdr) (rest expression)))
              ;;    (rec `(structure 'cons (car ,car) (cdr ,cdr)))))

              ;; magic variables
              ((eql :position)
               (make-instance 'parser.packrat.grammar.base::position-expression))

              ;; structure
              ((cons (eql structure))
               (destructuring-bind (type &rest sub-expressions) (rest expression)
                 (make-instance 'parser.packrat.grammar.sexp::structure-expression
                                :sub-expressions (map-rec (mapcar #'second sub-expressions))
                                :readers         (mapcar #'first sub-expressions)
                                :type            (rec type :structure-type))))

              ;; value as sequence
              ((cons (eql vector-elements))
               (make-instance 'parser.packrat.grammar.sexp::as-vector-expression
                              :sub-expression (rec (second expression))))
              ((cons (eql list-elements))
               (make-instance 'parser.packrat.grammar.sexp::as-list-expression
                              :sub-expression (rec (second expression))))
              ((cons (eql rest) (cons t null))
               (make-instance 'parser.packrat.grammar.sexp::rest-expression
                              :sub-expression (rec (second expression))))

              ;; predicate
              ((cons (member guard :guard))
               (destructuring-bind (sub-expression predicate)
                   (if (length= 2 expression)
                       (list* :any (rest expression))
                       (rest expression))
                 (make-instance 'base:predicate-expression
                                :sub-expression (rec sub-expression)
                                :predicate      predicate)))

              ;; anything
              ((member any :any)
               (make-instance 'base:anything-expression))
              ;; terminal
              ((and symbol (not keyword))
               (symbol expression context))
              ((cons (eql quote))
               (constant-ish (second expression) context))
              ((not (or cons
                        (and symbol (not keyword))))
               (constant-ish expression context))

              ;; combinators
              ((cons (eql not))
               (make-instance 'base:not-expression
                              :sub-expression (rec (second expression))))
              ((cons (eql or))
               (maybe-combinator 'base:or-expression (rest expression)))
              ((cons (eql and))
               (maybe-combinator 'base:and-expression (rest expression)))
              ((cons (eql :compose))
               (maybe-combinator 'base::compose-expression (rest expression)))
              ((cons (eql *))
               (let+ (((sub &optional min max) (rest expression)))
                 (make-instance 'seq:repetition-expression
                                :sub-expression  (rec sub)
                                :min-repetitions (when min (rec min :repetition-constraint))
                                :max-repetitions (when max (rec max :repetition-constraint)))))
              ((cons (member seq :seq))
               (combinator 'seq:sequence-expression (rest expression)))

              ;; Variables
              ((cons (member <- :<-))
               (let+ (((variable &optional (sub-expression :any)) (rest expression)))
                 (make-instance 'base:set-expression
                                :sub-expression (rec sub-expression)
                                :variable       variable)))
              ((cons (member <<- :<<-))
               (let+ (((variable &optional (sub-expression :any)) (rest expression)))
                 (make-instance 'base:push-expression
                                :sub-expression (rec sub-expression)
                                :variable       variable)))

              ;; const
              ((cons (eql const))
               (make-instance 'base:constant-expression :value (second expression)))

              ;; transform
              ((cons (eql :transform))
               (destructuring-bind (sub-expression &body code) (rest expression)
                 (make-instance 'base:transform-expression
                                :sub-expression (rec sub-expression)
                                :code           code)))

              ((cons (member next-rule :next-rule))
               (let ((arguments (rest expression)))
                 (make-instance 'base:next-rule-invocation-expression
                                :arguments (map-rec arguments :invoke))))
              ((cons (or symbol                           ; rule-name
                         (cons symbol (cons symbol null)) ; (rule-name grammar-name)
                         ))
               (let+ (((rule-and-grammar &rest arguments) expression)
                      ((rule &optional grammar) (ensure-list rule-and-grammar)))
                 (make-instance 'base:rule-invocation-expression
                                :rule      rule
                                :grammar   grammar
                                :arguments (map-rec arguments :invoke))))))))
    (rec expression)))

(defparameter *bootstrap-meta-grammar*
  `(or ;; anything
       'any

       ;; terminal
       ;; any

       ;; Combinators
       (or ;; not
           (list 'not 'expression)
           ;; and
           (list 'and (* 'expression))
           ;; or
           (list 'or (* 'expression 1))
           ;; *
           (list '* 'expression (? (seq 'expression (? 'expression))))
           ;; seq
           (list 'seq (* 'expression)))
       ;;
       (list '>> (<- amount 'positive-integer) 'expression)
       (list '<< (<- amount 'positive-integer) 'expression)

       ;; Variables
       (or (list '<-  (<- name 'symbol) 'expression)
           (list '<<- (<- name 'symbol) 'expression)
           'symbol) ; variable-reference

       ;; rule-invocation
       (list (<- name 'symbol) (* (<<- arguments 'expression)))))

(defun bootstrap ()
  (let+ ((bootstrap-grammar        *bootstrap-meta-grammar*)
         (bootstrap-grammar/parsed (bootstrap-parse bootstrap-grammar)))
    (parser.packrat.compiler:compile-rule
     (make-instance 'parser.packrat.grammar.sexp::sexp-grammar)
     ()
     bootstrap-grammar/parsed)))
