;;;; parse.lisp --- Bootstrap parser.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.bootstrap)

;;; Hook into expression parsing

(defvar *bootstrapping* t)

(defmethod parser.packrat.grammar:parse-expression :around ((grammar t) (expression t) &key builder)
  (declare (ignore builder))
  (if *bootstrapping*
      (parse expression)
      (call-next-method)))

;;;

(defparameter *macros*
  `((?       . ,(lambda (expression)
                  (assert (length= 2 expression))
                  `(* ,(second expression) nil 1)))
    (+       . ,(lambda (expression)
                  (assert (length= 2 expression))
                  `(* ,(second expression) 1)))

    (list    . ,(lambda (expression)
                  `(list-elements (seq ,@(rest expression)))))

    (list*   . ,(lambda (expression)
                  (let ((rest (rest expression)))
                    `(list-elements (seq ,@(butlast rest) (rest ,(lastcar rest)))))))

    (cons    . ,(lambda (expression)
                  (let+ (((car cdr) (rest expression)))
                    `(structure 'cons (car ,car) (cdr ,cdr)))))

    (value   . ,(lambda (expression)
                  (let+ ((((object) expression) (rest expression)))
                    `(and ,object ,expression))))))

(defparameter *primitives*
  (flet ((constant-ish (expression context)
           (case context
             ((:invoke :repetition-constraint :structure-type)
              (make-instance 'base:constant-expression :value expression))
             (t
              (make-instance 'base:terminal-expression :value expression)))))

    `(;; Predicate
      ((cons (eql guard))
       . ,(lambda (expression context recurse)
            (declare (ignore context))
            (destructuring-bind (sub-expression predicate)
                (if (length= 2 expression)
                    (list* :any (rest expression))
                    (rest expression))
              (make-instance 'base:predicate-expression
                             :sub-expression (funcall recurse sub-expression)
                             :predicate      predicate))))

      ;; Anything
      ((member any :any)
       . ,(lambda (expression context recurse)
            (declare (ignore expression context recurse))
            (make-instance 'base:anything-expression)))

      ;; Terminal
      ((and symbol (not (or null keyword)))
       . ,(lambda (expression context recurse)
            (declare (ignore recurse))
            (case context
              ((:invoke :repetition-constraint :structure-type)
               (make-instance 'base:variable-reference-expression :variable expression))
              (t
               (make-instance 'base:set-expression
                              :sub-expression (make-instance 'base:anything-expression)
                              :variable       expression)))))

      ((cons (eql quote))
       . ,(lambda (expression context recurse)
            (declare (ignore recurse))
            (constant-ish (second expression) context)))

      ((not (or cons (and symbol (not (or null keyword))) (member :position)))
       . ,(lambda (expression context recurse)
            (declare (ignore recurse))
            (constant-ish expression context)))

      ;; Combinators
      ((cons (eql not))
       . ,(lambda (expression context recurse)
            (declare (ignore context))
            (make-instance 'base:not-expression
                           :sub-expression (funcall recurse (second expression)))))
      ((cons (eql or))
       . ,(lambda (expression context recurse)
            (declare (ignore context))
            (make-instance 'base:or-expression
                           :sub-expressions (map 'list recurse (rest expression)))))
      ((cons (eql and))
       . ,(lambda (expression context recurse)
            (declare (ignore context))
            (make-instance 'base:and-expression
                           :sub-expressions (map 'list recurse (rest expression)))))
      ((cons (eql :compose))
       . ,(lambda (expression context recurse)
            (declare (ignore context))
            (make-instance 'base::compose-expression
                           :sub-expressions (map 'list recurse (rest expression)))))
      ((cons (eql must))
       . ,(lambda (expression context recurse)
            (declare (ignore context))
            (make-instance 'base::must-expression
                           :sub-expression (funcall recurse (second expression))
                           :message        (third expression))))

      ;; Structure
      ((cons (eql structure))
       . ,(lambda (expression context recurse)
            (declare (ignore context))
            (destructuring-bind (type &rest sub-expressions) (rest expression)
              (make-instance 'parser.packrat.grammar.sexp::structure-expression
                             :sub-expressions (map 'list (compose recurse #'second)
                                                   sub-expressions)
                             :readers         (map 'list #'first sub-expressions)
                             :type            (funcall recurse type :structure-type))))))))

(defun parse (expression)
  (let+ (((&labels map-rec (expressions &optional context)
            (map 'list (rcurry #'rec context) expressions)))
         ((&labels combinator (class sub-expressions)
            (make-instance class :sub-expressions (map-rec sub-expressions))))
         ((&labels rec (expression &optional context)
            ;; "macros"
            (when (consp expression)
              (when-let ((expander (assoc-value *macros* (car expression) :test #'eq)))
                (return-from rec
                  (rec (funcall expander expression)))))
            ;; "primitives"
            (when-let ((expander (cdr (find expression *primitives*
                                            :test #'typep :key #'car))))
              (return-from rec
                (funcall expander expression context
                         (lambda (expression &optional (context context))
                           (rec expression context)))))

            (etypecase expression
              ;; Value as sequence
              ((cons (eql vector-elements))
               (make-instance 'parser.packrat.grammar.sexp::as-vector-expression
                              :sub-expression (rec (second expression))))
              ((cons (eql list-elements))
               (make-instance 'parser.packrat.grammar.sexp::as-list-expression
                              :sub-expression (rec (second expression))))
              ((cons (eql rest) (cons t null))
               (make-instance 'parser.packrat.grammar.sexp::rest-expression
                              :sub-expression (rec (second expression))))

              ;; Sequences
              ((cons (eql *))
               (let+ (((sub &optional min max) (rest expression)))
                 (make-instance 'seq:repetition-expression
                                :sub-expression  (rec sub)
                                :min-repetitions (when min (rec min :repetition-constraint))
                                :max-repetitions (when max (rec max :repetition-constraint)))))
              ((cons (eql seq))
               (combinator 'seq:sequence-expression (rest expression)))

              ;; Variables
              ((cons (eql <-))
               (let+ (((variable &optional (sub-expression :any)) (rest expression)))
                 (make-instance 'base:set-expression
                                :sub-expression (rec sub-expression)
                                :variable       variable)))
              ((cons (eql <<-))
               (let+ (((variable &optional (sub-expression :any)) (rest expression)))
                 (make-instance 'base:push-expression
                                :sub-expression (rec sub-expression)
                                :variable       variable)))

              ;; Transform
              ((cons (eql :transform))
               (destructuring-bind (sub-expression &body code) (rest expression)
                 (make-instance 'base:transform-expression
                                :sub-expression (rec sub-expression)
                                :code           code)))

              ; Invocation
              ((cons (eql next-rule))
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
