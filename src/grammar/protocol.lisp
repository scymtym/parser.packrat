;;;; protocol.lisp --- Protocol provided by the grammar module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar)

;;; Name protocol

(defgeneric name (grammar))

;;; Dependency protocol
;;;
;;; For querying and managing dependencies between rules.
;;;
;;;          dependent
;;;        ◀─────────────
;;;   user                used
;;;        ─────────────▶
;;;          dependency

(defgeneric dependencies (thing))

(defgeneric (setf dependencies) (new-value thing))

(defgeneric map-dependencies (function used))

(defgeneric dependents (thing))

(defgeneric (setf dependents) (new-value thing))

(defgeneric map-dependents (function used))

(defgeneric add-dependency (used user))

(defgeneric remove-dependency (used user))

;;; Default behavior

(macrolet ((define (name reader)
             `(defmethod ,name (function used)
                (let ((seen (make-hash-table :test #'eq)))
                  (labels ((visit (current)
                             (unless (gethash current seen)
                               (setf (gethash current seen) t)
                               (funcall function current)
                               (map nil #'visit (,reader current)))))
                    (declare (dynamic-extent #'visit))
                    (map nil #'visit (,reader used)))))))
  (define map-dependents dependents)
  (define map-depencies  dependencies))

(defmethod add-dependency :before (used user)
  (when (find used (dependencies user) :test #'eq)
    (error "~@<~A already depends on ~A.~@:>" user used)) ; TODO condition classes
  (when (find user (dependents used) :test #'eq)
    (error "~@<~A already is a dependent of ~A.~@:>" user used)))

(defmethod add-dependency (used user)
  (push used (dependencies user))
  (push user (dependents used)))

(defmethod add-dependency :after (used user)
  (loop :for (name . rule) :in (rules/alist user)
        :when (typep rule 'forward-referenced-rule)
        :do (when-let ((inherited (find-rule name used :if-does-not-exist nil)))
              (error "~@<Using grammar ~A in grammar ~A should replace
                      ~A with inherited rule ~A, but that behavior is
                      not implemented yet.~@:>"
                     used user rule inherited))))

;;; Rule protocol

(defgeneric expression (rule))

(defgeneric (setf rule-function) (new-value rule))

;;; Grammar protocol
;;;
;;; Mostly rule creation and acting as a container for rule instances.

(defgeneric rules (grammar))

(defgeneric rules/alist (grammar))

(defgeneric find-rule (name grammar &key recursive? if-does-not-exist)) ; TODO could also add find-direct-rule instead of recursive

(defgeneric (setf find-rule) (new-value name grammar
                              &key recursive? if-does-not-exist))

(defgeneric ensure-rule (name grammar &key rule-class &allow-other-keys))

(defgeneric ensure-rule-using-rule (rule name grammar
                                    &key rule-class &allow-other-keys))

(defgeneric parse-expression (grammar expression &key builder) ; TODO allow dispatch on rule class? separate protocol?
  (:documentation
   "Parse the s-expression EXPRESSION according to GRAMMAR's meta-grammar."))

(defgeneric parse (grammar expression input) ; TODO separate protocol?
  (:documentation
   "Parse INPUT according to EXPRESSION."))

(defgeneric default-environment (grammar expression)
  (:documentation
   "Return a default environment instance for GRAMMAR and expression."))

;;; Default behavior

(defun coerce-rule-if-does-not-exist (value name)
  (typecase value
    ((or (eql :forward-reference) (cons (eql :forward-reference)))
     (lambda (condition)
       (if (typep condition 'rule-missing-error)
           (let ((rule (apply #'make-instance 'forward-referenced-rule
                              :name name
                              (when (consp value)
                                (rest value)))))
             (invoke-restart 'store-value rule))
           (error condition))))
    (t
     value)))

(defmethod find-rule :around ((name t) (grammar t)
                              &key (recursive? t) (if-does-not-exist #'error))
  (let ((if-does-not-exist (coerce-rule-if-does-not-exist
                            if-does-not-exist name)))
    (or (call-next-method name grammar
                          :recursive?        recursive?
                          :if-does-not-exist nil)
        (more-conditions:error-behavior-restart-case
            (if-does-not-exist
             (rule-missing-error :grammar grammar :rule name)
             :warning-condition 'rule-missing-warning)
          (store-value (value)
            (setf (find-rule name grammar) value))))))

(defmethod (setf find-rule) :after ((new-value t) (name symbol) (grammar t)
                                    &key recursive? if-does-not-exist)
  (declare (ignore recursive? if-does-not-exist))
  (map-dependents
   (lambda (dependent)
     (when-let ((rule (find-rule name dependent :if-does-not-exist nil
                                                :recursive?        nil)))
       (when (typep rule 'forward-referenced-rule)
         (error "~@<Adding rule ~A to grammar ~A should replace ~A in ~
                 grammar ~A but that is not implemented yet.~@:>"
                name grammar rule dependent))))
   grammar))

(defmethod ensure-rule ((name t) (grammar t) &rest args &key &allow-other-keys)
  (let ((rule (find-rule name grammar :recursive? nil :if-does-not-exist nil)))
    (apply #'ensure-rule-using-rule rule name grammar args)))

(defmethod ensure-rule-using-rule ((rule null) (name t) (grammar t)
                                   &rest args
                                   &key (rule-class (find-class 'rule))
                                   &allow-other-keys)
  (setf (find-rule name grammar)
        (apply #'make-instance rule-class :name name
               (remove-from-plist args :rule-class))))

(defmethod ensure-rule-using-rule ((rule t) (name t) (grammar t)
                                   &rest args
                                   &key (rule-class (find-class 'rule))
                                   &allow-other-keys)
  (let ((initargs (list* :name name (remove-from-plist args :rule-class))))
    (if (eq (class-of rule) rule-class)
        (apply #'reinitialize-instance rule initargs) ; TODO reset environment to default if not supplied
        (apply #'change-class rule rule-class initargs))))

(defmethod parse ((grammar symbol) (expression t) (input t))
  (let ((grammar (find-grammar grammar)))
    (parse grammar expression input)))

(defmethod parse ((grammar t) (expression t) (input t))
  (parse grammar (parse-expression grammar expression) input))

(defmethod parse ((grammar t) (expression parser.packrat.expression:expression) (input t))
  (let* (; (exp:var)
         (code       (c:compile-rule grammar nil '() expression))
         (function   (compile nil code)))
    (parse grammar function input)
    #+later-special-case (if (typep expression 'parser.packrat.grammar.base:rule-invocation-expression)
                             (find-rule
                              (parser.packrat.grammar.base:rule expression)
                              grammar))))

(defmethod make-context ((grammar t) (expression t) (input t))
  (make-hash-table :test #'equal))

(defmethod parse ((grammar t) (expression function) (input t))
  (let ((context (make-context grammar expression input))) ; TODO passing EXPRESSION doesn't make any sense here
    (funcall expression context input)))

;;; Compilation

;;; Default method for compiler protocol
(defmethod c:compile-rule
    ((grammar    t)
     (name       t)
     (parameters list)
     (expression t)
     &key
     (environment (default-environment grammar expression)))
  (c:compile-rule-using-environment
   grammar name parameters environment expression))

(defmethod c:validate-invocation ((grammar     t)
                                  (environment t)
                                  (invocation  t))
  (typep environment (class-of (default-environment grammar invocation))))

;;; Grammar namespace

(defvar *grammars* (make-hash-table :test #'eq))

(defun find-grammar (name &key (if-does-not-exist #'error))
  (or (gethash name *grammars*)
      (more-conditions:error-behavior-restart-case
          (if-does-not-exist
           (grammar-missing-error :grammar name)
           :warning-condition grammar-missing-warning)
        (store-value (value)
          (setf (find-grammar name) value)))))

(defun (setf find-grammar) (new-value name)
  (setf (gethash name *grammars*) new-value))

(defgeneric ensure-grammar (name &key grammar-class &allow-other-keys))

(defgeneric ensure-grammar-using-grammar (grammar name
                                          &key grammar-class &allow-other-keys))

;;; Default behavior

(defmethod ensure-grammar ((name t) &rest args &key &allow-other-keys)
  (let ((grammar (find-grammar name :if-does-not-exist nil)))
    (apply #'ensure-grammar-using-grammar grammar name args)))

(flet ((resolve-used (names)
         (map 'list #'find-grammar names)))

  (defmethod ensure-grammar-using-grammar ((grammar null) (name t)
                                           &rest args
                                           &key grammar-class use
                                           &allow-other-keys)
    (setf (find-grammar name)
          (apply #'make-instance grammar-class
                 :name         name
                 :dependencies (resolve-used use)
                 (remove-from-plist args :grammar-class :use))))

  (defmethod ensure-grammar-using-grammar ((grammar t) (name t)
                                           &rest args
                                           &key grammar-class use
                                           &allow-other-keys)
    (let ((initargs (list* :name          name
                           :dependencies  (resolve-used use)
                           (remove-from-plist args :grammar-class :use))))
      (apply #'change-class grammar grammar-class initargs))))
