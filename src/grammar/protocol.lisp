;;;; protocol.lisp --- Protocol provided by the grammar module.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar)

;;; Name protocol

(defgeneric name (grammar))

;;; Dependency protocol
;;;
;;; For querying and managing dependencies between rules. (and grammars?)

(defgeneric dependencies (thing))

(defgeneric (setf dependencies) (new-value thing))

(defgeneric dependents (thing))

(defgeneric (setf dependents) (new-value thing))

(defgeneric add-dependency (used user))

(defgeneric remove-dependency (used user))

;;; Default behavior

(defmethod add-dependency :before (used user)
  (when (find used (dependencies user) :test #'eq)
    (error "~@<~A already depends on ~A.~@:>" user used))
  (when (find user (dependents used) :test #'eq)
    (error "~@<~A already is a dependent of ~A.~@:>" user used)))

(defmethod add-dependency (used user)
  (push used (dependencies user))
  (push user (dependents used)))

;;; Rule protocol

(defgeneric expression (rule))

(defgeneric (setf rule-function) (new-value rule))

;;; Grammar protocol
;;;
;;; Mostly rule creation and acting as a container for rule instances.

(defgeneric rules (grammar))

(defgeneric rules/alist (grammar))

(defgeneric find-rule (name grammar &key if-does-not-exist))

(defgeneric (setf find-rule) (new-value name grammar
                              &key if-does-not-exist))

(defgeneric ensure-rule (name grammar &key rule-class &allow-other-keys))

(defgeneric ensure-rule-using-rule (rule name grammar
                                    &key rule-class &allow-other-keys))

(defgeneric parse-expression (grammar expression) ; TODO allow dispatch on rule class? separate protocol?
  (:documentation
   ""))

(defgeneric parse (grammar expression input) ; TODO separate protocol?
  (:documentation
   "TODO"))

(defgeneric default-environment (grammar expression)
  (:documentation
   "Return a default environment instance for GRAMMAR and expression."))

(defmethod parse-expression ((grammar t) (expression t)) ; TODO hack
  (uiop:symbol-call '#:parser.packrat.bootstrap '#:bootstrap-parse
                    expression))

;;; Default behavior

(defun coerce-rule-if-does-not-exist (value name)
  (case value
    (:forward-reference
     (lambda (condition)
       (if (typep condition 'rule-missing-error)
           (let ((rule (make-instance
                        'forward-referenced-rule
                        :name name)))
             (invoke-restart 'store-value rule))
           (error condition))))
    (t
     value)))

(defmethod find-rule :around ((name t) (grammar t)
                              &key (if-does-not-exist #'error))
  (let ((if-does-not-exist (coerce-rule-if-does-not-exist
                            if-does-not-exist name)))
    (or (call-next-method)
        (more-conditions:error-behavior-restart-case
            (if-does-not-exist
             (rule-missing-error :grammar grammar :rule name)
             :warning-condition 'rule-missing-warning)
          (store-value (value)
            (setf (find-rule name grammar) value))))))

(defmethod ensure-rule ((name t) (grammar t) &rest args &key &allow-other-keys)
  (let ((rule (find-rule name grammar :if-does-not-exist nil)))
    (apply #'ensure-rule-using-rule rule name grammar args)))

(defmethod ensure-rule-using-rule ((rule null) (name t) (grammar t)
                                   &rest args &key rule-class &allow-other-keys)
  (setf (find-rule name grammar)
        (apply #'make-instance rule-class :name name
               (remove-from-plist args :rule-class))))

(defmethod ensure-rule-using-rule ((rule t) (name t) (grammar t)
                                   &rest args &key rule-class &allow-other-keys)
  (let ((initargs (list* :name name (remove-from-plist args :rule-class))))
    (if (typep rule rule-class)
        (apply #'reinitialize-instance rule initargs)
        (apply #'change-class rule rule-class initargs))))

(defmethod parse ((grammar symbol) (expression t) (input t))
  (let ((grammar (find-grammar grammar)))
    (parse grammar expression input)))

(defmethod parse ((grammar t) (expression t) (input t))
  (let* ((expression (parse-expression grammar expression))
                                        ; (exp:var)
         (code       (c:compile-rule grammar '() expression))
         (function   (compile nil code)))
    (parse grammar function input)
    #+later-special-case (if (typep expression 'parser.packrat.grammar.base:rule-invocation-expression)
                             (find-rule
                              (parser.packrat.grammar.base:rule expression)
                              grammar ))))

#+no (defmethod parse ((grammar t) (expression cons) (input t))
       (find-rule expression grammar input))

(defmethod make-context ((grammar t) (expression t) (input t))
  (make-hash-table :test #'equal))

(defmethod parse ((grammar t) (expression function) (input t))
  (let ((context (make-context grammar expression input)))
    (funcall expression context input)))

;;; Compilation

;;; Default method for compiler protocol
(defmethod c:compile-rule
    ((grammar    t)
     (parameters list)
     (expression t)
     &key
     (environment (default-environment grammar expression)))
  (c:compile-rule-using-environment
   grammar parameters environment expression))

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
           :warning-condition 'grammar-missing-warning)
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

(defmethod ensure-grammar-using-grammar ((grammar null) (name t)
                                         &rest args
                                         &key grammar-class &allow-other-keys)
  (setf (find-grammar name)
        (apply #'make-instance grammar-class :name name
               (remove-from-plist args :grammar-class))))

(defmethod ensure-grammar-using-grammar ((grammar t) (name t)
                                         &rest args
                                         &key grammar-class &allow-other-keys)
  (let ((initargs (list* :name name (remove-from-plist args :grammar-class))))
    (if (typep grammar grammar-class)
        (apply #'reinitialize-instance grammar initargs)
        (apply #'change-class grammar grammar-class initargs))))
