(cl:in-package #:parser.packrat.environment)

;; Idea for position/state representation:
;;
;; Instead of defining protocol for value-environment,
;; sequence-environment, etc., make a protocol
;; (defgeneric state-variable (name environment)
;; this could traverse ancestors to find the current value.

;;; Environment lookup protocol

(defgeneric direct-bindings (environment)
  (:documentation
   "TODO"))

(defgeneric bindings (environment &key include-shadowed?)
  (:documentation
   "TODO"))

(defgeneric lookup (name environment)
  (:documentation
   ""))

(defgeneric (setf lookup) (new-value name environment)
  (:documentation
   ""))

;;; Default behavior

(defmethod bindings ((environment t) &key include-shadowed?)
  (let ((direct    (direct-bindings environment))
        (inherited (when-let ((parent (parent environment)))
                     (bindings parent :include-shadowed? include-shadowed?))))
    (append direct
            (if include-shadowed?
                inherited
                (remove-if (lambda+ ((name . &ign))
                             (find name direct :test #'eq :key #'car))
                           inherited)))))

;;; Environment hierarchy protocol

(defgeneric parent (environment)
  (:documentation
   ""))

(defgeneric root (environment)
  (:documentation
   ""))

(defgeneric depth (environment))

;;; Default environment

(defmethod root ((environment t))
  (labels ((rec (environment)
             (if-let ((parent (parent environment)))
               (rec parent)
               environment)))
    (rec environment)))

(defmethod depth ((environment t))
  (labels ((rec (environment depth)
             (if-let ((parent (parent environment)))
               (rec parent (1+ depth))
               depth)))
    (rec environment 0)))

;;; Environment state protocol

(defgeneric state-variables (environment)
  (:documentation
   ""))

(defgeneric state-variables/plist (environment)
  (:documentation
   ""))

(defgeneric position-variables (environment)
  (:documentation
   ""))

(defgeneric position-variables/plist (environment)
  (:documentation
   ""))

;;; Environment manipulation

(defgeneric environment-binding (parent &rest bindings))

;;; Default behavior

(defmethod environment-binding ((parent t) &rest bindings &key)
  (let ((environment (environment-at parent '()) ; (make-instance (class-of parent) :parent parent)
          ))
    (loop :for (name value) :on bindings :by #'cddr
          :do (setf (lookup name environment) value))
    environment))

(defgeneric environment-at (base position &key class parent state)
  ;; For compiling invocations, we need something like
  (:documentation
   "Make a new environment with fresh position variables, taking the
    remaining state variables from this existing environment and
    optionally using yet another environment as parent"))

(defmethod environment-at ((base t) (position (eql :fresh))
                           &rest args &key class parent state)
  (declare (ignore class parent state))
  ;; TODO maybe (assert (or (not class) (eq (class-of base) class)))
  (let* ((position (position-variables/plist base))
         (fresh-position
           (loop :for (role variable) :on position :by #'cddr
                 :collect role :collect (gensym (string role)))))
    (apply #'environment-at base fresh-position args)))

(defmethod environment-at ((base t) (position t)
                           &key
                           (class  (class-of base))
                           (parent base)
                           (state  :copy))
  (apply #'make-instance class
         :parent parent
         (append position
                 (case state
                   (:copy (state-variables/plist base))
                   (t     state)))))

;;; Value environment protocol

(defgeneric value (environment))

(defun value* (environment)
  (labels ((rec (environment)
             (if (compute-applicable-methods #'value (list environment))
                 (let ((value (value environment)))
                   value)
                 (when-let ((parent (parent environment)))
                   (rec parent)))))
    (rec environment)))
