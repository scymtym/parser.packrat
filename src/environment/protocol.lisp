(cl:in-package #:parser.packrat.environment)

;; Idea for position/state representation:
;;
;; Instead of defining protocol for value-environment,
;; sequence-environment, etc., make a protocol
;; (defgeneric state-variable (name environment)
;; this could traverse ancestors to find the current value.

;;; Environment lookup protocol

(defgeneric lookup (name environment)
  (:documentation
   ""))

(defgeneric (setf lookup) (new-value name environment)
  (:documentation
   ""))

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
    environment))

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
  (let ((environment (make-instance (class-of parent) :parent parent)))
    (loop :for (name value) :on bindings :by #'cddr :do
       (setf (lookup name environment) value))
    environment))

(defgeneric environment-at (base position &key parent state)
  ;; For compiling invocations, we need something like
  (:documentation
   "Make a new environment with fresh position variables, taking the
remaining state variables from this existing environment and
optionally using yet another environment as parent"))

(defmethod environment-at ((base t) (position (eql :fresh))
                           &rest args &key parent state)
  (declare (ignore parent state))
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
