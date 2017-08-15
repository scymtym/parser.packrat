(cl:in-package #:parser.packrat.compiler)

(defun %remove-non-terminal-nils (forms)
  (append (remove nil (butlast forms))
          (when-let ((last (lastcar forms)))
            (list last))))

(defun maybe-let (bindings &rest forms)
  (let ((forms (%remove-non-terminal-nils forms)))
    (if bindings
        `(let ,bindings ,@forms)
        (apply #'maybe-progn forms))))

(defun maybe-let* (bindings &rest forms)
  (let ((forms (%remove-non-terminal-nils forms)))
    (if bindings
        `(let* ,bindings ,@forms)
        (apply #'maybe-progn forms))))

(defun maybe-symbol-macrolet (bindings &rest forms)
  (let ((forms (%remove-non-terminal-nils forms)))
    (if bindings
        `(symbol-macrolet ,bindings ,@forms)
        (apply #'maybe-progn forms))))

(defun maybe-progn (&rest forms)
  (let ((forms (%remove-non-terminal-nils forms)))
    (case (length forms)
      (0 nil)
      (1 (first forms))
      (t `(progn ,@forms)))))
