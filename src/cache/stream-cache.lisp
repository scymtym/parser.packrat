(cl:in-package #:parser.packrat.cache)

(defclass stream-cache ()
  ((stream :initarg  :stream
           :reader   stream)
   (chunks :reader   chunks
           :initform (make-chunk-cache 0 10))))

(defmacro define-make-functions (class make-chunk-function chunk-type)
  `(defmethod make-functions ((cache ,class))
     (let+ (((&accessors-r/o stream chunks) cache)
            (divisor         (chunk-cache-divisor chunks))
            (chunk-length    (ash 1 divisor))
            (length-at-least 0)
            (end-seen?       nil)
            ((&flet fill-chunk ()
               (declare (optimize (speed 3) (debug 0) (safety 0)))
               ; (log:info "~@<Reading ~D at ~D.~@:>" chunk-length length-at-least)
               (let* ((chunk (,make-chunk-function divisor))
                      (read  (read-sequence chunk stream)))
                 (declare (type ,chunk-type chunk))
                 (incf length-at-least read)
                 (when (< read chunk-length)
                   (setf end-seen? t))
                 chunk))))
       (locally (declare (type input-position length-at-least)
                         (optimize (speed 3) (debug 0) (safety 0)))
         (values
          ;; bounds test
          (lambda (position)
            (declare (type input-position position))
            (cond
              ((< position length-at-least)
               length-at-least)
              (end-seen?
               nil)
              (t
               (ensure-chunk chunks position #'fill-chunk)
               (when (< position length-at-least)
                 length-at-least))))
          ;; access
          (lambda (position)
            (declare (type input-position position))
            (let ((chunk      (the ,chunk-type
                                   (ensure-chunk chunks position #'fill-chunk)))
                  (position-2 (ldb (byte divisor 0) position)))
              (aref chunk position-2))))))))

;;; `octet-vector-stream-cache'

(deftype octet-vector-chunk ()
  '(simple-array t 1))

(declaim (inline make-octet-vector-chunk))
(defun make-octet-vector-chunk (divisor)
  (make-array (ash 1 divisor) :initial-element nil))

(defclass octet-vector-stream-cache (stream-cache)
  ())

;;; `string-stream-cache'

(deftype string-chunk ()
  '(and (not (simple-array nil)) simple-string))

(declaim (inline make-string-chunk))
(defun make-string-chunk (divisor)
  (make-string (ash 1 divisor)))

(defclass string-stream-cache (stream-cache)
  ())

(define-make-functions string-stream-cache make-string-chunk string-chunk)
