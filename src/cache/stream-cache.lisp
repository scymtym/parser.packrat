;;;; stream-cache.lisp --- A stream content cache for backtracking parsers.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.cache)

;;; `stream-cache'
;;;
;;; Superclass for chunk caches for input from streams. Sub classes
;;; handle streams with specific element types. The cache

(defclass stream-cache ()
  ((%stream :initarg  :stream
            :reader   stream)
   (%chunks :reader   chunks
            :initform (make-chunk-cache 0 10)))
  (:default-initargs
   :stream (missing-required-initarg 'stream-cache :stream)))

(defmacro define-make-functions (class make-chunk-function chunk-type)
  `(defmethod make-functions ((cache ,class))
     (let* ((stream          (stream cache))
            (chunks          (chunks cache))
            (divisor         (chunk-cache-divisor chunks))
            (chunk-length    (ash 1 divisor))
            (length-at-least 0)
            (end-seen?       nil))
       (declare (type input-position length-at-least)
                (optimize (speed 3) (debug 0) (safety 0)))
       (flet ((fill-chunk ()
                (let* ((chunk (,make-chunk-function divisor))
                       (read  (read-sequence chunk stream)))
                  (declare (type ,chunk-type chunk))
                  (incf length-at-least read)
                  (when (< read chunk-length)
                    (setf end-seen? t))
                  chunk)))
         (values
          ;; bounds test
          (lambda (position)
            (declare (type input-position position))
            (cond ((< position length-at-least)
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
;;;
;;; A chunk cache for parsing input in the form of an octet stream.

(deftype octet-vector-chunk ()
  '(simple-array t 1))

(declaim (inline make-octet-vector-chunk))
(defun make-octet-vector-chunk (divisor)
  (make-array (ash 1 divisor) :initial-element nil))

(defclass octet-vector-stream-cache (stream-cache)
  ())

(define-make-functions octet-vector-stream-cache
  make-octet-vector-chunk octet-vector-chunk)

(defun make-octet-vector-stream-cache (stream)
  (make-instance 'octet-vector-stream-cache :stream stream))

;;; `string-stream-cache'
;;;
;;; A chunk cache for parsing input in the form of a string stream.

(deftype string-chunk ()
  '(and (not (simple-array nil)) simple-string))

(declaim (inline make-string-chunk))
(defun make-string-chunk (divisor)
  (make-string (ash 1 divisor)))

(defclass string-stream-cache (stream-cache)
  ())

(define-make-functions string-stream-cache
  make-string-chunk string-chunk)

(defun make-string-stream-cache (stream)
  (make-instance 'string-stream-cache :stream stream))
