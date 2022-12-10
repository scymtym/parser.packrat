;;;; chunk-cache.lisp --- A linear position based, two-level cache.
;;;;
;;;; Copyright (C) 2018-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.cache)

(deftype input-position ()
  '(unsigned-byte 60))

(deftype chunk-divisor ()
  '(integer 1 30))

;;; Chunk

(deftype chunk ()
  '(simple-array * 1))

(declaim (inline make-chunk))
(defun make-chunk (divisor)
  (make-array (ash 1 divisor) :initial-element nil))

;;; `chunk-array'
;;;
;;; An array of "chunk" arrays. Inner "chunk" arrays correspond to
;;; segments of parser input and are not adjustable. The outer array
;;; is adjustable and generally lazily adjust as more input is seen.

(deftype chunk-array ()
  '(simple-array (simple-array * 1) 1))

(declaim (inline %make-chunk-array))
(defun %make-chunk-array (length)
  (make-array length))

(declaim (ftype (function (array-index chunk-divisor (or null function))
                          (values chunk-array &optional))
                make-chunk-array))
(defun make-chunk-array (length divisor fill)
  (declare (optimize speed))
  (map-into (%make-chunk-array length)
            (or fill (lambda () (make-chunk divisor)))))

(declaim (ftype (function (simple-vector array-index chunk-divisor (or null function))
                          (values chunk-array &optional))
                adjust-chunk-array))
(defun adjust-chunk-array (chunk-array new-length divisor fill)
  (declare (optimize speed))
  (let ((result (%make-chunk-array new-length)))
    (macrolet ((fill-it (filler)
                 `(loop :for i :from (length chunk-array) :below new-length
                        :do (setf (aref result i) ,filler))))
      (if fill
          (fill-it (funcall fill))
          (fill-it (make-chunk divisor))))
    (replace result chunk-array)))

;;; `chunk-cache'
;;;
;;; Provides the interface to the chunk cache and stores the divisor
;;; parameter which determines the size of the individual chunks.

(declaim (inline chunk-cache-chunks chunk-cache-divisor))
(defstruct (chunk-cache
             (:constructor
              make-chunk-cache
              (length divisor
               &optional fill
               &aux (chunks (make-chunk-array length divisor fill)))))
  (chunks  nil :type chunk-array)
  (divisor 10  :type chunk-divisor :read-only t))

(declaim (ftype (function (chunk-cache input-position) (values (or null chunk) &optional))
                find-chunk)
         (inline find-chunk))
(defun find-chunk (chunk-cache position)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((divisor    (chunk-cache-divisor chunk-cache))
         (position-1 (ash position (- divisor)))
         (chunks     (chunk-cache-chunks chunk-cache)))
    (when (< position-1 (length chunks))
      (aref chunks position-1))))
(declaim (notinline find-chunk))

(declaim (ftype (function (chunk-cache input-position &optional function)
                          (values chunk &optional))
                ensure-chunk))
(defun ensure-chunk (chunk-cache position &optional fill)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (or (locally (declare (inline find-chunk))
        (find-chunk chunk-cache position))
      (let* ((divisor    (chunk-cache-divisor chunk-cache))
             (new-length (1+ (ash position (- divisor))))
             (new-chunks (adjust-chunk-array (chunk-cache-chunks chunk-cache)
                                             new-length divisor fill)))
        (setf (chunk-cache-chunks chunk-cache) new-chunks)
        (aref new-chunks (1- new-length)))))
