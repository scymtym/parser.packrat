;;;; sexp-grammar.lisp --- Unit tests for the sexp-grammar class.
;;;;
;;;; Copyright (C) 2017-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.sexp.test)

(in-suite :parser.packrat.grammar.sexp)

(defclass klass ()
  ((e :initarg  :a
      :reader   a
      :initform 1)
   (b :initarg  :b
      :reader   b
      :initform 2)))

(test sexp-grammar.smoke
  "Smoke test for `sexp-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar)
    (rules-test (grammar)

      ;; `structure'
      (let ((instance-1 (make-instance 'klass :a 1 :b 2))
            (instance-2 (make-instance 'klass :a 1 :b 3)))
        `((structure 'klass (a 1) (b 2))

          (1           (nil 1))
          (,instance-1 (t   ,instance-1))
          (,instance-2 (nil 3))))

      ;; `cons' is syntactic sugar for
      ;;
      ;;   (structure 'cons (car CAR-PATTERN) (cdr CDR-PATTERN))
      ;;
      ;; .
      '((cons a b)

        (1       (nil 1))
        ((1 . 2) (t   (1 . 2))))

      ;; `list' is syntactic sugar for
      ;;
      ;;   (as-list (seq SUB-EXPRESSIONS))
      ;;
      ;; .
      '((list a :b (* 'c))

        ((1 :b c c) (t   (1 :b c c) (1 :b c c)))
        ((1 :b c d) (nil (1 :b c d))))

      '((list* a :b (list (* 'c)))

        ((1 :b c c) (t   (1 :b c c) (1 :b c c)))
        ((1 :b c d) (nil (1 :b c d))))

      ;; `as-vector'
      '((vector a :b (* 'c))

        (#(1 :b c c) (t   #(1 :b c c) #(1 :b c c)))
        (#(1 :b c d) (nil #(1 :b c d))))

      ;; TODO these belong to base-grammar
      ;; unification test
      '((list x x)

        ((1 1) (t   (1 1) (1 1)))
        ((1 2) (nil (1 2))))

      '((list a (list b c) (list a b c))

        (#1=((1) (2 3) ((1) 2 3)) (t #1# #1#)))

      '((:guard symbolp)

        (1 (nil 1))
        (y (t   y y)))

      '((:guard (typep 'symbol))

        (1 (nil 1))
        (y (t   y y))))))
