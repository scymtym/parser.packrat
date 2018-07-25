;;;; sexp-grammar.lisp --- Unit tests for the sexp-grammar class.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
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

(parser.packrat:defgrammar :test
  (:class parser.packrat.grammar.sexp::sexp-grammar))
(parser.packrat:in-grammar :test)
(parser.packrat:defrule test ()
    (list a (list b c) (list a b c))
  a)

(parser.packrat:parse '(test) '((1) (2 3) ((1) 2 3)))

(parser.packrat:defgrammar :test2
  (:class parser.packrat.grammar.sequence::sequence-grammar))
(parser.packrat:in-grammar :test2)
(parser.packrat:defrule test ()
    (:seq a a)
  a)



(test sexp-grammar.smoke
  "Smoke test for `sexp-grammar' grammar class."

  (grammar-test (grammar 'parser.packrat.grammar.sexp:sexp-grammar :name :test)
    (rules-test (grammar)
      ;;
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

      ;; `as-list'
      '((list a :b (* 'c))

        ((1 :b c c) (t   (1 :b c c)))
        ((1 :b c d) (nil (1 :b c d))))

      ;; `as-vector'
      '((vector a :b (* 'c))

        (#(1 :b c c) (t   #(1 :b c c)))
        (#(1 :b c d) (nil #(1 :b c d))))

      ;; TODO these belong to base-grammar
      '((list x x)

        ((1 1) (t   (1 1)))
        ((1 2) (nil (1 2))))

      '((:guard x symbolp)

        (1 (nil 1)))

      '((:guard x (typep 'symbol))

        (1 (nil 1))
        (y (t   y))))))
