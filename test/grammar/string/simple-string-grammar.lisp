;;;; simple-string-grammar.lisp --- Tests for simple-string-grammar class.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.string.test)

(in-suite :parser.packrat.grammar.string)

(test simple-string-grammar.smoke
  "Smoke test for the `simple-string-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar)

    (grammar:ensure-rule
     'foo grammar
     :expression #1='(seq #\f #\o #\o)
     :function   (compile
                  nil (c:compile-rule
                       grammar 'foo '() (grammar:parse-expression
                                         grammar `(:transform ,#1# :foo)))))

    (grammar:ensure-rule
     'bar grammar
     :expression #2='(seq #\b #\a #\r)
     :function   (compile
                  nil (c:compile-rule
                       grammar 'bar '() (grammar:parse-expression
                                         grammar `(:transform ,#2# :bar)))))

    (rules-test (grammar)
      '((or (seq "foo" (* "ba") #\r)
            (seq "foo" "baz"))

        ("foor"     (t   4))
        ("foobar"   (t   6))
        ("foobabar" (t   8))
        ("foobaz"   (t   6))
        ("foobabaz" (nil 0)))


      ;; TODO move this elsewhere; this is from a regression.
      ;;
      '((* (or (foo) (bar)))

        ("foo"    (t   3))
        ("bar"    (t   3))
        ("baz"    (t   0))
        ("foobar" (t   6))))))
