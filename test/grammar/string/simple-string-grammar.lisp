(cl:in-package #:parser.packrat.grammar.string.test)

(in-suite :parser.packrat.grammar.string)

(test simple-string-grammar.smoke
  "Smoke test for the `simple-string-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar)

    (parser.packrat.grammar:ensure-rule
     'foo grammar
     :rule-class 'parser.packrat.grammar::rule
     :expression #1='(:seq #\f #\o #\o)
     :function   (compile
                  nil (parser.packrat.compiler:compile-rule
                       grammar 'foo '() (parser.packrat.grammar:parse-expression
                                         grammar `(:transform ,#1# :foo)))))

    (parser.packrat.grammar:ensure-rule
     'bar grammar
     :rule-class 'parser.packrat.grammar::rule
     :expression #2='(:seq #\b #\a #\r)
     :function   (compile
                  nil (parser.packrat.compiler:compile-rule
                       grammar 'bar '() (parser.packrat.grammar:parse-expression
                                         grammar `(:transform ,#2# :bar)))))

    (rules-test (grammar)
      '((or (:seq "foo" (* "ba") #\r)
            (:seq "foo" "baz"))

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
