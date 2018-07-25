(cl:in-package #:parser.packrat.grammar.string.test)

(in-suite :parser.packrat.grammar.string)

(test simple-string-grammar.smoke
  "Smoke test for the `simple-string-grammar' grammar class."

  (grammar-test (grammar 'parser.packrat.grammar.string:simple-string-grammar :name :test)
    (rules-test (grammar)
      '((or (:seq "foo" (* "ba") #\r)
            (:seq "foo" "baz"))

        ("foor"     (t   4))
        ("foobar"   (t   6))
        ("foobabar" (t   8))
        ("foobaz"   (t   6))
        ("foobabaz" (nil 0))))))
