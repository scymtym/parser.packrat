(cl:in-package #:parser.packrat.grammar.stream.test)

(in-suite :parser.packrat.grammar.stream)

(test stream-grammar.smoke
  "Smoke test for the `stream-grammar' grammar class."

  (grammar-test (grammar 'mock-grammar)
    (rules-test (grammar)
      `((or (:seq #\f #\o #\o (* (:seq #\b #\a)) #\r)
            (:seq (:seq #\f #\o #\o) (:seq #\b #\a #\z)))

        (,(make-string-input-stream "foor")     (t   4))
        (,(make-string-input-stream "foobar")   (t   6))
        (,(make-string-input-stream "foobabar") (t   8))
        (,(make-string-input-stream "foobaz")   (t   6))
        (,(make-string-input-stream "foobabaz") (nil 0))))))
