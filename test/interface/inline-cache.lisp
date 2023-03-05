(cl:in-package #:parser.packrat.test)

(in-suite :parser.packrat.interface)

(test inline-cache.argument-count
  "Test invoking a rule with multiple argument counts."
  ;; Set up test grammar with `foo' test rule.
  (parser.packrat.grammar.test:grammar-test (grammar 'parser.packrat.grammar.sexp::sexp-grammar :name 'test)
    (eval '(parser.packrat:defrule (foo :grammar test) (a b) thing (list thing a b)))
    ;; Make cache and call `foo' rule with different (correct and
    ;; incorrect) argument counts.
    (let ((cache (make-instance 'parser.packrat::inline-cache)))
      (is (equal (values t '(:bar :a :b) '(:bar :a :b))
                 (funcall cache grammar '(foo :a :b) :bar)))
      (is (eql 1 (parser.packrat::entry-count cache)))
      (signals error (funcall cache grammar '(foo :a) :bar))
      (is (eql 2 (parser.packrat::entry-count cache))))))
