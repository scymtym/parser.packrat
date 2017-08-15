(cl:in-package #:parser.packrat.grammar.string)

;;; TODO this inheritance does not work properly
;;; it would be better to not repeat the slot here. but then we don't
;;; get a good constructor.
(exp:define-expression-class string-terminal (base:terminal-expression)
  (exp:value))
