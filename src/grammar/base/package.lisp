;;;; package.lisp --- Package definition for the grammar.base module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.base
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.compiler)

  (:import-from #:parser.packrat.expression
   #:sub-expression
   #:sub-expressions)

  (:local-nicknames
   (#:bp      #:architecture.builder-protocol)

   (#:cache   #:parser.packrat.cache)
   (#:exp     #:parser.packrat.expression)
   (#:grammar #:parser.packrat.grammar)
   (#:env     #:parser.packrat.environment))

  ;; Concrete syntax
  (:export
   #:guard
   #:<- #:<<-
   #:must
   #:next-rule)

  ;; Expressions
  (:export
   #:anything-expression

   #:terminal-expression

   #:must-expression
   #:message

   #:not-expression
   #:and-expression
   #:or-expression

   #:constant-expression

   #:arguments

   #:rule-invocation-expression
   #:grammar
   #:rule

   #:next-rule-invocation-expression

   #:variable-reference-expression
   #:variable-same-expression
   #:set-expression
   #:push-expression

   #:transform-expression
   #:code

   #:predicate-expression
   #:predicate)

  ;; Grammar
  (:export
   #:base-grammar)

  (:export
   #:compile-comparison)) ; TODO used?
