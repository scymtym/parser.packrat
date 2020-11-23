;;;; package.lisp --- Package definition for the grammar.sequence module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.grammar.sequence
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.compiler)

  (:local-nicknames
   (#:bp      #:architecture.builder-protocol)

   (#:exp     #:parser.packrat.expression)
   (#:env     #:parser.packrat.environment)

   (#:grammar #:parser.packrat.grammar)
   (#:base    #:parser.packrat.grammar.base))

  (:import-from #:parser.packrat.grammar.base
   #:<- #:<<- #:guard #:must)

  ;; Concrete syntax
  (:export
   #:seq #:?
   #:bounds)

  ;; Expressions
  (:export
   #:min-repetitions
   #:max-repetitions

   #:repetition-expression

   #:sequence-expression)

  ;; Sequence environment protocol
  (:export
   #:sequence*
   #:position*
   #:end

   #:sequence-environment)

  ;; List environment protocol and class
  (:export
   #:tail

   #:list-environment)

  ;; Vector environment class
  (:export
   #:vector-environment)

  ;; Sequence grammar protocol
  (:export
   #:sequence-type
   #:element-type
   #:index-type)

  ;; Mixins
  (:export
   #:sequential-grammar-mixin
   #:sequential-environment-mixin)

  ;; Sequence grammar
  (:export
   #:sequence-grammar))
