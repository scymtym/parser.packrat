;;;; concrete-syntax.lisp --- Meta-grammar for the grammar.string module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.string)

(parser.packrat:defgrammar meta-grammar
  (:class   parser.packrat.grammar.sexp:sexp-grammar)
  (:cached? nil)
  (:use     seq::meta-grammar
            base::meta-grammar))
(parser.packrat:in-grammar meta-grammar)

(parser.packrat:defrule base::expression (context)
  (or ((base::expression seq::meta-grammar) context)
      ((base::expression base::meta-grammar) context)))
