;;;; expressions.lisp --- Expressions provided by the grammar.string module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat.grammar.string)

(exp:define-expression-class string-terminal (base:terminal-expression)
  ())
