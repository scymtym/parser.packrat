;;;; package.lisp --- Package definition for the base module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.packrat.base
  (:use
   #:cl)

  ;; Types
  (:export
   #:grammar-designator

   #:rule-designator))
