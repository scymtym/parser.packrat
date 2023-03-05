;;;; protocol.lisp --- Protocol provided by the interface module.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat)

;;; TODO maybe allow GRAMMAR being a grammar designator later?
(defun parse (expression input &key (grammar *grammar*))
  (parser.packrat.grammar:parse grammar expression input))
