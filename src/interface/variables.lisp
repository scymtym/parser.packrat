;;;; variables.lisp --- Variables defined in the interface module.
;;;;
;;;; Copyright (C) 2018-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.packrat)

(defvar *grammar*
  "Stores the current grammar.

The current grammar is used by the `parse' function if no grammar is
supplied.

The current grammar is used by the `defrule' macro if no grammar is
supplied.

The macro `in-grammar' changes the current grammar.")
