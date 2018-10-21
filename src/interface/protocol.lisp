(cl:in-package #:parser.packrat)

;;; TODO maybe allow GRAMMAR being a grammar designator later?
(defun parse (expression input &key (grammar *grammar*))
  (parser.packrat.grammar:parse grammar expression input))


