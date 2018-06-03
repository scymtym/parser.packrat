(cl:defpackage #:parser.packrat
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:parser.packrat.base)

  (:local-nicknames
   (#:grammar #:parser.packrat.grammar))

  ;; Parsing protocol
  (:export
   #:parse)

  ;; Macros
  (:export
   #:defgrammar
   #:in-grammar

   #:defrule

   #:match #:ematch #:cmatch))
