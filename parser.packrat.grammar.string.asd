;;;; parser.packrat.grammar.string.lisp --- System definition for the grammar.string module.
;;;;
;;;; Copyright (C) 2017, 2018, 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "parser.packrat.grammar.string"
  :description      "String parsing for the parser.packrat system."
  :license          "BSD" ; see COPYING file for details

  :author           "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer       "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage         "https://scymtym.github.io/parser.packrat"
  :bug-tracker      "https://github.com/scymtym/parser.packrat/issues"
  :source-control   (:git "https://github.com/scymtym/parser.packrat.git")

  :version          (:read-file-form "version-string.sexp")
  :depends-on       ("let-plus"
                     "utilities.print-items"

                     (:version "parser.packrat" (:read-file-form "version-string.sexp")))

  :components       ((:module     "grammar-string"
                      :pathname   "src/grammar/string"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "expressions")
                                   (:file       "simple-string-grammar")
                                   (:file       "prepare")
                                   (:file       "compiler")

                                   (:file       "runtime")

                                   (:file       "concrete-syntax"))))

  :in-order-to      ((test-op (test-op "parser.packrat.grammar.string/test"))))

(defsystem "parser.packrat.grammar.string/test"
  :version          (:read-file-form "version-string.sexp")
  :depends-on       ((:version "fiveam"                        "1.4")

                     (:version "parser.packrat.grammar.string" (:read-file-form "version-string.sexp"))

                     (:version "parser.packrat/test"           (:read-file-form "version-string.sexp")))

  :components       ((:module     "grammar-string"
                      :pathname   "test/grammar/string"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "simple-string-grammar"))))

  :perform          (test-op (operation component)
                      (uiop:symbol-call '#:parser.packrat.grammar.string.test '#:run-tests)))
