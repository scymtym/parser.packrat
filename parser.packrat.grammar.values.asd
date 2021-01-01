;;;; parser.packrat.grammar.values.lisp --- System definition for the grammar.values module.
;;;;
;;;; Copyright (C) 2017-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "parser.packrat.grammar.values"
  :description      "Values parsing for the parser.packrat system."
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

  :components       ((:module     "grammar-values"
                      :pathname   "src/grammar/values"
                      :components ((:file       "package")
                                   (:file       "environment")
                                   (:file       "compiler"))))

  :in-order-to      ((test-op (test-op "parser.packrat.grammar.values/test"))))

(defsystem "parser.packrat.grammar.values/test"
  :version    (:read-file-form "version-string.sexp")
  :depends-on ((:version "fiveam"                        "1.4")

               (:version "parser.packrat.grammar.values" (:read-file-form "version-string.sexp"))

               (:version "parser.packrat/test"           (:read-file-form "version-string.sexp")))

  :components ((:module     "grammar-values"
                :pathname   "test/grammar/values"
                :serial     t
                :components ((:file       "package")
                             (:file       "values-grammar"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:parser.packrat.grammar.values.test '#:run-tests)))
