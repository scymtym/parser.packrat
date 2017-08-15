(defsystem "parser.packrat.grammar.string"
  :description      "String parsing for the parser.packrat system."
  :licence          "BSD" ; see COPYING file for details

  :author           "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer       "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage         "https://scymtym.github.io/parser.packrat"
  :bug-tracker      "https://github.com/scymtym/parser.packrat/issues"
  :source-control   (:git "https://github.com/scymtym/parser.packrat.git")


  :version          (:read-file-form "version.sexp")
  :depends-on       ("let-plus"
                     "utilities.print-items"

                     (:version "parser.packrat"       (:read-file-form "version.sexp"))
                     ; (:version "parser.packrat.cache" (:read-file-form "version.sexp"))
                     )

  :components       ((:module     "grammar-string"
                      :pathname   "src/grammar/string"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "expressions")
                                   (:file       "simple-string-grammar")
                                   (:file       "prepare")
                                   (:file       "compiler")

                                   (:file       "runtime"))))

  :in-order-to      ((test-op (test-op :parser.packrat.grammar.string/test))))

(defsystem :parser.packrat.grammar.string/test
  :version          (:read-file-form "version.sexp")
  :depends-on       ((:version "fiveam"                        "1.4")
                     (:version "parser.packrat.grammar.string" (:read-file-form "versions.sexp")))

  :components       ((:module     "grammar-string"
                      :pathname   "test/grammar/string"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "simple-string-grammar"))))

  :perform          (test-op (operation component)
                      (uiop:symbol-call '#:parser.packrat.grammar.string.test '#:run-tests)))
