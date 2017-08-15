(defsystem "parser.packrat.grammar.stream"
  :description      "Stream parsing for the parser.packrat system."
  :licence          "BSD" ; see COPYING file for details

  :author           "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer       "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage         "https://scymtym.github.io/parser.packrat"
  :bug-tracker      "https://github.com/scymtym/parser.packrat/issues"
  :source-control   (:git "https://github.com/scymtym/parser.packrat.git")

  :version          (:read-file-form "version-string.sexp")
  :depends-on       ("let-plus"
                     "utilities.print-items"

                     (:version "parser.packrat"       (:read-file-form "version-string.sexp"))
                     (:version "parser.packrat.cache" (:read-file-form "version-string.sexp")))

  :components       ((:module     "grammar-stream"
                      :pathname   "src/grammar/stream"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "environment")
                                   (:file       "stream-grammar")
                                   (:file       "compiler"))))

  :in-order-to      ((test-op (test-op "parser.packrat.grammar.stream/test"))))

(defsystem "parser.packrat.grammar.stream/test"
  :version    (:read-file-form "version-string.sexp")
  :depends-on ((:version "fiveam"                        "1.4")

               (:version "parser.packrat.grammar.stream" (:read-file-form "version-string.sexp"))

               (:version "parser.packrat/test"           (:read-file-form "version-string.sexp")))

  :components ((:module     "grammar-stream"
                :pathname   "test/grammar/stream"
                :serial     t
                :components ((:file       "package")
                             (:file       "stream-grammar"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:parser.packrat.grammar.stream.test '#:run-tests)))
