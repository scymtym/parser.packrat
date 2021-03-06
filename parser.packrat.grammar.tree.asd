(defsystem "parser.packrat.grammar.tree"
  :description      "Tree parsing for the parser.packrat system."
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

  :components       ((:module     "grammar-tree"
                      :pathname   "src/grammar/tree"
                      :serial     t
                      :components ((:file       "package")

                                   (:file       "expressions")
                                   (:file       "environment")
                                   (:file       "compiler")

                                   (:file       "concrete-syntax")

                                   (:file       "runtime"))))

  :in-order-to      ((test-op (test-op "parser.packrat.grammar.tree/test"))))

(defsystem "parser.packrat.grammar.tree/test"
  :version    (:read-file-form "version-string.sexp")
  :depends-on ((:version "fiveam"                      "1.4")

               (:version "parser.packrat.grammar.tree" (:read-file-form "version-string.sexp"))

               (:version "parser.packrat/test"         (:read-file-form "version-string.sexp")))

  :components ((:module     "grammar-tree"
                :pathname   "test/grammar/tree"
                :serial     t
                :components ((:file       "package")
                             (:file       "tree-grammar"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:parser.packrat.grammar.tree.test '#:run-tests)))
