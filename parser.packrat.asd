(defsystem "parser.packrat"
  :description      "A Packrat parser for Common Lisp."
  :author           "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer       "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage         "https://scymtym.github.io/parser.packrat"
  :bug-tracker      "https://github.com/scymtym/parser.packrat/issues"
  :source-control   (:git "https://github.com/scymtym/parser.packrat.git")
  :licence          "BSD" ; see COPYING file for details

  :version          (:read-file-form "version.sexp")
  :depends-on       ("closer-mop"
                     "alexandria"
                     "let-plus"
                     "more-conditions"
                     "utilities.print-items"

                     "architecture.builder-protocol")

  :components       ((:module     "base"
                      :pathname   "src/base"
                      :components ((:file       "package")
                                   (:file       "types")))

                     (:module     "cache"
                      :pathname   "src/cache"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "chunk-cache")
                                   (:file       "stream-cache")
                                   (:file       "packrat-cache")))

                     (:module     "expression"
                      :pathname   "src/expression"
                      :serial     t
                      :components ((:file       "package")
                                   ; (:file       "types")
                                   (:file       "protocol")
                                   ; (:file       "variables")
                                   ; (:file       "conditions")
                                   (:file       "mixins")
                                   (:file       "macros")))

                     (:module     "environment"
                      :pathname   "src/environment"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "protocol")
                                   (:file       "macros")
                                   (:file       "environment")))

                     (:module     "compiler"
                      :pathname   "src/compiler"
                      :depends-on ("expression" "environment")
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "protocol" )))

                     (:module     "grammar"
                      :pathname   "src/grammar"
                      :depends-on ("expression")
                      :components ((:file      "package")
                                   (:file      "conditions")
                                   (:file      "protocol")
                                   (:file      "mixins")
                                   (:file      "rule")))

                     (:module     "grammar-base"
                      :pathname   "src/grammar/base"
                      :depends-on ("expression" "grammar")
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "expressions")
                                   (:file       "base-grammar")
                                   (:file       "compiler")))

                     (:module     "grammar-sequence"
                      :pathname   "src/grammar/sequence"
                      :depends-on ("expression" "grammar" "grammar-base")
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "expressions")
                                   (:file       "mixins")
                                   (:file       "environment")
                                   (:file       "sequence-grammar")
                                   (:file       "compiler")))

                     (:module     "grammar-sexp"
                      :pathname   "src/grammar/sexp"
                      :depends-on ("expression"
                                   "grammar"
                                   "grammar-base"
                                   "grammar-sequence")
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "expressions")
                                   (:file       "sexp-grammar")
                                   (:file       "compiler")))

                     (:module     "bootstrap"
                      :pathname   "src/bootstrap"
                      :depends-on ("expression")
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "bootstrap")))

                     (:module     "interface"
                      :pathname   "src"
                      :depends-on ("base")
                      :serial     t
                      :components ((:file       "package")
                                   ; (:file       "protocol")
                                   (:file       "macros")))

                     (:module     "examples"
                      :components (; (:static-file "sexp.lisp")
                                   ; (:static-file "symbol-table.lisp")
                                   ; (:static-file "left-recursion.lisp")
                                   ;  (:static-file "function-terminals.lisp")
                                   (:static-file "lambda-lists")
                                   (:static-file "lambda-calculus")
                                   ))

                     (:static-file "README.org"))

  ; :in-order-to      ((test-op (test-op :parser.packrat/tests)))
  )

; (defsystem :parser.packrat/test)
