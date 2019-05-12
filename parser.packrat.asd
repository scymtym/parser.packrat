(defsystem "parser.packrat"
  :description      "A Packrat parser for Common Lisp."
  :license          "BSD" ; see COPYING file for details

  :author           "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer       "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage         "https://scymtym.github.io/parser.packrat"
  :bug-tracker      "https://github.com/scymtym/parser.packrat/issues"
  :source-control   (:git "https://github.com/scymtym/parser.packrat.git")

  :version          (:read-file-form "version-string.sexp")
  :depends-on       ((:require "sb-cltl2")

                     "closer-mop"
                     "alexandria"
                     "let-plus"
                     "more-conditions"
                     "utilities.print-items"

                     "architecture.builder-protocol"

                     (:version "parser.packrat.cache" (:read-file-form "version-string.sexp")))

  :components       ((:module     "base"
                      :pathname   "src/base"
                      :components ((:file       "package")
                                   (:file       "types")))

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
                                   (:file       "util")
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

                                   (:file       "prepare")
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

                                   (:file       "prepare")
                                   (:file       "compiler")))

                     (:module     "bootstrap"
                      :pathname   "src/bootstrap"
                      :depends-on ("expression")
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "parse")))

                     (:module     "interface"
                      :pathname   "src/interface"
                      :depends-on ("base")
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "variables")
                                   (:file       "protocol")
                                   (:file       "macros")

                                   (:file       "inline-cache")
                                   (:file       "compilation")))

                     (:module     "grammar-base-concrete-syntax"
                      :pathname   "src/grammar/base"
                      :depends-on ("interface")
                      :components ((:file       "concrete-syntax")))

                     (:module     "grammar-sequence-concrete-syntax"
                      :pathname   "src/grammar/sequence"
                      :depends-on ("interface"
                                   "grammar-base-concrete-syntax")
                      :components ((:file       "concrete-syntax")))

                     (:module     "grammar-sexp-concrete-syntax"
                      :pathname   "src/grammar/sexp"
                      :depends-on ("interface"
                                   "grammar-base-concrete-syntax"
                                   "grammar-sequence-concrete-syntax")
                      :components ((:file       "concrete-syntax")))

                     (:module     "examples"
                      :components (; (:static-file "sexp.lisp")
                                   ; (:static-file "symbol-table.lisp")
                                   ; (:static-file "left-recursion.lisp")
                                   ;  (:static-file "function-terminals.lisp")
                                   (:static-file "lambda-lists")
                                   (:static-file "lambda-calculus")
                                   ))

                     (:static-file "version-string.sexp")
                     (:static-file "README.org"))

  :perform          (load-op :after (operation component)
                      (let ((symbol (find-symbol (string '#:*bootstrapping*)
                                                 '#:parser.packrat.grammar)))
                        (setf (symbol-value symbol) nil)))

  :in-order-to      ((test-op (test-op "parser.packrat/test"))))

(defsystem "parser.packrat/test"
  :description "Unit tests for the parser.packrat system."

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:require "sb-cltl2")

                (:version "fiveam"         "1.4")

                (:version "parser.packrat" (:read-file-form "version-string.sexp")))

  :components  ((:module     "grammar"
                 :pathname   "test/grammar"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")))

                (:module     "grammar-base"
                 :depends-on ("grammar")
                 :pathname   "test/grammar/base"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "concrete-syntax")
                              (:file       "base-grammar")))

                (:module     "grammar-sequence"
                 :depends-on ("grammar")
                 :pathname   "test/grammar/sequence"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "concrete-syntax")
                              (:file       "sequence-grammar")))

                (:module     "grammar-sexp"
                 :depends-on ("grammar")
                 :pathname   "test/grammar/sexp"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "concrete-syntax")
                              (:file       "sexp-grammar"))))

  :perform     (test-op (operation component)
                 ;; (uiop:symbol-call '#:parser.packrat.grammar.test          '#:run-tests)
                 (uiop:symbol-call '#:parser.packrat.grammar.base.test     '#:run-tests)
                 (uiop:symbol-call '#:parser.packrat.grammar.sequence.test '#:run-tests)
                 (uiop:symbol-call '#:parser.packrat.grammar.sexp.test     '#:run-tests)))
