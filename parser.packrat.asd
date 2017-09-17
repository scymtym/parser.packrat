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

                     (:static-file "README.org")))