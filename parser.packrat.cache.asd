(defsystem "parser.packrat.cache"
  :description      "Caches for the parser.packrat system."
  :licence          "BSD" ; see COPYING file for details

  :author           "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer       "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage         "https://scymtym.github.io/parser.packrat"
  :bug-tracker      "https://github.com/scymtym/parser.packrat/issues"
  :source-control   (:git "https://github.com/scymtym/parser.packrat.git")

  :version          (:read-file-form "version-string.sexp")
  :depends-on       ("alexandria"
                     "let-plus"
                     "more-conditions")

  :components       ((:module     "cache"
                      :pathname   "src/cache"
                      :serial     t
                      :components ((:file       "package")
                                   (:file       "chunk-cache")
                                   (:file       "stream-cache")
                                   (:file       "packrat-cache"))))

  :in-order-to      ((test-op (test-op "parser.packrat.cache/test"))))

(defsystem "parser.packrat.cache/test"

  :version    (:read-file-form "version-string.sexp")
  :depends-on ((:version "fiveam"               "1.4")

               (:version "parser.packrat.cache" (:read-file-form "version-string.sexp")))

  :components ()

  :perform    (test-op (operation component)
                ))
