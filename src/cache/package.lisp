(cl:defpackage #:parser.packrat.cache
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:shadow
   #:stream)

  ;; Chunk cache protocol
  (:export
   #:make-chunk-cache
   #:find-chunk
   #:ensure-chunk)

  ;; Packrat cache protocol
  (:export
   #:cached
   #:cached/arguments)

  (:documentation
   "TODO"))
