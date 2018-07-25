(cl:defpackage #:parser.packrat.cache
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:shadow
   #:stream)

  ;; Chunk cache protocol
  (:export
   #:make-chunk-cache
   #:find-chunk
   #:ensure-chunk)

  ;; Stream caches
  (:export
   #:make-octet-vector-stream-cache
   #:make-string-stream-cache)

  ;; Packrat cache protocol
  (:export
   #:cached
   #:cached/arguments)

  (:documentation
   "TODO"))
