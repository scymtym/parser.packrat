(cl:defpackage #:parser.packrat.environment
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  ;; Environment lookup protocol
  (:export
   #:lookup) ; also `setf'

  ;; Environment hierarchy protocol
  (:export
   #:parent
   #:root
   #:depth)

  (:export
   #:state-variables    #:state-variables/plist
   #:position-variables #:position-variables/plist)

  ;; Environment manipulation protocol
  (:export
   #:environment-binding
   #:environment-at)

  ;; Base environment
  (:export
   #:environment)

  ;; Value environment protocol and class
  (:export
   #:value
   #:environment-carrying

   #:value-environment)

  ;; Macros
  (:export
   #:define-state-methods))
