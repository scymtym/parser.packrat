(cl:in-package #:parser.packrat)

;;; Compiler macro

(maybe-partially-evaluate-rule-invocation '(list 'foo a 2))

(defun f (state input)
  (parse (list 'cleaal.reader::string state) input :grammar 'cleaal.reader::cleaal))

(f '() "\"foo\"")

(defun g (state input)
  (parse (list 'cleaal.reader::string '()) input :grammar 'cleaal.reader::cleaal))

;;; Inline cache

(let ((cache (make-instance 'inline-cache)))
  (list
   (multiple-value-list
    (architecture.builder-protocol:with-builder ('list)
      (funcall cache *grammar* '(cleaal.reader::string 'nil) "\"aaa\"")))
   (multiple-value-list
    (architecture.builder-protocol:with-builder ('list)
      (funcall cache *grammar* '(cleaal.reader::string 'nil) "\"aaa\"")))
   (multiple-value-list
    (architecture.builder-protocol:with-builder ('list)
      (funcall cache *grammar* '(cleaal.reader::string '(nil)) "\"aaa\"")))))

(let ((cache (make-instance 'inline-cache)))
  (list
   (multiple-value-list
    (architecture.builder-protocol:with-builder ('list)
      (funcall cache *grammar* '(:transform (* (:<<- strings (cleaal.reader::string 'nil))) strings) "\"aaa\"")))
   (multiple-value-list
    (architecture.builder-protocol:with-builder ('list)
      (funcall cache *grammar* '(:transform (* (:<<- strings (cleaal.reader::string 'nil))) strings) "\"aaa\"")))
   (multiple-value-list
    (architecture.builder-protocol:with-builder ('list)
      (funcall cache *grammar* '(:transform (* (:<<- strings (cleaal.reader::string '(nil)))) strings) "\"aaa\"")))))
