(cl:in-package #:parser.packrat.grammar.string)

(defun %string= (string1 string2 start1 end1)
  (string= string1 string2 :start1 start1 :end1 end1))
(define-compiler-macro %string= (string1 string2 start1 end1)
  (let ((value)
        (length))
    (if (and (constantp string2)
             (<= (setf length (length (setf value (eval string2)))) 5))
        `(locally
             ; #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
             (and ,@(loop :for i :below length
                          :collect `(char= (aref ,string1 (+ ,start1 ,i))
                                           ,(aref string2 i)))))
        `(string= ,string1 ,string2 :start1 ,start1 :end1 ,end1))))
