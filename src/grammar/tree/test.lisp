(cl:in-package #:parser.packrat.grammar.tree)

;;; New test

(parser.packrat:defgrammar tree-test
  (:class parser.packrat.grammar.sexp:sexp-grammar))

(parser.packrat:in-grammar tree-test)

(parser.packrat:defrule find-bindings (operator)
    (ancestors
     (* (or (list* operator
                   (list (* (list (:<<- names)
                                  (:<<- values (:transform value (class-of value))))))
                   :any)
            :any)))
  (list names values))

(parser.packrat:parse
 '(find-bindings 'let*)
 '(let ((a 1))
   (progn
     (let ((b (1+ (let ((c 2))
                    (values 'let (let ())))))
           (let 'let))
       (foo let)))))


;;; Test 1

"//foo"

(funcall
 (compile nil `(lambda (value)
                 (let ((result))
                  ,(:ce (make-instance 'ancestors-expression
                                       :sub-expression (:parse '(* (or (parser.packrat.bootstrap::<<- result 4)
                                                                       parser.packrat.bootstrap::any))))
                        :grammar :sexp
                        :environment `(:value :value ,'value))
                  result)))
 1)


(compile-rule (make-instance 'sexp-grammar)
              (make-instance 'ancestor-expression
                             :sub-expression (:parse '(seq 1 (* 2)))))

;;; Test 2

(defmethod children ((node integer))
  (when (< node 10)
    (list (1+ node) (+ node 2))))

(labels ((visit (value cont)
           (print value)
           (funcall cont #'visit)))
  (funcall (depth-first-traverse 1) #'visit))

(test 1)

;;; Test 3

(defstruct node
  (id)
  (children '()))

(defmethod children ((node node))
  (node-children node))

(let* ((tree      (make-node :id 1 :children (list (make-node :id 2 :children (list (make-node :id 4)
                                                                                    (make-node :id 5)
                                                                                    (make-node :id 6)))
                                                   (make-node :id 3))))
       (generator (depth-first-traverse tree)))
  (labels ((visit (value cont)
             (print value)
             (funcall cont #'visit)))
    (funcall generator #'visit)))

(test)
