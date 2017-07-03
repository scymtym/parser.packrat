(cl:in-package #:parser.packrat.cache)

;;; Packrat cache
;;;
;;; TODO explain

;; TODO compare this against
;; singleton-cons ~> small vector with linear probing ~> hash-table
(macrolet
    ((define-cached (name arguments?)
       `(defun ,name (symbol position ,@(when arguments? '(arguments)) cache)
          (let* ((divisor    (chunk-cache-divisor cache))
                 (chunk      (find-chunk cache position))
                 (position-2 (ldb (byte divisor 0) position))
                 (cell       (when chunk
                               (aref chunk position-2))))
            (cond
              ((null cell)
               nil)
              ((not (consp cell))
               ,(if arguments?
                    `(let ((key (cons symbol arguments)))
                       (declare (dynamic-extent key))
                       (gethash key cell))
                    `(gethash symbol cell)))
              ((not (consp (cdr cell)))
               (when ,(if arguments?
                          `(and (eq    (caar cell) symbol)
                                (equal (cdar cell) arguments))
                          `(eq (car cell) symbol))
                 (cdr cell)))
              (t
               ,(if arguments?
                    `(let ((key (cons symbol arguments)))
                       (declare (dynamic-extent key))
                       (assoc-value (cdr cell) key :test #'equal))
                    `(assoc-value (cdr cell) symbol :test #'eq))))))))
  (define-cached cached           nil)
  (define-cached cached/arguments t))

(macrolet
    ((define-setf-cached (name arguments?)
       `(defun ,name (result symbol position ,@(when arguments? '(arguments)) cache)
          (let* ((divisor    (chunk-cache-divisor cache))
                 (chunk      (flet ((%make-chunk ()
                                      (make-chunk divisor)))
                               (declare (dynamic-extent #'%make-chunk))
                               (ensure-chunk cache position #'%make-chunk)))
                 (position-2 (ldb (byte divisor 0) position))
                 (cell       (aref chunk position-2)))
            (cond

              ;; No entry => Create a singleton entry using one CONS.
              ((null cell)
               (setf (aref chunk position-2)
                     (cons ,(if arguments?
                                `(cons symbol arguments)
                                `symbol)
                           result)))

              ;; Not a CONS => Has to be a hash-table. Store the result.
              ((not (consp cell))
               (setf (gethash ,(if arguments?
                                   `(cons symbol arguments)
                                   `symbol)
                              cell)
                     result))

              ;; A singleton CONS => Maybe extend to a list of the form
              ;;
              ;;   (LENGTH . (KEY1 . RESULT1) (KEY2 . RESULT2) ...)
              ;;
              ;; where LENGTH is initially 2 after upgrading from a singleton
              ;; CONS.
              ((not (consp (cdr cell)))
               (if ,(if arguments?
                        `(and (eq (caar cell) symbol)
                              (equal (cdar cell) arguments))
                        `(eq (car cell) symbol))
                   (setf (cdr cell) result)
                   (setf (aref chunk position-2)
                         (cons 2 (acons ,(if arguments?
                                             `(cons symbol arguments)
                                             `symbol)
                                        result
                                        (list cell))))))

              ;; A list with leading length as described above.
              (t
               (let ((count   (car cell)) ; note: faster than DESTRUCTURING-BIND
                     (entries (cdr cell)))
                 (declare (type (integer 0 16) count))
                 (cond
                   ;; When there is an entry for RESULT, update it.
                   ((when-let ((entry ,(if arguments?
                                           `(let ((key (cons symbol arguments)))
                                              (assoc key entries :test #'equal))
                                           `(assoc symbol entries :test #'eq))))
                      (setf (cdr entry) result)
                      t))
                   ;; When there are 16 entries and we need another one,
                   ;; upgrade to HASH-TABLE, then store the new entry.
                   ((= 16 count)
                    (let ((table (setf (aref chunk position-2)
                                       (alist-hash-table entries :test #'eq))))
                      (setf (gethash ,(if arguments?
                                          `(cons symbol arguments)
                                          `symbol)
                                     table)
                            result)))
                   ;; When there are less than 16 entries and we need
                   ;; another one, increase the counter and add an
                   ;; entry.
                   (t
                    (incf (car cell))
                    (setf (cdr cell)
                          (acons ,(if arguments?
                                      `(cons symbol arguments)
                                      `symbol)
                                 result entries))))))))
          result)))
  (define-setf-cached (setf cached)           nil)
  (define-setf-cached (setf cached/arguments) t))
