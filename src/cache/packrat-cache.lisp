(cl:in-package #:parser.packrat.cache)

;;; Packrat cache
;;;
;;; A cache mapping tuples (input position, rule name, rule arguments)
;;; to parse results. Its purpose is avoiding multiple identical
;;; applications of rules. This can improve performance and act as the
;;; foundation of a framework for handling left recursion.
;;;
;;; Since reads from and writes to this cache can be a performance
;;; bottleneck, the implementation tries to be as runtime and memory
;;; efficient as possible. A two-level scheme maps the tuples
;;; mentioned above to parse results:
;;; 1. an array maps the input position to secondary structure
;;; 2. this structure maps the rule name and arguments to the cached
;;;    parse results
;;;
;;; The interesting part about 1. is not allocating an array of the
;;; same size as the input upfront while keeping lookup performance
;;; reasonable. This trade-off is achieved using a "chunk cache".
;;;
;;; The difficulty with 2. is the variety of scenarios that have to be
;;; supported efficiently w.r.t. memory and runtime. To address this
;;; issue, the secondary structure uses different representation
;;; depending on the situation:
;;;
;;; + If only a mapping from a single rule (optionally with arguments)
;;;   to the associated parse result has to be represented, a single
;;;   cons cell is used.
;;;
;;; + For a small number of mapping entries, the number of entries and
;;;   an alist are stored to represent the mapping.
;;;
;;; + In the (uncommon) case that more than a few entries have to be
;;;   stored, a hash-table is used.
;;;
;;; Switches between representation happen when entries are added.

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
