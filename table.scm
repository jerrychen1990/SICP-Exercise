(define (look-up key table)
    (let ((record (assoc key (cdr table))))
         (if record
             (cdr record)
             false)))

(define (assoc key records)
    (cond ((null? records) false)
          ((eq? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
        

(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
         (if record
             (set-cdr! record value)
             (set-cdr! table (cons (cons key value) (cdr table)))))
    'OK)

(define (make-table)
    (list '*table*))

(define (look-up2 k1 k2 table)
    (let ((sub-table (assoc k1 (cdr table))))
         (if sub-table
             (look-up k2 sub-table)
             false)))

(define (insert2! k1 k2 value table)
    (let ((sub-table (assoc k1 (cdr table))))
         (if sub-table
             (insert! k2 value sub-table)
             (set-cdr! table (cons (list k1 (cons k2 value))
                                   (cdr table)))))
    'OK)

(define (make-table2)
    (let ((local-table (list '*table*)))
        (define (lookup k1 k2)
                (look-up2 k1 k2 local-table))
        (define (insert! k1 k2 value)
                (insert2! k1 k2 value local-table))
        (define (dispatch m)
                (cond ((eq? m 'lookup-proc) lookup)
                      ((eq? m 'insert-proc) insert!)
                      (else (error "UNKNOWN OPERATION --TABLE" m))))
        dispatch))

(define operation-table (make-table2))

(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))










