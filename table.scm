(define (look-up key table)
    (let ((record (assoc key (cdr table))))
         (if record
             (cdr record)
             false)))

(define (assoc key records)
    (cond ((null? records) false)
          ((eq? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
        


