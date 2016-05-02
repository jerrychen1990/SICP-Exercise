(load "core.scm")

(define (=>? clause)
    (eq? (cadr clause) '=>))

(define (cond-action clause)
    (if (=>? clause)
        (list (caddr clause) (car clause))
        (cdr clause)))


(define (expand-clause clauses)
     (if (null? clauses)
         'false
         (let ((first (car clauses))
               (rest (cdr clauses)))
               (if (cond-else-clause? first)
                   (if (null? rest)
                       (sequence->exp (cond-action first))
                       (error "ELSE CLAUSE IS NOT LAST cond-if" clauses))
                   (make-if (cond-predicate first)
                            (sequence->exp (cond-action first))
                            (expand-clause rest))))))

(load "start-loop.scm")
