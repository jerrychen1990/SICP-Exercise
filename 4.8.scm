(load "4.6.scm")

(define (let->combination exp)
    (if (= (length exp) 4)
        (let->combination-new exp)
        (let->combination-old exp)))

(define (let-proc exp)
    (cadr exp))

(define (let-cond-new exp)
    (caddr exp))

(define (let-body-new exp)
    (cadddr exp))

(define (let->combination-new exp)
    (let ((let-proc (let-proc exp))
          (let-cond (let-cond-new exp))
          (let-body (let-body-new exp)))
          (let ((parameters (map car let-cond))
                (arguments (map cadr let-cond)))
               (list 'begin
                     (list 'define 
                           (cons let-proc parameters)
                           let-body)
                     (cons let-proc arguments)))))         

(define (let->combination-old exp)
     (let ((let-cond (let-cond exp))
           (let-body (let-body exp)))
           (let ((variables (map car let-cond))
                 (values (map cadr let-cond)))
                 (display-ln variables)
                 (display-ln values)
                 (display-ln let-body)
                 ;(display-ln (make-lambda variables let-body)))))
                 (cons (make-lambda variables let-body) values))))

;test case
(define input '(let fib-iter ((a 1) (b 0) (count 10)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))

(eval input the-global-environment)




