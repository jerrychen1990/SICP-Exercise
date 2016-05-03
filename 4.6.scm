(load "core.scm")
(define (let? exp)
    (tagged-list? exp 'let))

(define (let-cond exp)
    (cadr exp))

(define (let-body exp)
    (cddr exp))

 (define (eval exp env)
     (display "exp: ")
     (display exp)
     (newline)
     (cond ((self-evaluating? exp) exp)
           ((variable? exp) (lookup-variable-value exp env))
           ((quoted? exp) (text-of-quoted exp))
           ((assignment? exp) (eval-assignment exp env))
           ((definition? exp) (eval-definition exp env))
           ((if? exp) (eval-if exp env))
           ((let? exp) (eval (let->combination exp) env))
           ((lambda? exp) (make-procedure (lambda-parameter exp)
                                          (lambda-body exp)
                                          env))
           ((begin? exp) (eval-sequence (begin-actions exp)
                                        env))
           ((cond? exp) (eval (cond->if exp) env))
           ((application? exp) (my-apply (eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
           (else (error "UNKNOWN EXPRESSION TYPE -- EVAL" exp))))

(define (let->combination exp)
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
(define input
    '(let ((a 3) (b 4)) (* a b)))

(eval input the-global-environment)






