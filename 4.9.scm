(load "4.6.scm")
(define (while? exp)
    (tagged-list? exp 'while))
(define (cond-init exp)
    (cadr exp))
(define (cond-while exp)
    (caddr exp))
(define (cond-body exp)
    (cdddr exp))
(define (while-if exp)
    (make-let (cond-init exp)
        (make-if (cond-while exp)
                 'true
                 (list 'begin (cond-body exp) exp))))




;eval an expression
 (define (eval exp env)
     (display-ln "exp: ")
     (display-ln exp)
     (cond ((self-evaluating? exp) exp)
           ((variable? exp) (lookup-variable-value exp env))
           ((quoted? exp) (text-of-quoted exp))
           ((assignment? exp) (eval-assignment exp env))
           ((definition? exp) (eval-definition exp env))
           ((if? exp) (eval-if exp env))
           ((lambda? exp) (make-procedure (lambda-parameter exp)
                                          (lambda-body exp)
                                          env))
           ((begin? exp) (eval-sequence (begin-actions exp)
                                        env))
           ((while? exp) (eval (while-if exp) env))
           ((cond? exp) (eval (cond->if exp) env))
           ((application? exp) (my-apply (eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
           (else (error "UNKNOWN EXPRESSION TYPE -- EVAL" exp))))




(define input
    '(while ((tmp 1)) (< tmp 10) (set! tmp (+ tmp 1)) (display tmp)))

(eval input the-global-environment)

