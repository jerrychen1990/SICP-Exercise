(load "4.6.scm")

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
           ((let? exp) (eval (let->combination exp) env))
           ((let*? exp) (eval (let*->let exp) env))
           ((cond? exp) (eval (cond->if exp) env))
           ((application? exp) (my-apply (eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
           (else (error "UNKNOWN EXPRESSION TYPE -- EVAL" exp))))


(define (let*? exp)
    (tagged-list? exp 'let*))

(define (let*-cond exp)
    (cadr exp))

(define (let*-body exp)
    (cddr exp))

(define (let*->let exp)
    (let ((let*-cond (let*-cond exp))
          (let*-body (let*-body exp)))
          (define (loop-cond cond-list body)
                  (display "cond-list: ")
                  (display-ln cond-list)
                  (display "cond-body ")
                  (display-ln body)
                  (if (null? cond-list)
                      body
                      (list (make-let (list (car cond-list)) 
                                (loop-cond (cdr cond-list) body)))))
          (car (loop-cond let*-cond let*-body))))



;test case
(define input
    '(let* ((a 3) (b (+ a 4))) (* a b)))

(eval input the-global-environment)




