(load "core.scm")
(define (and? exp)
    (tagged-list? exp 'and))

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
           ((lambda? exp) (make-procedure (lambda-parameter exp)
                                          (lambda-body exp)
                                          env))
           ((begin? exp) (eval-sequence (begin-actions exp)
                                        env))
           ((cond? exp) (eval (cond-if exp) env))
           ((and? exp) (eval-and exp env))
           ((application? exp) (my-apply (eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
           (else (error "UNKNOWN EXPRESSION TYPE -- EVAL" exp))))

(define (last-predict? predicts)
    (null? (cdr predicts)))

(define (first-predict predicts)
    (if (pair? predicts)
        (car predicts)
        'true))

(define (rest-predict predicts)
    (cdr predicts))      


(define (eval-and exp env)
    (let ((predicts (cdr exp)))
        (define (loop-predicts predicts)
            (if (last-predict? predicts)
                (eval (first-predict predicts) env)
                (if (eval (first-predict predicts) env)
                    (loop-predicts (rest-predict predicts))
                    false)))
        (loop-predicts predicts)))

(define (last-predict? predicts)
    (if (null? (cdr predicts))
         'true
         'false))


(define (and-if exp env)
    (let ((predicts (cdr exp)))
        (make-if (first-predict predicts)
                 (make-if (last-predict? predicts)
                          'true
                          (cons 'and (rest-predict predicts)))
                 'false)))

(define (eval-and exp env)
    (eval (and-if exp env) env))

;or 的情形同理

(load "start-loop.scm")

