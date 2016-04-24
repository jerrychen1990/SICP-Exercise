;eval an expression
(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quoted exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp) (make-procedure (lambda-parameter exp)
                                         (lambda-body exp)
                                         evv))
          ((begin? exp) (eval-sequence (begin-actions exp)
                                       env))
          ((cond? exp) (eval (cond-if exp) env))
          ((application? exp) (apply (eval (operator exp) env)
                                     (list-of-values (operands exp))
                                     env))
          (else (error "UNKNOWN EXPRESSION TYPE -- EVAL" exp))))

;apply arguments to procedure
(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((conpound-procedure? procedure)
           (eval-sequence (procedure-body procedure)
                          (extend-environment 
                            procedure-parameter procedure
                            arguments
                            procedure-environment procedure)))
          (else (error "UNKOWN PROCUDURE TYPE -- APPLY" procedure))))

;eval all parameters of exp and generate a list of values
(define (list-of-values exp env)
        (if (no-oprands? exp)
            '()
            (cons (eval (first-oprands exp) env)
                  (list-of-values (rest-oprands exp) env))))

;eval if expression
(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
               (eval (if-conquent exp) env)
               (eval (if-alternative exp) env)))

;eval a sequence of expressions
(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval exps env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exp exps) env))))

;assignment operation
(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (assignment-value exp))
    'OK)

;definition operation
(define (eval-definition exp env)
    (define-variable-value! (assignment-variable exp)
                            (assignment-value exp))
    'OK)







