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
                                     (list-of-values (operands exp) env)
                                     env))
          (else (error "UNKNOWN EXPRESSION TYPE -- EVAL" exp))))

;apply arguments to procedure
(define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((conpound-procedure? procedure)
           (eval-sequence (procedure-body procedure)
                          (extend-environment 
                           (procedure-parameter procedure)
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
               (eval (if-consequent exp) env)
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


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
        (symbol? exp))

(define (quoted? exp)
        (tagged-list? exp 'quote))

(define (text-of-quoted exp)
        (cadr exp))

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        true))

(define (assignment? exp)
    (tagged-list? exp 'set!))

(define (assignment-variable exp)
    (car exp))

(define (assignment-value exp)
    (caddr exp))

(define (definition? exp)
    (tagged-list? exp 'define))

(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambada (cdadr exp) (cddr exp))))

(define (lambda? exp)
    (tagged-list? exp 'lambda))

(define (lambda-parameter exp)
    (cadr exp))

(define (lambda-body exp)
    (cddr exp))

(define (make-lambada parameter body)
    (cons 'lambda (cons parameter body)))

(define (if? exp)
    (tagged-list? exp 'if))

(define (if-predicate exp)
    (cadr exp))

(define (if-consequent exp)
    (caddr exp))

(define (if-predicate exp)
    (if (null? (cdddr exp))
        'false
        (cdddr exp)))

(define (make-if predicate consequent altenative)
    (list 'if predicate consequent altenative))

(define (begin? exp)
    (tagged-list? exp 'begin))

(define (begin-actions exp)
    (cdr exp))

(define (last-exp? seq)
    (null? (cdr seq)))

(define (first-exp seq)
    (car seq))

(define (rest-exp seq)
    (cdr seq))

(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

(define (make-begin seq)
    (list 'begin seq))

(define (application? exp)
    (pair? exp))

(define (operator exp)
    (car exp))

(define (operands exp)
    (cdr exp))

(define (no-oprands? ops)
    (null? ops))

(define (first-oprands ops)
    (car ops))

(define (rest-oprands ops)
    (cdr ops))

;cond procedures
(define (cond? exp)
    (tagged-list? exp 'cond))

(define (cond-clauses exp)
    (cdr exp))

(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
    (car clause))

(define (cond-action clause)
    (cdr clause))

(define (cond->if exp)
    (expand-clause (cond-clauses exp)))

(define (expand-clause clauses)
    (if (null? clauses)
        'false
        (let ((first (car clause))
              (rest (cdr clause)))
             (if (cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-action first))
                     (error "ELSE CLAUSE IS NOT LAST cond-if" clause))
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-action first))
                          (expand-clause rest))))))

(define (true? exp)
    (not (eq? exp false)))

(define (false? exp)
    (eq? exp false))

(define (make-procedure parameters body env)
    (list 'procedure parameters body env))

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (procedure-parameter p)
    (cadr p))

(define (procedure-body p)
    (caddr p))

(define (procedure-environment p)
    (cadddr p))

(define (enclosing-environment env)
    (cdr env))

(define (first-frame env)
    (car env))

(define (the-empty-environment evn)
    '())

(define (make-frame variables values)
    (cons variables values))

(define (frame-variables frame)
    (car frame))

(define (frame-values frame)
    (cdr frame))

(define (add-binding-to-frame var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))


(define (extend-environment variables values base-env)
    (if (= (length variables) (length values))
        (cons (make-frame variables values) base-env)
        (if (< (length variables) (length values))
            (error "TOO FEW ARGUMENTS SUPPLIED!" variables values)
            (error "TOO MANY ARGUMENTS SUPPLIED!" variables values))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
                (cond ((null? vars)
                       (env-loop (enclosing-environment env)))
                      ((eq? var (car vars))
                       (car vals))
                      (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "UNBOUNDED VARIABLE" var)
            (let ((frame (first-frame env)))
                 (scan (frame-variables frame)
                       (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
                (cond ((null? vars)
                       (env-loop (enclosing-environment env)))
                      ((eq? var (car vars))
                       (set-car! vals val))
                      (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "UNBOUNDED VARIABLE" var)
            (let ((frame (first-frame env)))
                 (scan (frame-variables frame)
                       (frame-values frame)))))
    (env-loop env))

(define (define-variable-value! var val env)
    (let ((frame (first-frame)))
        (define (scan vars vals)
            (cond ((null? vars)
                   (add-binding-to-frame var val frame))
                  ((eq? var (car vars))
                   (set-car! vals val))
                  (else (scan (car vars) (car vals)))))
        (scan (frame-variables frame)
              (frame-values frame))))


(define (setup-environment)
    (let ((init-env (extend-environment (primitive-procedure-names)
                                         primitive-procedure-objects)
                                         the-en)
        
                     








