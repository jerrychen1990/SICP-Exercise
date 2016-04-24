;use let grammer to control calculate sequence
(define (list-of-values-left exps env)
    (if (no-operands? exps)
        '()
        (let ((first (eval (first-operand exps) env)))
             (cons first (list-of-values-left (rest-operand exps) env)))))

(define (list-of-values-right exps env)
    (if (no-oprands? exps)
        '()
        (let ((rest (list-of-values-right (rest-operand exps) env)))
             (cons (eval (first-operand exps) env)
                   rest))))

