(define (list-of-values-left exps env)
    (if (no-operands? exps)
        '()
        (let (
