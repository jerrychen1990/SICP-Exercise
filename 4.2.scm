;  a)
;  eval 会把绝大部分表达式（满足 pair?条件）都判定为application。
;  (define x 3) 被判定为 名称为define的过程应用，参数是x 和 3

(load "core.scm")
(define (application? exp)
    (tagged-list exp 'call))

(define (operator exp)
    (cadr exp))

(define (operands exp)
    (cddr exp))





