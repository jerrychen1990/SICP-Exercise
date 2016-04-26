;设置好类型与执行过程的映射关系
(load "table.scm")
(load "core.scm")

(define (install-exp-type exp-type procedure)
    (put 'op exp-type procedure))

(install-exp-type 'assignment eval-assignment)
;其他安装相同

(define (exp-type exp)
    (car exp))

(define (exp-body exp)
    (cdr exp))


(define (eval exp env)
    (let ((exp-type (exp-type exp))
          (exp-body (exp-body exp)))
         ((get 'op exp-type) exp-body env)))


;测试用例
(eval (list 'assignment x 3) env)
        









