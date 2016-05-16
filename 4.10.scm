 ;修改let语法，调换参数定义与函数实体的未知

 (load "4.6.scm")

 (define (let-cond exp)
     (caddr exp))

 (define (let-body exp)
     (cadr exp))

 (define (make-let let-cond let-body)
     (display "make: ")
     (display-ln let-cond)
     (display-ln let-body)
     (append (list 'let let-body) let-cond))

;测试数据

(define input 
'(let (+ a b) ((a 3) (b (* 4 5)))))

(eval input the-global-environment)




