;ex2.38
(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(define nil '())
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
;如果要保证使用某个op时fold-right和fold-left都产生同样点结果，则op需要满足交换律。
;例如op为*
(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
;ex2.39
;加深对两种折叠方式执行顺序的理解
(define (reverse sequence)
    (fold-right (lambda (x y)
                    (if (null? y)
                        (list x)
                        (append y (list x))))
                nil
                sequence))
(reverse (list 1 2 3 4 5))
(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse (list 1 2 3 4 5))
