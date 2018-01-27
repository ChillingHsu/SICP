;ex2.53
(list 'a 'b 'c)
;Value 2: (a b c)

(list (list 'george))
;Value 3: ((george))

(cdr '((x1 x2) (y1 y2)))
;Value 4: ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;Value 5: (y1 y2)

(pair? (car '(a short list)))
;Value: #f

(memq 'red '((red shoes) (blue socks)))
;Value: #f

(memq 'red '(red shoes blue socks))
;Value 6: (red shoes blue socks)

;ex2.54
(define (equal? a b)
  (cond ((null? a) (null? b))
        ((null? b) (null? a))
        (else (and (eq? (car a) (car b))
                   (equal? (cdr a) (cdr b))))))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
;ex2.55
;实际上执行的是
;(car (quote (quote abracadabra)))
;是一个符号表，该符号表的第一个元素为quote
(car ''abracadabra)
