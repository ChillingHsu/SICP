;ex3.15
(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
(set-to-wow! z1)
(set-to-wow! z2)
