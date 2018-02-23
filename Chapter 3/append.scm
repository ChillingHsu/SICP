;ex3.12
(define x (list 'a 'b))
(define y (list 'c 'd))
(append x y)
x
;Value : (a b)
(append! x y)
x
;Value : (a b c d)
