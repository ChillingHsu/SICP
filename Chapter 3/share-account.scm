;ex3.38
;TODO
(define balance 100)

(set! balance (+ balance 10))
(set! balance (- balance 20))
(set! balance (- balance (/ balance 2)))
balance

(define balance 100)

(set! balance (+ balance 10))
(set! balance (- balance (/ balance 2)))
(set! balance (- balance 20))
balance

(define balance 100)

(set! balance (- balance (/ balance 2)))
(set! balance (+ balance 10))
(set! balance (- balance 20))
balance

;ex3.39
; 过程P1(lambda() (set! x ((s (lambda () (* x x))))))
; 过程P2(s (lambda () (set! x (+ x 1))))
; 100: P1访问x，而后P2将x设置为11，而后P1将x设置为100
; 101: P1将x设置为100，而后P2将x设置为101
; 121: P2将x设置为11，而后P1将x设置为121
;ex3.40~ex3.45 TODO
