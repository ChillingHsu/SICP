(define (map proc item . items)
;     (cond ((null? items) (accumulate (lambda (x y) (cons (proc x) y)) nil item))
;           ((not (pair? items))) (accumulate (lambda (x y) (cons (proc x) y)) nil items)
;           (else (accumulate-n (lambda (x y) (cons (proc x) y)) nil items))))