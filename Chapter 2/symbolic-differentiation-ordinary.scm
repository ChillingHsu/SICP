;ex2.58
;中缀表达式求导
(define (deriv exp var)
  ; (newline)
  ; (display exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ; ((exponentiation? exp)
        ;  (make-product (make-product (exponent exp)
        ;                              (make-exponentiation (base exp)
        ;                                                   (make-sum (exponent exp) (- 1))))
        ;                (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))
(define (variable? v) (symbol? v))
(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
(define (=number? e n)
  (and (number? e) (= e n)))


(define (sum? e)
  (and (pair? e) (or (eq? (cadr e) '+)
                     (and (<= 5 (length e))
                          (sum? (cddr e))))))
(define (product? e)
  (and (not (sum? e)) (eq? (cadr e) '*)))

(define (addend e)
  (if (eq? '+ (cadr e))
    (car e)
    (append (list (car e) (cadr e))
            (let ((suf (addend (cddr e))))
                 (if (pair? suf)
                     suf
                     (list suf))))))
(define (augend e)  (cadr (memq '+ e)))

(define (multiplier e) (car e))
(define (multiplicand e) (if (= 1 (length (cddr e))) (caddr e) (cddr e)))

(define (make-sum-list al)
    (cond ((= 1 (length al)) (list (car al)))
          ((= 2 (length al))
           (cond ((=number? (car al) 0) (cadr al))
           ((=number? (cadr al) 0) (car al))
           ((and (number? (car al)) (number? (cadr al))) (+ (car al) (cadr al)))
           (else (list (car al) '+ (cadr al)))))
          (else (append (list (car al) '+ (cadr al) '+ )
                        (make-sum-list (cddr al))))))
(define (make-sum a1 a2 . an)
  (make-sum-list (append (list a1 a2) an)))

(define (product? e)
  (and (pair? e) (eq? (cadr e) '*)))

(define (make-product-list ml)
  (cond ((= 1 (length ml)) (list (car ml)))
        ((= 2 (length ml))
         (cond ((or (=number? (car ml) 0) (=number? (cadr ml) 0)) 0)
         ((=number? (car ml) 1) (cadr ml))
         ((=number? (cadr ml) 1) (car ml))
         ((and (number? (car ml)) (number? (cadr ml))) (* (car ml) (cadr ml)))
         (else (list (car ml) '* (cadr ml)))))
        (else (append (list (car ml) '* (cadr ml) '* )
                      (make-product-list (cddr ml))))))
(define (make-product m1 m2 . mn)
  (make-product-list (append (list m1 m2) mn)))

(define e '(x * 3 + (x * x + y + 2)))
; (multiplicand (addend e))
(deriv e 'x)
