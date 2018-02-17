;ex2.73
; These code is NOT complete.(needs some additional procedures)
; a
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;每个类型都分别有一个定义好的过程，而不是检查表达式类型然后再deriv的过程里执行求导。这些过程都通过对应的操作符装载到分派表里。如果这个表达式不是一个数或者变量，那么就会从分派表中获得一个对应的过程，这个过程基于操作符并且已经求出传入参数值。操作符和操作数都是从给定的表达式中得到。
;数字和变量表达式不需要使用操作符和操作数过程，所以不能使用通用的分派机制。

; b
(define (install-deriv)
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  ; part of c
  (put 'deriv '** deriv-exponent))
(define (derive-sum operands var)
  (make-sum (deriv (addend operands) var)
            (deriv (augend operands) var)))
(define (deriv-product operands var)
  (make-sum (make-product (multiplier operands)
                          (deriv (multiplicand operands) var))
            (make-product (deriv (multiplier operands) var)
                          (multiplicand operands))))
; c
(define (deriv-exponent operands var)
  (make-product
    (make-product (exponent exp)
                  (make-exponentiation
                    (base exp)
                    (make-sum (exponent exp) (- 1))))
    (deriv (base exp) var)))

; d
; 只需要在求导装载(install-deriv)的put过程中作一点小的修改。
; (put '** 'deriv deriv-exponentiation)
; (put '+ 'deriv deriv-sum )
; (put '* 'deriv deriv-product)
