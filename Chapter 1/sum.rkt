#lang sicp
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
(define (cube a)
  (* a a a))
;(define (integral f a b dx)
;    (define (add-dx x) (+ x dx))
;    (* (sum f (+ a (/ dx 2.0)) add-dx b)
;       dx))
(define (integral f a b n)
  (define (simpson h)
    (define (add-two-h a)
            (+ a h h))
    (define (add-h a)
      (+ a h))
    (display h)
    (* (/ h 3)
       (+ (f a)
          (f b)
          (* 4
             (sum f (add-h a) add-two-h b))
          (* 2
             (sum f (add-two-h a) add-two-h b)))))
     
  (simpson (* 1.0 (/ (- b a) n))))
