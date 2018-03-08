(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (without-interrupts
    (lambda ()
      (if (car cell)
          true
          (begin (set-car! cell true)
                false)))))


;ex3.46
; Time  process#1  Mutex   process#2
;   |       |      false
;   |   'acquire---->|
;   |       |        |<----'acquire
;   |       |        v         |
;   |       |<-----true------->|
;   |       |                  |
;   |       |                  |
;   |       |  both-in-process |
;   v
; Time

;ex3.47
; a
(define (make-semaphore n)
  (let ((resources n)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (lambda (x)
               (mutex 'acquire)
               (if (> x resources)
                   (begin (mutex 'release)
                          ((the-semaphore 'acquire) x))
                   (begin (set! resources (- resources x))
                          (mutex 'release)))))
            ((eq? m 'release)
             (lambda (x)
               (mutex 'acquire)
               (if (not (negative? x))
                   (set! resources (+ resources x))
                   (error "negative delta of semaphore release -- SEMAPHORE" x))
               (mutex 'release)))
            ((eq? m 'resources) resources)))
    the-semaphore))
; b
(define (make-semaphore n)
  (let ((resources n)
        (cell (list false)))
    (define (the-semaphore m)
       (cond ((eq? m 'acquire)
              (lambda (x)
                (if (test-and-set! cell)
                    ((the-semaphore 'acquire) x)
                    (if (> x resources)
                        (begin (clear! cell)
                               ((the-semaphore 'acquire) x))
                        (begin (set! resources (- resources x))
                               (clear! cell))))))
             ((eq? m 'release)
              (lambda (x)
                (if (test-and-set! cell)
                    ((the-semaphore 'release) x)
                    (if (not (negative? x))
                        (set! resources (+ resources x))
                        (error "negative delta of semaphore release -- SEMAPHORE" x)))
                (clear! cell)))
             ((eq? m 'resources) resources)))
    the-semaphore))
(define s (make-semaphore 4))
((s 'acquire) 3)
(s 'resources)
((s 'release) 10)
((s 'acquire) 12)
