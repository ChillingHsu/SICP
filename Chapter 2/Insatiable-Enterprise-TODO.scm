(define (make-headquarters-file division origin-file)
  (cons division origin-file))
(define (division headquarters-file)
  (car headquarters-file))
(define (origin-file headquarters-file)
  (cdr headquarters-file))

; a
(define (get-record employee file)
  ((get 'get-record (division file))
    employee
    (origin-file file)))
; b
