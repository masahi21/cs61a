;;;;;;;;;;;;;;;
;; Questions ;;
;;;;;;;;;;;;;;;

; Scheme

(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
  
)

(define (caddr s)
  (car (cdr (cdr s)))
)

(define (sign x)
  (cond ((> x 0) 1) ((= x 0) 0) ((< x 0) -1)) 
)

(define (square x) (* x x))

(define (pow b n)
  (cond
    ((zero? n) 1)
    ((even? n) (square (pow b (/ n 2))))
    ((odd? n) (* b (pow b (- n 1)))))
)

(define (unique s)
  (if (null? s) nil
  (cons (car s) (unique (filter (lambda (x) (not (equal? (car s) x))) (cdr s)))))
)