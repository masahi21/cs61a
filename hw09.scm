
; Tail recursion

(define (replicate x n)
   (define (helper x n lst)
      (if (= n 0)
           lst
          (helper x (- n 1) (cons x lst))))
    (helper x n '())
)

(define (accumulate combiner start n term)
  (if (< n 1) start
      (combiner (term n) (accumulate combiner start (- n 1) term)))
)

(define (accumulate-tail combiner start n term)
  (if (< n 1) start
      (accumulate-tail combiner (combiner (term n) start) (- n 1) term))
  )

; Streams

(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define multiples-of-three
  (cons-stream 3 (map-stream (lambda (x) (+ 3 x)) multiples-of-three))
)


(define (nondecreastream s)
  (if (null? s) nil
      (begin (define (small x y)
                (if (null? x) nil
                    (if (< (car x) y) nil
                    (cons (car x) (small (cdr-stream x) (car x))))))
             (define (size x)
                (if (null? x) 0 (+ 1 (size (cdr x)))))
             (define (new s n)
                (if (= n 0)
                     s
                    (new (cdr-stream s) (- n 1))))
      (cons-stream (small s 0)
  (nondecreastream ( new s (size (small s 0)))))))
)



(define finite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 3
                (cons-stream 1
                    (cons-stream 2
                        (cons-stream 2
                            (cons-stream 1 nil))))))))

(define infinite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 2
                infinite-test-stream))))
