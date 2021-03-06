(define (partial-sums stream)
  		(define (helper sums stream)
  				(if (null? stream)
  					nil
  					(cons-stream (+ (car stream) sums)
  								 (helper (+ (car stream) sums) (cdr-stream stream)))))
  		(helper 0 stream)
)
