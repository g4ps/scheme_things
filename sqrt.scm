(define abs
  ( lambda (x) (if (> x 0) x (- 0 x)))
  )

(define sq
  (lambda (x)
    (* x x))
  )


  
(define (sqrt x)
  (define (ge? guess)
    (< (abs ( - (sq guess) x)) 0.001))
  (define (iq guess)
    (/ (+ guess (/ x guess)) 2))
  (define (sqrt-iter guess)
    (if (ge? guess)
	guess
	(sqrt-iter (iq guess))))
  (sqrt-iter 1.0)
  )
