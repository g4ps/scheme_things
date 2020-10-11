(define (newton func low ac)
  (define (abs x)
    (if (> x 0)
	x
	(- 0 x)
	)
    )
  
  (define (good-enough? x y acc f)
    (if (< (abs (- y (f x))) acc)
	#t
	#f
	)
    )
  
  (define (iter-newt l u all acc f)
    (let ((q (/ (+ l u) 2)))
      (begin
	(if (good-enough? q all acc f)
	    q
	    (if (< (- (f q) all) 0)
		(iter-newt (/ (+ l u) 2) u all acc f)
		(iter-newt l (/ (+ l u) 2) all acc f)	      
		)
	    )
	)
      )
    )
  (lambda (x) (iter-newt low x x ac func))
  )




(define (sq x) (* x x))

(define sqrt (newton sq 0.0 0.000001))
