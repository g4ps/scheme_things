(define (cube x)
  (* x x x)
  )

(define (abs x)
  (if (< x 0)
      (- 0 x)
      x)
  )


(define (cbrt x)
  (define (good-enough? x y acc)
    (if (< (abs (- y (cube x))) acc)
	#t
	#f
	)
    )
  (define (iter-cbrt l u a acc)
    (let ((q (/ (+ l u) 2)))
      (begin
	;(display l)
	;(display " ")
	;(display u)
	;(newline)
	(if (good-enough? q a acc)
	    q
	    (if (< (- (cube q) a) 0)
		(iter-cbrt (/ (+ l u ) 2) u a acc)
		(iter-cbrt l (/ (+ l u) 2) a acc)	      
		)
	    )
	)
      )
    )
  (iter-cbrt 0.0 x x 0.0000001)
  )


(define (func x)
  (if (= x 0)
      0
      (if (integer? (/ x (floor (cbrt x))))
	  (+ 1 (func ( - x 1)))
	  (func (- x 1))
	  )
      )
  )

		    
