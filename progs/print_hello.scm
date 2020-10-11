(define (f x)
(if (= x 0)
	0
	(begin
		(display "Heck\n")
		(f (- x 1))
	)
)
)

(f 400)
