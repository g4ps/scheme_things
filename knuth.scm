(define (knuth x)
  (let ((c (- x 1)))
    (if (= x 0)
	1
	(+ 1 (min ( * 2 ( knuth (floor (/ c 2)))) (* 3 (knuth ( floor (/ c 3))))))
	)
    )
  )

(define (prop x)
  (>= (knuth x) x))
