(define (sum f l u)
  (if (= l u)
      (f l)
      (+ (f l) (sum f (+ 1 l) u))
      )
  )


(define (range x)
  (define (iter-range x m)
    (if (= x m)
	'()
	(cons x (iter-range (+ x 1) m))
	)
    )
  (iter-range 0 x)
  )

(define (isalltrue? l)
  (if (null? l)
      #t
      (if (car l)
	  (isalltrue? (cdr l))
	  #f)
      )
)


(define x (list #t #t #t #t))
(define y (list #t #f #t #t))
