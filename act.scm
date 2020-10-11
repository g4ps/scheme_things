(define (perform act)
  ((car act) (cdr act))
  )


(define sq
  (lambda(x)
    (* x x))
  )

(define x (cons sq 100))
(define y (cons sq 200))
(define k (cons sq 300))

(define suml
  (lambda (l)
    (if (null? l)
	0
	(+ (car l) (suml (cdr l)))
	)
    )
  )

