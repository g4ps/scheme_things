(define (make-acc bal)
  (lambda (x)
    (if (> x bal)
	"Not enough funds"
	(begin (set! bal (- bal x)) bal)
	)
    )
  )

(define (set-ind! arr x arg)
  (if (= x 0)
      (begin (set-car! arr arg))
      (set-ind! (cdr arr) (- x 1) arg)      
      )
  )


(define x (list 1 2 3 4 5 6))
