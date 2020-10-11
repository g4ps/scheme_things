(define (fact x)
  (if (= x 1)
      1
      (* x (fact (- x 1)))
      )
  )

(define (ntimes i f x)
  (if (= i 0)
      0
      (begin (f x) (ntimes (- i 1) f x))
      )
  )

(ntimes 10000000 fact 10)
