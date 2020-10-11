(define (fp n m)
  (if (= m 0)
      1
      (* (+ (- n m) 1) (fp n (- m 1)))
      )
  )

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))
      )
  )

(define (rp n m)
  (if (= m 1)
      n
      (* (+ n m -1) (rp n (- m 1)))
      )
  
  )


(define (rpx x)
  (lambda (y)
    (rp x y)
    )
  )


(define (fpx x)
  (lambda (y)
    (fp x y)
    )
  )
