(define (peg x)
  (if (= x 0)
      0
      (+ 2 (* 3 (peg ( - x 1))))
      )
  )


