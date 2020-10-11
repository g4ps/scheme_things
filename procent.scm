(define (acc proc sum year)
  (if (> year 0)
      (acc proc (* sum (+ 1 proc)) (- year 1))
      sum
      )
  )
