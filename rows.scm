(define (hms n m)
  (lambda ()
    (let ((ret (ceiling ( / n m))))
      (begin (set! n (- n ret)) (set! m (- m 1)) ret)
      )
    )
  )
