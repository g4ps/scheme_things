(define make-set  
  (let ((total 0)
	(count  0))
    (lambda (x)
      (set! total (+ total x))
      (set! count (+ count 1))
      (/ total count)
      )
    )
  )

   
