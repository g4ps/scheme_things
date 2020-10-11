

(define (make-acc proc)
  (let ((total 0))
    (lambda (sum)
      (begin (set! total (+ (* total (+ 1.0 (/ proc 100.0))) sum))
	     total)
      )
    )
  )
