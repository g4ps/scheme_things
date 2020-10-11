(define (loop1 x)
  (if (= x 1)
      1
      (+ x (loop1 (- x 1)))
      )
  )

(define (lg x)
  (if (< x 2)
      1
      (+ 1 (lg (/ x 2)))
      )
  )

(define (abs x)
  (if (< x 0)
      (- 0 x)
      x)
  )

(define (range x)
  (define (rrange x)
    (if (= x 0)
	'()
	(cons x (rrange (- x 1)))
	)
    )
  (reverse (rrange x))
  )

(define (nlg x)
  (* x (lg x))
  )


(define (delta l1 l2)
  (lambda (x) (abs (- (l1 x) (l2 x))))
  )

(define (for_list s e d)
  (if (> s e)
      '()
      (cons s (for_list (+ s d) e d))
      )
  )

(define (max l)
  (define (imax isf l m)
    (if (null? l)
	m
	(if (= isf 1)
	    (imax 0 l m)
	    (if (> (car l) m)
		(imax 0 (cdr l) (car l))
		(imax 0 (cdr l) m)
		)
	    )
	)
    )
  (imax 1 l 0)
  )

(define (lsum l)
  (if (null? l)
      0
      (+ (car l) (lsum (cdr l)))
      )
  )

(define (average l)
  (/ (lsum l) (length l))
  )

(define (is_close f1 f2 s e d ch)
  (< (max (map (delta f1 f2) (for_list s e d))) ch)
  )


(define (lin x)
  (x)
  )


(define (f1 x)
  (* x 2)
  )

(define (f2 x)
  (+ (* x 2) 0.2)
  )

(define (lin x)
  (lambda (y)
    (* x y)
    )
  )

(define (der_list f1 s e d)
  (map (lambda (x) (/ (- (f1 x) (f1 (+ x d))) d))
       (for_list s e d)
       )
  )

(define (is_list_ap_close l ch)
  (let ((avr (average l)))
    (> ch (max (map (lambda (x) (abs (- x avr))) l)))
    )
  )

(define (is_linear f1 s e d ch)
  (is_list_ap_close (der_list f1 s e d) ch)
  )

(define (del_list p l isf)
  (if (null? l)
      '()
      (if (= isf 1)
	  (del_list (car l) (cdr l) 0)
	    (cons (- p (car l)) (del_list (car l) (cdr l) 0))
	    )
      )
  )
  

(define x (del_list 0 (map f1 (range 1000)) 0))

(define (il f1) (is_linear f1 -600 600 0.1 0.4))

(define (is_asp_close f1 f2 s e d ch)
  (and (is_list_ap_close (map (lambda (x) (/ (f1 x) (f2 x))) (for_list s e d)) ch)
       (is_list_ap_close (map (lambda (x) (/ (f2 x) (f1 x))) (for_list s e d)) ch)
       )
  )

(define (iac f1 f2)
  (is_asp_close f1 f2 10 1000000000000000 1000000000000 5)
  )

(define (sq x)
  (* x x))

(define (f3 x) (if (< x 1) 1 (+ x (f3 (- x 1)))))

(define (f4 x) (/ (* x (+ x 1)) 2))

(define (rf x)
  (if (< x 1)
      1
      (+ (* 3 (rf (/ x 2.0))) (* x (lg x))))
  )
