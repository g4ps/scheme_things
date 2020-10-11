(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))     
      )
  )

(define (mod x y)
  (- x (* (floor (/ x y)) y))
  )


(define (bintodec x)
  (define (rev x)
    (if (null? x)
	0
	(+ (car x) (* 2 (rev (cdr x))))
	)
    )
  (rev (reverse x))
  )


(define (revd x)  
  (if (= x 0)
      '()
      (cons (mod x 2) (revd (floor (/ x 2))))
      )
  )



(define (dectobin x)
  (reverse (revd x))
  )

(define (dectohex x)
  (define (hnum y)
    (if (< y 10)
	y
	(cond ((= y 10) 'a)
	      ((= y 11) 'b)
	      ((= y 12) 'c)
	      ((= y 13) 'd)
	      ((= y 14) 'e)
	      ((= y 15) 'f)
	      )
	)
    )
  (define (revh x)
    (if (= x 0)
	'()
	(cons (hnum (mod x 16)) (revh (floor (/ x 16))))
	)
    )
  (reverse (revh x))
  )

(define (add_null l x)
  (define (ins l x)
    (if (= x 0)
	l
	(cons 0 (ins l (- x 1)))
	)
    )
  (ins l (- x (length l)))
  )


(define (chtolog l)
  (if (null? l)
      '()
      (if (= (car l) 0)
	  (cons #f (chtolog (cdr l)))
	  (cons #t (chtolog (cdr l)))
	  )
      )
  )

(define (logtobin l)
  (if (null? l)
      '()
      (if (car l)
	  (cons 1 (logtobin (cdr l)))
	  (cons 0 (logtobin (cdr l)))
	  )
      )
  )
      

(define (make-table ar)
  (define (tbiter x c)
    (if (= x c)
	'()
	(cons (chtolog (add_null (dectobin x) ar)) (tbiter (+ x 1) c))
	)
    )
  (tbiter 0 (pow 2 ar))
  )



(define (func x y z)
  (and y (or x y))
  )

(define (get_truth_table func ar)
  (define (iter func l)
    (if (null? l)
	'()
	(cons (apply func (car l)) (iter func (cdr l)))
	)
    )
  (iter func (make-table ar))
  )

