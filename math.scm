(define (sum f l u) ;sum of f starting in l upto u
  (if (= l u)
      (f l)
      (+ (f l) (sum f (+ 1 l) u))
      )
  )


(define (range x)
  (define (iter-range x m)
    (if (= x m)
	'()
	(cons x (iter-range (+ x 1) m))
	)
    )
  (iter-range 0 x)
  )

(define (isalltrue? l) ;and gate for lists of booleans
  (if (null? l)
      #t
      (if (car l)
	  (isalltrue? (cdr l))
	  #f)
      )
)



(define (newton func low ac) ;half-assed newton
  (define (abs x)
    (if (> x 0)
	x
	(- 0 x)
	)
    )
  
  (define (good-enough? x y acc f)
    (if (< (abs (- y (f x))) acc)
	#t
	#f
	)
    )
  
  (define (iter-newt l u all acc f)
    (let ((q (/ (+ l u) 2)))
      (begin
	(if (good-enough? q all acc f)
	    q
	    (if (< (- (f q) all) 0)
		(iter-newt (/ (+ l u) 2) u all acc f)
		(iter-newt l (/ (+ l u) 2) all acc f)	      
		)
	    )
	)
      )
    )
  (lambda (x) (iter-newt low x x ac func))
  )



(define (cube x) (* x x x))
(define (sq x) (* x x)) ;square

(define sqrt (newton sq 0.0 0.000001))
(define cbrt (newton cube 0.0 0.0000001))

(define (root x p)  ;root of x power of p
  ((newton (lambda (y) (ipow y p)) 0.0 0.0001) x)
  )


(define (ipow n m)  ;power for integers
  (cond ((= m 0) 1)
	((< m 0) (/ 1 (ipow n (- m))))
	(else (* n (pow n (- m 1))))
      )
  )

(define (pow n m)  ;power of a number
  (cond ((integer? m) (ipow n m))
	((rational? m)
	 (ipow (root n (denominator m))
	       (* m (denominator m))))
	)
  )


      

(define (mod x y)  ;remainder
  (- x (* (floor ( / x y)) y))
  )

(define pi 3.1415926)


(define (gcd x y)
  (let ((q (mod x y)))
    (if (= q 0)
	y
	(gcd y q))
    )
  )

(define (lcm x y)
  (let ((q (gcd x y)))
    (* q (/ x q) ( / y q))
    )
  )

(define (fact x)  ;factorial 
  (if (= x 0)
      1
      (* x (fact (- x 1)))
      )
  )

(define (fp x y) ;falling power
  (if (= y 0)
      1
      (if (= y 1)
	  x
	  (* (- x y -1) (fp x (- y 1)))
	  )
      )
  )

(define (bc n k) ;binominal coefficient
  (/ (fp n k) (fact k))
  )


(define (pascal-triangle x) ;print pascal triangle
  (define (full-range x)
    (if (< x 0)
	'()
	(cons x (full-range (- x 1)))
	)
    )
  
  (define (pascal-iter x l)
    (if (> x l)
	'()
	(cons (map (lambda(y) (bc x y)) (full-range x))
	      (pascal-iter (+ x 1) l))
	)
    )
  (pascal-iter 0 x)
  )

  
(define (print-each l)
  (if (null? l)
      0
      (begin (display (car l)) (newline) (print-each (cdr l)))
      )
  )

(define lg  ;bynary log
  (newton (lambda (y) (pow 2.0 y)) 0.0 0.001)
  )

(define (clg x) ;ceiling of lg
  (if (> x 2)
      (+ 1 (clg (/ x 2)))
      1)
  )

(define (ispow? x n) ;is x a power of n
  (if (= x n)
      #t
      (if (integer? (/ x n))
	  (ispow? (/ x n) n)
	  #f)
      )
  )

(define (iver x) ;iversonian notation
  ;if x is true return 1 otherwise 0
  (if x
      1
      0
      )
  )

      

(define (bytes k)  ;how many bytes does is take to
  ;store k
  (+ (clg k) (iver (ispow? k 2)))
  )

(define (t x)
  (if (= x 0)
      5
      (/ (+ (* 3 (fact x)) (* x (t (- x 1)))) 2.0)
      )
  )
