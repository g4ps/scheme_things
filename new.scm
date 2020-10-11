(define (last-pair l)
  (if (eq? (cdr l) '())
      l
      (last-pair (cdr l))
      )
  )

(define (ai i l)
  (if (= i 0)
      l
      (ai (- i 1) (cdr l))
      )
  )

(define (append! l a)
  (set-cdr! (last-pair l) a)
  )


(define x (list 1 2 3 4 5))
(set-cdr! (last-pair x) x)



(define (make-q)
  (cons '() '())
  )

(define (f x . l)
  l)

(define (front-ptr q)
  (car q)
  )
(define (rear-prt q)
  (cdr q)
  )

(define empty?
  (lambda (x)
    (and (eq? (front-ptr x) '()))
    )
  )

(define (insert q arg)
  (let ((new (cons arg '())))
    (if (empty? q)
	(begin (set-car! q new)
	       (set-cdr! q new)
	       q)      
	(begin 
	  (set-cdr! (cdr q) new)
	  (set-cdr! q new)
	  q)
	)
    )
  )

(define (ret-list q)
  (car q)
  )

(define (delete q)
  (if (empty? q)
      "q is empty"
      (let ((ret (caar q)))
	(begin (set-car! q (cdr (car q)))
	       ret)
	)
      )
  )

(define q (make-q))
(insert q 10)
(insert q 20)
(insert q 30)
