;; bnk's promises

(define (promise cb)
  (let ((value #f)
		(error #f) 
		(resolved #f)
		(value-listener #f)
		(error-listener #f))
	(let ((resolve (λ (x)
					 (set! resolved #t)
					 (set! value x)
					 (when value-listener (value-listener x))))
		  (reject (λ (x)
					(set! resolved #t)
					(set! error x)
					(when error-listener (error-listener x)))))
	  
	  (cb resolve reject)
	  (let ((then-fun (λ (f)  (if resolved
									   (f value)
									   (set! value-listener f))))
			(catch-fun (λ (f) (if resolved
									   (f error)
									   (set! error-listener f)))))
	  `(promise ,then-fun ,catch-fun)))))


(define (then promise fun)
  ((cadr promise) fun))

(define (then-catch promise fun)
  (caddr promise) fun)

;;timeout dispatching systeme
(define timeout #f)
(define dispatch #f)
(let ((dispatchers '()))
  (set! timeout (λ (fun time)
				  (set! dispatchers (append! dispatchers (list (cons time fun))))))
  (set! dispatch (λ (time)
				   (set! dispatchers (map (λ (x) (cons (- (car x) time) (cdr x))) dispatchers))
				   (map (λ (x) ((cdr x)))
						(filter (λ (x) (negative? (car x))) dispatchers))
				   (set! dispatchers (filter (λ (x) (not (negative? (car x)))) dispatchers)))))
				   
  
  

;;test
(let ((p1 (promise (λ (resolve reject) (resolve 1)))))
  (then p1 display)
  (then-catch p1 throw))

(let ((p1 (promise (λ (resolve reject) (reject 1)))))
  (then p1 display)
  (then-catch p1 throw))

(let ((p2 (promise (λ (resolve reject) (timeout (λ () (resolve 'Yo)) 2)))))
  (then p2 display)
  (then-catch p2 throw))

(dispatch 1)
(dispatch 1.7)


