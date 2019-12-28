;; bnk's promises

(define (promise cb)
  (let ((value #f)
		(error #f) 
		(resolved #f)
		(value-listener #f)
		(error-listener #f))
	(let ((resolve (lambda (x)
					 (set! resolved #t)
					 (set! value x)
					 (when value-listener (value-listener x))))
		  (reject (lambda (x)
					(set! resolved #t)
					(set! error x)
					(when error-listener (error-listener x)))))
	  
	  (cb resolve reject)
	  (let ((then-fun (lambda (f)  (if resolved
									   (f value)
									   (set! value-listener f))))
			(catch-fun (lambda (f) (if resolved
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
  (set! timeout (lambda (fun timeout)
				  (set! dispatchers (append! dispatchers (list (cons timeout fun))))))
  (set! dispatch (lambda (timeout)
				   (set! dispatchers (map (lambda (x) (cons (- (car x) timeout) (cdr x))) dispatchers))
				   (map (lambda (x) ((cdr x)))
						(filter (lambda (x) (negative? (car x))) dispatchers))
				   (set! dispatchers (filter (lambda (x) (not (negative? (car x)))) dispatchers)))))
				   
  



;;test
(let ((p1 (promise (lambda (resolve reject) (resolve 1)))))
  (then p1 display)
  (then-catch p1 'throw))

(let ((p1 (promise (lambda (resolve reject) (reject 1)))))
  (then p1 display)
  (then-catch p1 'throw))

(let ((p2 (promise (lambda (resolve reject) (timeout (lambda () (resolve 'Yo)) 2)))))
  (then p2 display)
  (then-catch p2 throw))

(dispatch 1)
(dispatch 1.7)


