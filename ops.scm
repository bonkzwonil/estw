;;Operations

;;An op can be called by his next method and produces a new op and a intermediate result
;;

(use-modules (srfi srfi-1) (srfi srfi-8)) ;(srfi srfi-89))

(define (op next data)
  "Results of ops are a cons with the next op or #f and the result so far"
  (cons next data))

(define (op-next op)
  (car op))

(define (op-data op)
  (cdr op))

(define (op-cont op)
  (if (op-next op)
	  ((op-next op) (op-data op))
	  (op-data op)))

(define (op-done? op)
  (not (op-next op)))

(define op? pair?)

(define (op-delayed work ticks)
  "An delayed Op simulates the duration of work of atomic tasks"
  (lambda(data)
	(if (positive? ticks)
		(op (op-delayed work (1- ticks)) data)
		(op (op-atomic work) data))))

(define (op-atomic work)
  "An atomic op"
  (lambda(data)
	(op #f (work data))))

(define (op-nop)
  (op-atomic (lambda(data)data)))

  
(define* (op-resolve op #:optional c)
  "Fully recursively resolves op ... for debugging be careful ;). second value is used ticks"
  (if (op-done? op)
	  (values op c)
	  (op-resolve (op-cont op) (1+ (or c 0)))))

(define (op-chain oplist)
  (if (null? oplist)
	  (op-nop)
	  (lambda(data)
		(let ((v ((car oplist) data)))
		  (if (op-done? v) ;That op is done
			  (op (op-chain (cdr oplist)) (op-data v)) ;Zwischenergebnis!
			  (op (op-chain (cons (op-next v) (cdr oplist))) (op-data v)))))))

(define (op-parallel oplist)
  "parallel means actually in waterfall sequence in one tick :)"
  (if (null? oplist)
	  (op-nop)
	  (lambda(data)
		(let* ((folded (fold
						(lambda(op last)
						  (cons (op (op-data (car last))) last))
						(list ((op-nop) data))
						oplist))
			   (data (op-data (car folded)))
			   (oplist (map op-next (reverse (filter (negate op-done?) folded)))))
		  (op (op-parallel oplist) data)))))



;;Macros :)

(define-macro (ops data-varname . rest)
  (let ((oplist (map (lambda(form)
					   ;;each form has the syntax: (body) or ((body)(body...)) for parallel ops
					   ;;Special form for delay is a number
					   (if (number? form)
						   `(op-delayed (lambda(x)x) ,form)
						   `(op-atomic (lambda(,data-varname)
										 ,form))))
					 rest)))

	`(op-chain ,(cons 'list oplist))))


;;TEsts


	
	
  
(let ((op (op-parallel (list (op-atomic 1+) (op-delayed 1+ 2) (op-chain (list (op-delayed 1+ 3) (op-atomic 1+))) (op-atomic 1-)))))
  (op-resolve (op 5)))


(ops var
	 (1+ var)
	 'bar
	 (display 'foo)
	 8
	 (display var))

(ops var
	 (1+ var)
	 (ops foo
		  (+ foo 3)
		  (- foo 9))
	 (display 'foo)
	 (display var))
