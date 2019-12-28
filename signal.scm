;; Testo


(define netz

  '( ((gleis . 9872) ((az 0)) (signal . 2))
	 ((signal . 1) ((m 0)) (gleis . 9872))
	 ((signal . 2) ((m 0)) (gleis . 2))
	 ((gleis . 2) () (weiche . 1))
	 ((weiche . 1) (()) ((gleis . 3) (gleis . 4)))
	 ((gleis . 4) () ())
	 ((gleis . 3) () ())))

(define (make-signal nr next) `((signal . ,nr) (()) next))

(define (make-gleis nr next) `((gleis . ,nr) () next))

(define (make-weiche nr a b) `((weiche . ,nr) () (a b)))

(define netz-in '(
				  ((gleis 9872) (signal  2) (gleis 3) (weiche 1) (gleis 4))
				  ((weiche 1) (gleis 5) (signal 3) (gleis 9))))
  

(define (node-valid-type? n)
  (memq n '(signal gleis weiche)))
	 
(define (node-id? n)
  (and (pair? n)
	   (node-valid-type? (car n))))

(define (with-next-nodes node fun)
  (let ((nodes (caddr node)))
	(map (lambda (x)
		   (fun (assoc x netz)))
		 (if (list? nodes)
			 nodes
			 (list nodes)))))
		
(define (plan-draw node)
  (when node
	(display (car node))
	(when (caddr node) (display " --> "))
	(with-next-nodes node plan-draw)
	(display " < ")
	node))


  

(define (signal nr) `((type signal) (nr ,nr) (m 0) (zs 0)))

(define (weiche nr) `((type weiche) (nr ,nr) (m 0) (in nil) (out (nil nil))))

(define strecke '(0 0 nil nil))

(define (connect a b) ; connect a mit b => strecke
  `(0 0 ,a ,b))

  (define plan 
