;; Testo

(define (signal-before netz id)
  (let ((before-id (car (netz-referer netz id))))
	(if (eqv? 'signal (car before-id))
		before-id
		(signal-before netz before-id))))
  
(define (signal-cmd-HAGT netz id) ; HaGT = Signal auf Hp0 / Halt
	;;; Signal rot stellen = vorsignal gelb (falls grün) -> Signal rot
  (let 	((vorsignal-id (signal-before netz id)))
	(let ((signal (netz-get netz id)))
	  (if (eqv? (node-prop-get signal 'HS) 'Hp0); HS = Hp0? Done :)
		  ((op-nop) netz)
		  (op (op-chain (list
						 (signal-cmd-KS2 vorsignal-id)
						 (op-delayed (lambda(netz)
									   (node-prop-set signal 'HS 'Hp0)
									   netz)
									 1)))
			  netz)))))
					
(define (signal-cmd-KS2 id) ; KS2 = Signal auf KS2 / Halt erwarten (intern)
  (op-delayed (lambda(netz)
				(let ((signal (netz-get netz id)))
				  (if (eqv? (node-prop-get signal 'HS) 'Hp0)
					  netz
					  (begin (node-prop-set signal 'HS 'KS2)
							 netz))))
			  1))
							  
			   
