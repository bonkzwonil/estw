;;Netzwerk


(define (make-netz netzdef)
  (let ((makeid (lambda (id) (if (list? id)
							(cons (car id) (cadr id))
							id
							))))
	(map (lambda (node)
		   (let ((id (makeid (car node)))
				 (next (makeid (cadr node))))
			 (case (caar node)
			   ((signal) (make-node id (list next)))
			   ((gleis) (make-node id (list next)))
			   ((weiche) (make-node id (list next
											 (makeid (caddr node)))))
			   (else #f))))
		 netzdef)))
		   

(define (netz-dotty netz)
  (for-each (lambda (node)
			  (when node
				(for-each (lambda (slot)
							(map display `("\"" ,(node-id node) "\""
										   " -> "
										   "\"" ,slot "\""))
							(newline))
						  (node-slots node))))
			netz))

(define (netz-get netz id)
  (assoc id netz))

(define netz (make-netz
			  '(((signal 1) (gleis 1))
				((gleis 1) (signal 2))
				((signal 2) (gleis 2))
				((gleis 2) (weiche 1))
				((weiche 1) (gleis 3) (gleis 4))
				((gleis 3) (signal 3))
				((gleis 4) (signal 1)))))


 
   
 
;; (make-netz
;;  '(((signal 1) (gleis 1) (signal 2) (gleis 2) (weiche 1) (gleis 3) (signal 3))
;;    ((weiche 1) (gleis 4) (signal 4))))
   
