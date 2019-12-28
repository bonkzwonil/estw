;;Netzwerk

(use-modules (srfi srfi-1))

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


(use-modules (ice-9 format))

(define (netz-dotty netz)
  (format #t "digraph { ~%")
  (for-each (lambda (node)
			  (format #t "\"~a\" [style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">~a</font></td></tr>" (node-id node) (node-id node))
			  (hash-for-each (lambda(k v)
							   (format #t "<tr><td align=\"left\" port=\"r5\">~a</td><td bgcolor=\"grey\" align=\"right\">~a</td></tr>" k v))
							 (node-properties node))
			  (format #t "</table>> ]~%")
			  (when node
				(for-each (lambda (slot)
							(map display `("\"" ,(node-id node) "\""
										   " -> "
										   "\"" ,slot "\""))
							(newline))
						  (node-slots node))))
			netz)
  (format #t "} ~%"))

(define (netz-get netz id)
  (assoc id netz))

(define (netz-referer netz id)
  (map node-id
	   (filter (lambda(node)
			(member id (node-slots node)))
		  netz)))

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
   

(define (netz-debug op c)
  "Fully recursively resolves op ... for debugging be careful ;). second value is used ticks"
  (if (op-done? op)
	  op
	  (begin
		(with-output-to-file (format #f "graph~a.dot" c)
		  (lambda()
			(netz-dotty (op-data op))))
		(netz-debug (op-cont op) (1+ c)))))
