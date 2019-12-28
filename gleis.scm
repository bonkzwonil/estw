;;gleise

(define (make-gleis id next)
  (make-node id (list next)))

(define (gleis-nummer gleis)
  (cdr (node-id gleis)))
