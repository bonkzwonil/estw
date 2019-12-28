(use-modules (srfi srfi-9))


(define (make-node id slots)
  (list id (make-hash-table) slots))
(define (node-id node)
  (car node))
(define (node-slots node)
  (caddr node))
(define (node-properties node)
  (cadr node))


(define (node-prop-set node key value)
  (hashq-set! (node-properties node)
			  key value))

(define (node-prop-get node key)
  (hashq-ref (node-properties node) key))

