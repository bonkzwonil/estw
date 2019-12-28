(use-modules (srfi srfi-9))

(define-record-type <node>
  (make-node id slots)
  node?
  (id node-id)
  (properties node-properties)
  (slots node-slots))

(define (make-node id slots)
  (list id #f slots))
(define (node-id node)
  (car node))
(define (node-slots node)
  (caddr node))

