(module lang racket
  
  (provide (all-defined-out))
  
  (define h-ref!
    (lambda (h k v) (hash-ref! h k v)))
  
  (define make-h
    (lambda () (make-hash)))
  
  (define h-set!
    (lambda (h k v) (hash-set! h k v)))
  
  (define h-has-key?
    (lambda (h k) (hash-has-key? h k)))
  
  (define h-ref
    (lambda (h k) (hash-ref h k)))
  )
