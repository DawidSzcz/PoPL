(module lang racket
  
  (provide (all-defined-out))
  
  (define h-ref!
    (lambda (h k v) (hash-ref! h k v)))
  
  (define make-h
    (lambda () (make-hash)))
  
  (define h-set!
    (lambda (h k v) (hash-set! h k v)))
  
  
  (define h-keys
    (lambda (h) (hash-keys h)))
  
  (define h-has-key?
    (lambda (h k) (hash-has-key? h k)))
  
  (define h-ref
    (lambda (h k [f (lambda () (error 'not-in-hash (list hash k)))]) (hash-ref h k)))
  
  (define h-copy
    (lambda (h) (hash-copy h)))
  
  (define h 
    (lambda (array)
      (let ((hash (make-hash)))
        (for-each 
         (lambda (assoc) 
           (hash-set! hash (car assoc) (cadr assoc)))
        array)
        hash)))
  
  (define h?
    (lambda (hash)
      (hash? hash)))
  )
