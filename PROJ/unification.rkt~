(module lang racket 
  
  (provide resolve-var unify unify-call)
  
  (define unify-call
    (lambda (args values)
      (let ((h (make-hash)))
        (map (lambda(k v) (hash-set! h k v)) args values)
        h)))
  
  (define unify
    (lambda (env arg val)
      (let ((h (hash-set! env arg val)))
        h)))
  
  (define resolve-var
    (lambda (hash key)
      (hash-ref hash key)))
  )