(module call-stack racket 
  (require "predicates.rkt")
  
  (define cs 'uninitialized)
  
  (define init-cs
    (lambda ()
      (set! cs '())))
  
  (define call 
    (lambda (id)
      (set! cs (cons (call-predicate id)))))
  )