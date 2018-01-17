(module call-stack racket 
  (require "predicates.rkt")
  
  (provide call-predicate)
  
  (define cs 'uninitialized)
  
  (define init-cs
    (lambda ()
      (set! cs '())))
  
  (define call-predicate
    (lambda (id)
      (let ((clauses (get-predicate id)))
        (set! cs (list (cdr clauses) cs))
        (car clauses))))
  )