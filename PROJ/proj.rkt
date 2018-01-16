(module lang (lib "eopl.ss" "eopl") 
  
  (require "predicates.rkt")
  (require "lang.rkt")
  (provide process-program scan&parse)

  
  (define process-program
    (lambda (prog)
      (initialize-predicates)
      (cases program prog
        (a-program (term-query clauses)
                   (process-clauses clauses)
                   (cases term term-query
                     (a-term (id args) 
                             (call-predicate (id))
                             (deeper args))
                     (else (eopl:error 'invalid-clause-head))
                   )))))
  
  (define process-clauses
    (lambda (cls)
      (for-each
      (lambda (cl)
            (cases clause cl 
              (a-clause (head terms) 
                        (cases term head
                          (a-term (id args) 
                                  (add-predicate id (list args terms)))
                          (else (eopl:error 'invalid-clause-head)))))))))
  
  (define deeper
    (lambda (args)
      (ca
  )

