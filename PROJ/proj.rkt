(module lang (lib "eopl.ss" "eopl") 
  
  (require "predicates.rkt")
  (require "lang.rkt")
  (require "unification.rkt")
  (require "call-stack.rkt")
  (require "exp.rkt")
  (provide process-program )
  
    (define process-clauses
    (lambda (cls)
      (for-each
       (lambda (cl)
         (cases clause cl 
           (a-clause (head terms) 
                     (cases term head
                       (a-term (id args) 
                               (register-clause id (list args terms)))
                       (else (eopl:error 'invalid-clause-head))))))
       cls)))
  
  (define process-program
    (lambda (prog)
      (initialize-predicates)
      (cases program prog
        (a-program (term-query clauses)
                   (process-clauses clauses)
                   (cases term term-query
                     (a-term (id args) 
                             (call args (call-predicate id)))
                     (else (eopl:error 'invalid-clause-head))
                     )))))
  
  (define call
    (lambda (values predicate)
      (let* ((args (car predicate))
            (clauses (cadr predicate))
            (return (unify-call args values)))
        (if (null? clauses)
            return
            (process (car clauses) return )))))
  
  (define process
    (lambda (t env)
      (cases term t
        (a-term (id args)
                (call args (call-predicate id)))
        (a-variable (var) var)
        (a-unification (t1 t2) (unify (get-var t1) t2))
        (a-assignment (t1 exp) (unify env (get-var t1) (value-of exp env)))
        (a-literal (num) num))))
  
  (process-program(scan&parse "test(3, $z) ? test($z, $y) :- is($y, $z)."  ))
                
  )

