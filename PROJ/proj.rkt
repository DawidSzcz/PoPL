(module lang (lib "eopl.ss" "eopl") 
  
  (require "predicates.rkt")
  (require "lang.rkt")
  (require "unification.rkt")
  (require "call-stack.rkt")
  (require "exp.rkt")
  (provide process-program )
  
  (define group-clauses
    (lambda (cls)
      (for-each
       (lambda (cl)
         (cases clause cl 
           (a-clause (head terms) 
                     (cases term head
                       (a-term (id args) 
                               (register-clause id (list args terms)))
                       (else (eopl:error 'invalid-clause-head head))))))
       cls)))
  
  (define process-program
    (lambda (prog)
      (initialize-predicates)
      (initialize-unification)
      (cases program prog
        (a-program (term-query clauses)
                   (group-clauses clauses)
                   (cases term term-query
                     (a-term (id args) 
                             (let ((env (make-init-env args)))
                               (process term-query env)
                               (print-unif)
                               ;(display env)
                               (resolve-query env)))
                     (else (eopl:error 'invalid-query term-query))
                     )))))
  
  (define call
    (lambda (old-env values predicate)
      (let* ((args (car predicate))
             (clauses (cadr predicate))
             (new-env (unify-call old-env args values)))
        (process-clauses clauses new-env )
         old-env)))
  
  (define process-clauses
    (lambda (clauses env)
      (if (null? clauses)
          env
          (process-clauses (cdr clauses) (process (car clauses) env)))))
  
  (define process
    (lambda (t env)
      (let ((result (cases term t
        (a-term (id args)
                (call env args (call-predicate id args env)))
        (a-variable (var) env)
        (a-unification (t1 t2) (unify (get-var t1) t2))
        (a-assignment (t1 exp) (let* ((var (get-var t1))
                                      (e (value-of exp env))
                                      (h (unify env var (value e)))) h))
        (a-literal (num) env))))
        (cases retype result
          (fail (string) 
                (let ((new-clause (call-next)))
                  (display new-clause) (newline)
                  (call (cadr new-clause) (caddr new-clause) (car new-clause))))
          (else result)))))
  
  (process-program(scan&parse "test($z) ? test($z) :- is($z, 4); is($z, 3). test(4) :-."  ))
  
  (process-program(scan&parse "test(3, $z, $y) ? test($z, $y, 1) :- is($y, $z)."  ))
  (process-program(scan&parse "test(3, $z, $y, $k) ? test($z, $y, 1, $r) :- is($y, $z); t($r). t(5) :-."  ))
  (process-program(scan&parse "test(3, $z, $y, $k) ? test($z, $y, 1, $r) :- is($y, $z); t($a); is($r, $a). t(5) :-."  ))
  
  )

