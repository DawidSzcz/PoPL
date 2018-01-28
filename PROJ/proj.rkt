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
                   ;(print-predicates)
                   (cases term term-query
                     (a-term (id args) 
                             (letrec ((env (make-init-env args))
                                      (backtrack (lambda ()
                                                   (if (not (has-clauses?))
                                                       (let () (display "no more solutions") (newline))
                                                       (let ((clause  (call-next)))
                                                         ;(display clause) (newline)
                                                         (cases retype (call clause)
                                                           (fail (f) (backtrack))
                                                           (environment (e) 
                                                                        ;(print-unif)
                                                                        (display (resolve-query env)) 
                                                                        (newline)
                                                                        (backtrack))))))))
                               (call-predicate id args env)
                               (backtrack)))
                     (else (eopl:error 'invalid-query term-query)))
                   (display "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&") (newline)))))
  
  (define call
    (lambda (clause)
      ;(print-unif)
      ;(print-counter)
      (let* ((args (caar clause))
             (clauses (cadar clause))
             (new-env (unify-call (cadr clause) args (caddr clause)))
             (env (process-clauses clauses new-env (cadr clause))))
        (cases retype env
          (fail (string)
                (if (has-clauses?) 
                    (call (call-next))
                    (fail "no more solutions")))
          (else env))
        )))
  
  (define process-clauses
    (lambda (clauses env old-env)
      (cases retype env
        (fail (f) env)
        (environment (e)       
                     (if (null? clauses)
                         old-env
                         (process-clauses (cdr clauses) (process (car clauses) env) old-env))))))
  
  
  (define process
    (lambda (t env)
      (let ((r (cases term t(a-term (id args)
                                    (call-predicate id args env)
                                    (call (call-next)))
                 (a-variable (var) env)
                 (a-unification (t1 t2) (unify (get-var t1) t2))
                 (a-assignment (t1 exp) (let* ((e (value-of exp env))
                                               (h (unify env t1 (value e)))) h))
                 (a-cut () (cut-stack) env)
                 (a-literal (num) env))))
        ;(print-unif)
        ;(display r) (newline)
        r)))


  (process-program(scan&parse "
pow(50, $z) ? 
pow($X, $Z):-
    aux($X, 1, 1, $Z).
aux($X, $X, $Z, $Z) :- !.
aux($A, $B, $P, $P) :- . 
aux($X, $Y, $P, $R) :-
    is($Y1, +($Y,1)) ;
    is($P1, *($P, $Y1)) ;
    aux($X, $Y1, $P1, $R)."))
  (process-program(scan&parse "pow(10, $z) ? pow(1, 1) :- !. pow($y, $z) :- is($a, -($y,1)); pow($a, $r); is($z, *($y, $r))."  ))
  (process-program(scan&parse "test($z) ? test($z) :- is($z, 4); is($z, 3). test(4) :-."  ))
  (process-program(scan&parse "pow(1, $z) ? pow(1, 1) :- !. pow($y, $z) :- is($a, -($y,1)); pow($a, $r); is($z, *($y, $r))."  ))

  (process-program(scan&parse "test(3, $z, $y) ? test($z, $y, 1) :- is($y, $z)."  ))
  (process-program(scan&parse "test(3, $z, $y, $k) ? test($z, $y, 1, $r) :- is($y, $z); t($r). t(5) :-."  ))
  (process-program(scan&parse "test(3, $z, $y, $k) ? test($z, $y, 1, $r) :- is($y, $z); t($a); is($r, $a). t(5) :-."  ))

  )

