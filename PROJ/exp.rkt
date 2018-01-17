(module lang (lib "eopl.ss" "eopl")
  
  (provide value-of)
  
  (require "lang.rkt")
  (require "unification.rkt")
  
  (define value-of
    (lambda (exp env)
      (cases expression exp
        
        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) num)
        
        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (resolve-var env (a-variable var)))
        
        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (- val1 val2)))
        
        (add-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 env))
                       (val2 (value-of exp2 env)))
                   (+ val1 val2)))
        (mult-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (* val1 val2)))
        
        )))
  
  )