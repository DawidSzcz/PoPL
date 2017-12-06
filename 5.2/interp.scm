(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language
  
  (require "drscheme-init.scm")
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)
  
  ;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;
  
  (define instrument-let (make-parameter #f))
  
  (define (foldl f init seq) 
    (if (null? seq) 
        init 
        (foldl f (f init (car seq)) (cdr seq)))) 
  
  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.
  
  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
  
  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
                   (value-of-statement exp1 (init-env))))))
  
  (define value-of-statement
    (lambda (stat env)
      (cases statement stat
        (print-state (exp)
                     (begin
                       (write (expval->num (value-of exp env)))
                       (newline)))
        (assign-state (var exp1)
                      (setref!
                       (apply-env env var)
                       (value-of exp1 env)))
        (block-state (state states)
                     (begin 
                       (value-of-statement state env)
                       (map (lambda (stt) (value-of-statement stt env)) states)))
        (if-state (exp state1 state2) 
                  (let ((val (value-of exp)))
                    (if (expval->bool val)
                        (value-of-statement state1 env)
                        (value-of-statement state2 env))))
        (while-state (exp state) 
                     (if (expval->bool (value-of exp env))
                         (begin 
                           (value-of-statement state env)
                           (value-of-statement (while-state exp state) env))
                         (num-val 27)))
        (var-state (var exp vars exps state) 
                   (letrec ((proc-vars 
                             (lambda (vars exps nenv)
                               (if (null? vars)
                                   nenv
                                   (proc-vars (cdr vars) (cdr exps) (extend-env (car vars) (newref (value-of (car exps) nenv)) nenv)))))) 
                     (value-of-statement state (proc-vars vars exps (extend-env var (newref (value-of exp env)) env))))))))
  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp
        
        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))
        
        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))
        
        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))
        
        (sum-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 env))
                       (val2 (value-of exp2 env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (num-val
                      (+ num1 num2)))))
        
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
        (not-exp (exp1)
                 (let ((val1 (expval->bool (value-of exp1 env))))
                   (if val1 (bool-val #f) (bool-val #t))))
        
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))
        
        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
                 (let ((v1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var (newref v1) env))))
        
        (proc-exp (var body)
                  (proc-val (procedure var body env)))
        
        (call-exp (rator rand)
                  (let ((proc (expval->proc (value-of rator env)))
                        (arg (value-of rand env)))
                    (apply-procedure proc arg)))
        
        (letrec-exp (p-names b-vars p-bodies letrec-body)
                    (value-of letrec-body
                              (extend-env-rec* p-names b-vars p-bodies env)))
        
        (begin-exp (exp1 exps)
                   (letrec 
                       ((value-of-begins
                         (lambda (e1 es)
                           (let ((v1 (value-of e1 env)))
                             (if (null? es)
                                 v1
                                 (value-of-begins (car es) (cdr es)))))))
                     (value-of-begins exp1 exps)))
        
        (assign-exp (var exp1)
                    (begin
                      (setref!
                       (apply-env env var)
                       (value-of exp1 env))
                      (num-val 27)))
        
        )))
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119
  
  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
                   (let ((r (newref arg)))
                     (let ((new-env (extend-env var r saved-env)))
                       (when (instrument-let)
                         (begin
                           (eopl:printf
                            "entering body of proc ~s with env =~%"
                            var)
                           (pretty-print (env->list new-env)) 
                           (eopl:printf "store =~%")
                           (pretty-print (store->readable (get-store-as-list)))
                           (eopl:printf "~%")))
                       (value-of body new-env)))))))  
  
  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
       (lambda (p)
         (list
          (car p)
          (expval->printable (cadr p))))
       l)))
  
  )





