(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)            ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (car (value-of exp1 (init-env) (initialize-store!)))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env the-store)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (list (num-val num) the-store))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (list (apply-env env var) the-store))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let* ((val1&s1 (value-of exp1 env the-store))
                (val2&s2 (value-of exp2 env (cadr val1&s1))))
            (let ((num1 (expval->num (car val1&s1)))
                  (num2 (expval->num (car val2&s2))))
              (list (num-val (- num1 num2)) (cadr val2&s2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1&s1 (value-of exp1 env the-store)))
            (let ((num1 (expval->num (car val1&s1))))
              (list 
               (if (zero? num1)
                (bool-val #t)
                (bool-val #f)) (cadr val1&s1)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1&s1 (value-of exp1 env the-store)))
            (if (expval->bool (car val1&s1))
              (value-of exp2 env (cadr val1&s1))
              (value-of exp3 env (cadr val1&s1)))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1&s1 (value-of exp1 env the-store)))
            (value-of body
              (extend-env var (car val1&s1) env) (cadr val1&s1))))
        
        (proc-exp (var body)
          (list (proc-val (procedure var body env)) the-store))

        (call-exp (rator rand)
          (let* ((val1&s1 (value-of rator env the-store))
                (val2&s2 (value-of rand env (cadr val1&s1))))
            (apply-procedure (expval->proc (car val1&s1)) (car val2&s2) (cadr val2&s2))))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env) the-store))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es inner-store)
                 (let ((val1&s1 (value-of e1 env inner-store)))
                   (if (null? es)
                     val1&s1
                     (value-of-begins (car es) (cdr es) (cadr val1&s1)))))))
            (value-of-begins exp1 exps the-store)))

        (newref-exp (exp1)
          (let* ((val1&s1 (value-of exp1 env the-store))
                (val2&s2 (newref (cadr val1&s1) (car val1&s1))))
            (list (ref-val (car val2&s2)) (cadr val2&s2))))

        (deref-exp (exp1)
                   (let ((val1&s1 (value-of exp1 env the-store)))
                     (list (deref (cadr val1&s1) (expval->ref (car val1&s1))) (cadr val1&s1))))

        (setref-exp (exp1 exp2)
          (let* ((val1&s1 (value-of exp1 env the-store))
            (val2&s2 (value-of exp2 env (cadr val1&s1))))
              (list (num-val 23) (setref! (cadr val2&s2) (expval->ref (car val1&s1)) (car val2&s2)))))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg the-store)
      (cases proc proc1
        (procedure (var body saved-env)
                   (let ((r arg))
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
                       (value-of body new-env the-store)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 (value-of-program  (a-program
 (let-exp
  'g
  (let-exp
   'counter
   (newref-exp (const-exp 0))
   (proc-exp 'dummy (let-exp 'd (setref-exp (var-exp 'counter) (diff-exp (deref-exp (var-exp 'counter)) (const-exp -1))) (deref-exp (var-exp 'counter)))))
  (diff-exp (call-exp (var-exp 'g) (const-exp 11)) (call-exp (var-exp 'g) (const-exp 22))))))
  
  )

  



  
