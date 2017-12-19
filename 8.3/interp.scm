(module interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)
  (provide trace-apply-procedure)

  (define trace-apply-procedure (make-parameter #f))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (body)
                   (value-of/k body (init-env) (list
                                                (lambda (val) val)
                                                (lambda (val) (eopl:error 'apply-handler "uncaught exception!"))))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 173
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        
        (const-exp (num) (apply-cont cont (num-val num)))
        
        (const-list-exp (nums)
                        (apply-cont cont
                                    (list-val (map num-val nums))))
        
        (var-exp (var) (apply-cont cont (apply-env env var)))
        
        (diff-exp (exp1 exp2)
                  (value-of/k exp1 env
                              (cons (lambda (val1)
                                (value-of/k exp2 env
                                            (cons (lambda (val2)
                                              (apply-cont cont (num-val (- (expval->num val1) (expval->num val2)))))
                                                  (cdr cont))))
                                    (cdr cont))))
        
        (unop-exp (unop exp1)
                  (value-of/k exp1 env
                              (cons (lambda (val)
                                ((car cont) (apply-unop unop val))) (cdr cont))))
        
        (if-exp (exp1 exp2 exp3)
                (value-of/k exp1 env
                            (cons (lambda (val)
                              (if (expval->bool val)
                                  (value-of/k exp2 env cont)
                                  (value-of/k exp3 env cont)))
                                  (cdr cont))))
        
        (proc-exp (var body)
                  (apply-cont cont
                              (proc-val
                               (procedure var body env))))
        
        (call-exp (rator rand)
                  (value-of/k rator env
                              (cons
                               (lambda (val1)
                                (value-of/k rand env
                                            (cons (lambda (val2)
                                              (apply-procedure (expval->proc val1) val2 cont)) 
                                                  (cdr cont))))
                               (cdr cont))))
        
        ;; make let a macro, because I'm too lazy to add the extra
        ;; continuation
        (let-exp (var exp1 body)
                 (value-of/k
                  (call-exp (proc-exp var body) exp1)
                  env
                  cont))
        
        (letrec-exp (p-name b-var p-body letrec-body)
                    (value-of/k
                     letrec-body
                     (extend-env-rec p-name b-var p-body env)
                     cont))
        
        (try-exp (exp1 var handler-exp)
                 (value-of/k exp1 env
                  (list 
                   (lambda (val) 
                    (apply-cont cont val))
                   (lambda (val)
                     (value-of/k handler-exp (extend-env var val env)
                                 cont)))))
        
        (raise-exp (exp1)
                   (value-of/k exp1 env
                               (cons (lambda (val)
                                 (apply-handler cont val))
                                     (cdr cont)))))))
  
  (define apply-cont
    (lambda (cont val)
      ((car cont) val)))
  
  (define apply-handler
    (lambda (cont val)
      ((cadr cont) val))) 

  ;; apply-procedure : procedure * expval * cont -> final-expval

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))


  (define apply-unop
    (lambda (unop val)
      (cases unary-op unop
        (null?-unop ()
          (bool-val
            (null? (expval->list val))))
        (car-unop ()
          (car (expval->list val)))
        (cdr-unop ()
          (list-val (cdr (expval->list val))))
        (zero?-unop ()
          (bool-val
            (zero? (expval->num val)))))))


  ;; to get the detailed trace:
  ;; (trace value-of/k apply-cont apply-handler)
  
  (value-of-program (scan&parse "try -(1, raise 44) catch (m) m"))

  )
